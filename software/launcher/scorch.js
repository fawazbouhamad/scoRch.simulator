// The SCORCH setup window in the browser: the event map with its controls,
// and the file dialog. R supplies every number; nothing comes from the internet.
(function () {
  'use strict';

  const SVG = 'http://www.w3.org/2000/svg';
  const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
    'October', 'November', 'December'];
  // Light yellow to deep red: hotter cells are darker.
  const HEAT = ['#ffe9a8', '#ffd166', '#ffb347', '#ff8c42', '#f4633a', '#d93a2b', '#a8201a', '#6d0f1f'];
  const calm = window.matchMedia ? window.matchMedia('(prefers-reduced-motion: reduce)') : { matches: false };

  // R sends a one-element vector as a single value.
  const list = value => (value == null ? [] : Array.isArray(value) ? value : [value]);
  const count = (n, word) => thousands(n) + ' ' + word + (n === 1 ? '' : 's');
  const thousands = n => String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ',');

  function build(namespace, tag, attributes, children) {
    const element = namespace ? document.createElementNS(namespace, tag) : document.createElement(tag);
    for (const key in attributes || {}) {
      const value = attributes[key];
      if (value == null || value === false) continue;
      if (key === 'text') element.textContent = value;
      else if (key.slice(0, 2) === 'on') element.addEventListener(key.slice(2), value);
      else element.setAttribute(key, value === true ? '' : value);
    }
    for (const child of children) if (child != null) element.append(child);
    return element;
  }
  const h = (tag, attributes, ...children) => build(null, tag, attributes, children);
  const s = (tag, attributes, ...children) => build(SVG, tag, attributes, children);

  const parts = iso => iso.split('-').map(Number);
  const longDate = iso => { const [y, m, d] = parts(iso); return d + ' ' + MONTHS[m - 1] + ' ' + y; };
  const shortDate = iso => { const [, m, d] = parts(iso); return d + ' ' + MONTHS[m - 1].slice(0, 3); };
  function addDays(iso, days) {
    const [y, m, d] = parts(iso);
    return new Date(Date.UTC(y, m - 1, d + days)).toISOString().slice(0, 10);
  }
  function dateRange(start, end) {
    if (start === end) return longDate(start);
    const [y1, m1, d1] = parts(start);
    const [y2, m2] = parts(end);
    if (y1 !== y2) return longDate(start) + ' to ' + longDate(end);
    return (m1 === m2 ? String(d1) : d1 + ' ' + MONTHS[m1 - 1]) + ' to ' + longDate(end);
  }

  // n colours spread over the heat ramp. Up to eight classes take the ramp's
  // own colours; more classes are blended between neighbouring colours, so
  // that no two classes share one.
  function heat(n) {
    if (n <= 1) return [HEAT[4]];
    const last = HEAT.length - 1;
    if (n <= HEAT.length) return Array.from({ length: n }, (_, i) => HEAT[Math.round(i * last / (n - 1))]);
    const rgb = HEAT.map(colour => [1, 3, 5].map(at => parseInt(colour.slice(at, at + 2), 16)));
    return Array.from({ length: n }, (_, i) => {
      const at = i * last / (n - 1);
      const k = Math.min(Math.floor(at), last - 1);
      return '#' + rgb[k].map((value, channel) => Math.round(value + (rgb[k + 1][channel] - value) * (at - k))
        .toString(16).padStart(2, '0')).join('');
    });
  }

  // One small picture per type: day tiles holding the clusters of that day.
  function glyph(type) {
    const days = [[1], [3], [1, 1, 1], [1, 2, 0]][type - 1];
    const picture = s('svg', { class: 'type-glyph', viewBox: '0 0 ' + (days.length * 26) + ' 24', 'aria-hidden': 'true' });
    days.forEach((clusters, day) => {
      const x = day * 26 + 1;
      picture.append(s('rect', { x, y: 1, width: 22, height: 22, rx: 3, class: 'tile' }));
      const spots = [[], [[11, 12, 8, 4.5, -20]], [[7.5, 8, 4.5, 2.6, -25], [14.5, 16, 4.5, 2.6, 20]],
        [[7, 7.5, 4, 2.4, -25], [15.5, 10, 3.6, 2.2, 30], [9.5, 17, 4, 2.3, 10]]][clusters];
      for (const [cx, cy, rx, ry, turn] of spots) {
        picture.append(s('ellipse', { cx: x + cx, cy: cy + 1, rx, ry, class: 'spot',
          transform: 'rotate(' + turn + ' ' + (x + cx) + ' ' + (cy + 1) + ')' }));
      }
    });
    return picture;
  }

  let serial = 0;

  class Explorer {
    constructor(root, request) {
      this.root = root;
      this.request = request;      // asks R for the days of an event
      this.frames = {};
      this.timer = null;
      this.day = 0;
      this.still = calm.matches;
      this.pointer = null;         // where the pointer rests on the map, if it does
    }

    load(data) {
      this.stop();
      this.data = data;
      this.frames = {};
      for (const id in data.frames || {}) this.frames[id] = list(data.frames[id]);
      this.events = list(data.events);
      this.types = list(data.types);
      this.ext = list(data.extent);
      // The picture is 720 units wide and at most 520 high, the height the window
      // gives it at that scale. A taller region is drawn narrower, not smaller, so
      // that its labels, ellipses and centroids keep their size.
      const across = this.ext[1] - this.ext[0];
      this.scale = Math.min(720 / across, 520 / (this.ext[3] - this.ext[2]));
      this.width = Math.round(across * this.scale * 10) / 10;
      this.height = (this.ext[3] - this.ext[2]) * this.scale;
      this.breaks = list(data.breaks);
      this.colours = heat(this.breaks.length - 1);
      this.render();
      const longest = this.events.slice().sort((a, b) => list(b.counts).length - list(a.counts).length || a.id - b.id)[0];
      if (longest) this.showEvent(longest.id);
    }

    x(lon) { return (lon - this.ext[0]) * this.scale; }
    y(lat) { return (this.ext[3] - lat) * this.scale; }
    line(flat, close) {
      let d = '';
      for (let i = 0; i + 1 < flat.length; i += 2) {
        d += (i ? 'L' : 'M') + this.x(flat[i]).toFixed(1) + ' ' + this.y(flat[i + 1]).toFixed(1);
      }
      return d && close ? d + 'Z' : d;
    }
    classOf(value) {
      let k = 0;
      while (k < this.colours.length - 1 && value >= this.breaks[k + 1]) k++;
      return k;
    }

    render() {
      const data = this.data;
      const demo = !!data.demonstration;
      this.root.textContent = '';
      this.root.classList.toggle('is-demo', demo);
      this.baseId = 'scorch-base-' + (++serial);

      this.typeCards = this.types.map(type => {
        const shown = this.events.filter(event => event.type === type.type).length;
        const card = h('button', { type: 'button', class: 'type-card', style: '--type:' + type.colour,
          disabled: shown === 0, 'aria-pressed': 'false',
          onclick: () => this.showEvent(this.events.find(event => event.type === type.type).id) },
        h('span', { class: 'type-top' },
          h('span', null, h('span', { class: 'type-n', text: 'Type ' + type.type }), h('span', { class: 'type-name', text: type.name })),
          glyph(type.type)),
        h('span', { class: 'type-meaning', text: type.meaning }),
        h('span', { class: 'type-total', text: (demo ? 'In the full example: ' : '') + count(type.events, 'event') +
            (type.events ? ' over ' + count(type.days, 'day') : '') }));
        return card;
      });

      this.picker = h('select', { class: 'form-control', 'aria-label': 'Event', onchange: () => this.showEvent(Number(this.picker.value)) });
      this.eventLine = h('p', { class: 'event-line' });
      this.rule = h('p', { class: 'rule' });

      this.map = s('svg', { class: 'map', viewBox: '0 0 ' + this.width + ' ' + this.height.toFixed(1), role: 'img' });
      this.drawBase();
      this.dateLabel = h('strong', { class: 'map-date' });
      this.dayLabel = h('span', { class: 'map-day' });
      this.tip = h('div', { class: 'map-tip', hidden: true });
      this.map.addEventListener('mousemove', event => { this.pointer = event; this.hover(event); });
      this.map.addEventListener('mouseleave', () => { this.pointer = null; this.tip.hidden = true; });

      const control = (label, action, text) => h('button', { type: 'button', class: 'btn btn-default', onclick: action,
        'aria-label': label, text });
      this.previous = control('Previous day', () => this.step(-1), '‹ Previous day');
      this.playButton = h('button', { type: 'button', class: 'btn btn-default play', onclick: () => this.toggle() });
      this.next = control('Next day', () => this.step(1), 'Next day ›');
      // Read the chosen day first: stop() puts the slider back on the day shown.
      this.slider = h('input', { type: 'range', min: 1, max: 1, step: 1, value: 1, 'aria-label': 'Day of the event',
        oninput: () => { const day = Number(this.slider.value) - 1; this.stop(); this.draw(day); } });
      this.stillButton = h('button', { type: 'button', class: 'btn btn-default still-toggle',
        onclick: () => this.setStill(!this.still), text: 'Still view' });
      this.strip = h('ol', { class: 'days' });
      this.facts = h('p', { class: 'frame-facts' });
      this.stills = h('div', { class: 'stills', hidden: true });
      this.live = h('p', { class: 'sr-only', 'aria-live': 'polite' });
      this.waiting = h('p', { class: 'fine', hidden: true, text: 'Loading this event…' });

      this.root.append(
        h('header', { class: 'explorer-head' },
          h('span', { class: 'badge-tag', text: demo ? 'Demonstration' : 'This run' }),
          h('h2', { text: demo ? 'How SCORCH reads a heatwave' : 'Events from this run' }),
          h('p', { class: 'explorer-source', text: data.source + (demo ? '. These events illustrate the method; they are not your data.' : '') })),
        h('div', { class: 'explorer-grid' },
          h('div', { class: 'stage' },
            h('div', { class: 'event-bar' }, h('div', { class: 'event-pick' }, this.picker), this.eventLine),
            h('div', { class: 'map-head' }, this.dateLabel, this.dayLabel),
            h('div', { class: 'map-wrap' }, this.map, this.tip),
            this.waiting,
            h('div', { class: 'controls', role: 'group', 'aria-label': 'Step through the days of this event' },
              this.previous, this.playButton, this.next, this.slider, this.stillButton),
            this.strip, this.rule, this.facts),
          h('div', { class: 'type-cards', role: 'group', 'aria-label': 'The four event types' }, ...this.typeCards)),
        this.stills, this.legend(), this.live);
    }

    drawBase() {
      const base = this.data.basemap || {};
      const all = (shapes, close) => list(shapes).map(shape => this.line(list(shape), close)).join('');
      const clip = this.baseId + '-clip';
      const lines = s('g', { class: 'graticule' });
      // The lines are spaced for the scale of the picture: by the degrees that 720 units
      // span, which a region drawn narrower than that does not fill.
      const span = Math.max(this.ext[1] - this.ext[0], (this.ext[3] - this.ext[2]) * 720 / 520);
      const step = [1, 2, 5, 10, 15, 20, 30, 45].find(value => span / value <= 7) || 60;
      for (let lon = Math.ceil(this.ext[0] / step) * step; lon < this.ext[1]; lon += step) {
        if (lon <= this.ext[0]) continue;
        // A longitude is printed as the run holds it, 255°E in a file that counts from 0 to 360:
        // the dataset card, Summary.txt and the catalogs name the same cell the same way.
        const label = Math.abs(lon) + '°' + (lon < 0 ? 'W' : lon > 0 ? 'E' : '');
        // A label that would run past the right edge stands to the left of its line.
        const tight = this.x(lon) + 3 + label.length * 7.6 > this.width;
        lines.append(s('path', { d: 'M' + this.x(lon).toFixed(1) + ' 0V' + this.height.toFixed(1) }),
          s('text', tight ? { x: this.x(lon) - 3, y: this.height - 4, 'text-anchor': 'end', text: label }
                          : { x: this.x(lon) + 3, y: this.height - 4, text: label }));
      }
      for (let lat = Math.ceil(this.ext[2] / step) * step; lat < this.ext[3]; lat += step) {
        if (lat <= this.ext[2]) continue;
        lines.append(s('path', { d: 'M0 ' + this.y(lat).toFixed(1) + 'H' + this.width }),
          s('text', { x: 4, y: this.y(lat) - 3, text: Math.abs(lat) + '°' + (lat < 0 ? 'S' : lat > 0 ? 'N' : '') }));
      }
      this.layer = s('g', { class: 'frame' });
      this.map.append(
        s('defs', null,
          s('clipPath', { id: clip }, s('rect', { width: this.width, height: this.height.toFixed(1) })),
          s('g', { id: this.baseId },
            s('rect', { class: 'map-sea', width: this.width, height: this.height.toFixed(1) }),
            s('path', { class: 'map-land', d: all(base.land, true) }),
            s('path', { class: 'map-lake', d: all(base.lakes, true) }),
            s('path', { class: 'map-state', d: all(base.states, false) }))),
        s('g', { 'clip-path': 'url(#' + clip + ')' }, s('use', { href: '#' + this.baseId }), lines, this.layer),
        s('rect', { class: 'map-edge', width: this.width, height: this.height.toFixed(1) }));
    }

    legend() {
      const swatch = (cls, text, colour) => h('li', null, h('span', { class: 'swatch ' + cls, style: colour ? 'background:' + colour : null }), h('span', { text }));
      const classes = h('ol', { class: 'heat-scale', 'aria-label': 'Colours of clustered heatwave cells by daily maximum temperature' });
      this.colours.forEach((colour, i) => classes.append(h('li', { style: 'background:' + colour },
        h('span', { text: this.breaks[i] + (i === this.colours.length - 1 ? '–' + this.breaks[i + 1] + ' °C' : '') }))));
      const terms = this.data.terms || {};
      return h('div', { class: 'legend' },
        h('div', { class: 'legend-heat' },
          h('span', { class: 'legend-title', text: 'Heatwave cells in a cluster, by daily maximum temperature (°C)' }), classes),
        h('ul', { class: 'legend-marks' },
          swatch('outside', 'Heatwave cell outside every cluster'),
          swatch('edge', 'Edge of the heatwave cells'),
          swatch('ring', 'Ellipse fitted to a cluster'),
          swatch('axes', 'Major and minor axes'),
          swatch('dot', 'Temperature-weighted centroid')),
        h('details', { class: 'terms' }, h('summary', { text: 'What the map shows' }),
          h('ul', null, ...['cell', 'day', 'cluster', 'ellipse', 'event'].filter(key => terms[key]).map(key => h('li', { text: terms[key] })))));
    }

    // The cells and ellipses of one day. Cells of one colour share one path.
    dayGroup(frame, small) {
      const grid = this.data.grid;
      const width = grid.lon_step * this.scale;
      const height = grid.lat_step * this.scale;
      const across = width.toFixed(2);
      const down = height.toFixed(2);
      const paths = this.colours.map(() => '');
      let outside = '';
      let edge = '';
      const cells = list(frame.cell);
      const clusters = list(frame.cluster);
      const tmax = list(frame.tmax);
      const held = new Set(cells);
      for (let k = 0; k < cells.length; k++) {
        const column = cells[k] % grid.columns;
        const row = Math.floor(cells[k] / grid.columns);
        const left = this.x(grid.lon + (column - 0.5) * grid.lon_step).toFixed(1);
        const top = this.y(grid.lat + (row + 0.5) * grid.lat_step).toFixed(1);
        const rect = 'M' + left + ' ' + top + 'h' + across + 'v' + down + 'h-' + across + 'Z';
        if (clusters[k] < 0) outside += rect; else paths[this.classOf(tmax[k])] += rect;
        // Sides that no other heatwave cell of this day shares.
        if (!held.has(cells[k] + grid.columns)) edge += 'M' + left + ' ' + top + 'h' + across;
        if (!held.has(cells[k] - grid.columns)) edge += 'M' + left + ' ' + (Number(top) + height).toFixed(2) + 'h' + across;
        if (column === 0 || !held.has(cells[k] - 1)) edge += 'M' + left + ' ' + top + 'v' + down;
        if (column === grid.columns - 1 || !held.has(cells[k] + 1)) {
          edge += 'M' + (Number(left) + width).toFixed(2) + ' ' + top + 'v' + down;
        }
      }
      const group = s('g', { class: 'day' + (small ? ' small' : '') });
      // The white line between cells keeps its share of the cell on fine grids
      // (0.55 of a ten-unit cell at one degree); a fixed width would wash the
      // colours out of quarter-degree cells.
      const line = 'stroke-width:' + Math.min(0.55, Math.min(width, height) / 18).toFixed(3);
      if (outside) group.append(s('path', { class: 'cell outside', d: outside, style: line }));
      paths.forEach((d, k) => { if (d) group.append(s('path', { class: 'cell', d, fill: this.colours[k], style: line })); });
      // One outline around all heatwave cells of the day. The coolest classes
      // are almost the colour of the land; the outline shows where they end.
      if (edge) group.append(s('path', { class: 'footprint', d: edge }));
      // A small picture of a region drawn narrower than 720 units shows fewer units in
      // the same room: its ellipse lines, 11 and 5 units in style.css, are thinned to
      // the weight they have in a picture of full width.
      const thin = small && this.width < 720 ? this.width / 720 : 0;
      for (const ellipse of list(frame.ellipses)) {
        const outline = this.line(list(ellipse.outline), true);
        group.append(s('path', { class: 'ellipse-halo', d: outline, style: thin ? 'stroke-width:' + (11 * thin).toFixed(2) : null }),
          s('path', { class: 'ellipse', d: outline, style: thin ? 'stroke-width:' + (5 * thin).toFixed(2) : null }));
        if (small) continue;
        const centre = list(ellipse.centroid);
        group.append(
          s('path', { class: 'axis major', d: this.line(list(ellipse.major)) }),
          s('path', { class: 'axis minor', d: this.line(list(ellipse.minor)) }),
          s('circle', { class: 'centroid', cx: this.x(centre[0]).toFixed(1), cy: this.y(centre[1]).toFixed(1), r: 3.6 },
            s('title', { text: 'Ellipse of ' + count(ellipse.cells, 'cell') + ': ' + thousands(ellipse.major_km) + ' by ' +
              thousands(ellipse.minor_km) + ' km, ' + thousands(ellipse.area_km2) + ' km²' })));
      }
      return group;
    }

    days() { return this.event ? this.frames[this.event.id] || [] : []; }

    showEvent(id) {
      this.stop();
      const event = this.events.find(candidate => candidate.id === id);
      if (!event) return;
      this.event = event;
      const type = this.types.find(candidate => candidate.type === event.type);
      this.typeCards.forEach((card, k) => card.setAttribute('aria-pressed', String(this.types[k].type === event.type)));
      const peers = this.events.filter(candidate => candidate.type === event.type);
      this.picker.textContent = '';
      for (const peer of peers) {
        this.picker.append(h('option', { value: peer.id, selected: peer.id === id,
          text: 'Event ' + peer.id + ': ' + dateRange(peer.start, peer.end) }));
      }
      this.picker.value = String(id);
      this.picker.parentNode.hidden = peers.length < 2;
      const counts = list(event.counts);
      this.eventLine.textContent = 'Event ' + event.id + ', ' + dateRange(event.start, event.end) + ': ' +
        count(counts.length, 'day') + ', ' + count(event.ellipses, 'ellipse') + '.';
      this.rule.textContent = '';
      this.rule.append(h('span', { text: event.reason + ' → ' }),
        h('strong', { class: 'type-tag', style: '--type:' + type.colour, text: 'Type ' + type.type + ': ' + type.name }));
      this.strip.textContent = '';
      this.strip.setAttribute('aria-label', 'Clusters on each day of event ' + event.id);
      counts.forEach((clusters, day) => {
        const date = addDays(event.start, day);
        this.strip.append(h('li', null, h('button', { type: 'button',
          class: 'day-chip ' + (clusters === 0 ? 'none' : clusters === 1 ? 'one' : 'many'),
          'aria-label': longDate(date) + ': ' + count(clusters, 'cluster'),
          onclick: () => { this.stop(); this.draw(day); } },
        h('span', { class: 'day-date', text: shortDate(date) }), h('strong', { text: String(clusters) }),
        h('span', { class: 'day-word', text: clusters === 1 ? 'cluster' : 'clusters' }))));
      });
      const ready = !!this.frames[id];
      this.waiting.hidden = ready;
      if (!ready) {
        // Nothing is drawn until the days arrive, so nothing is described either.
        this.layer.textContent = '';
        this.shown = null;
        this.tip.hidden = true;
        if (this.request) this.request(id);
        this.sync();
        return;
      }
      this.fillStills();
      this.draw(0, true);
    }

    receive(id, frames) {
      this.frames[id] = list(frames);
      if (this.event && this.event.id === id) this.showEvent(id);
    }

    draw(index, fresh) {
      const days = this.days();
      const frame = days[index];
      if (!frame) return;
      this.day = index;
      const group = this.dayGroup(frame, false);
      const earlier = Array.from(this.layer.children);
      this.layer.append(group);
      if (fresh || this.still || calm.matches || !earlier.length) {
        earlier.forEach(old => old.remove());
      } else {
        const last = earlier.pop();
        earlier.forEach(old => old.remove());
        last.classList.add('leaving');
        group.classList.add('entering');
        window.setTimeout(() => last.remove(), 300);
      }
      const cells = list(frame.cell);
      const clusters = list(frame.cluster);
      const tmax = list(frame.tmax);
      this.lookup = new Map(cells.map((cell, k) => [cell, k]));
      this.shown = frame;
      // The tooltip follows the day: it describes the cell under a resting
      // pointer on the day now shown, or goes.
      if (this.pointer) this.hover(this.pointer); else this.tip.hidden = true;
      const formed = list(frame.ellipses).length;
      const outside = clusters.filter(label => label < 0).length;
      const hottest = tmax.length ? Math.max(...tmax) : NaN;
      const coolest = tmax.length ? Math.min(...tmax) : NaN;
      const summary = [
        count(cells.length, 'heatwave cell') + ' of ' + thousands(this.data.domain_cells) +
          ' (a regionally extensive day needs ' + thousands(this.data.threshold_cells) + ')',
        formed === 0 ? 'no cluster formed' : formed === 1 ? '1 cluster with its ellipse' : formed + ' clusters, each with an ellipse',
        outside ? count(outside, 'cell') + ' outside every cluster' : null,
        tmax.length ? 'daily maximum ' + coolest.toFixed(1) + ' to ' + hottest.toFixed(1) + ' °C' : null
      ].filter(Boolean).join(' · ');
      this.dateLabel.textContent = longDate(frame.date);
      this.dayLabel.textContent = days.length === 1 ? 'Single-day event' : 'Day ' + (index + 1) + ' of ' + days.length;
      this.facts.textContent = summary;
      this.map.setAttribute('aria-label', 'Map for ' + longDate(frame.date) + ': ' + summary);
      if (!this.timer) this.live.textContent = longDate(frame.date) + '. ' + summary;
      this.sync();
    }

    // Controls, the day strip and the still pictures follow the current day.
    sync() {
      const total = this.days().length;
      const single = total < 2;
      this.slider.max = Math.max(total, 1);
      this.slider.value = this.day + 1;
      this.slider.hidden = this.previous.hidden = this.next.hidden = single;
      // A button that disables itself on the first or last day would drop the
      // keyboard to the top of the page: the other button takes it over.
      const held = document.activeElement;
      this.previous.disabled = this.day <= 0;
      this.next.disabled = this.day >= total - 1;
      if (held === this.next && this.next.disabled && !this.previous.disabled) this.previous.focus();
      if (held === this.previous && this.previous.disabled && !this.next.disabled) this.next.focus();
      this.playButton.hidden = single || this.still;
      this.playButton.textContent = this.timer ? '❚❚ Pause' : this.day >= total - 1 && total > 1 ? '▶ Play again' : '▶ Play';
      this.playButton.setAttribute('aria-label', this.timer ? 'Pause' : 'Play the days of this event');
      this.stillButton.hidden = single;
      this.stillButton.setAttribute('aria-pressed', String(this.still));
      this.stills.hidden = single || !this.still;
      Array.from(this.strip.children).forEach((item, day) => {
        if (day === this.day) item.firstChild.setAttribute('aria-current', 'true'); else item.firstChild.removeAttribute('aria-current');
      });
      Array.from(this.stills.children).forEach((item, day) => item.classList.toggle('is-current', day === this.day));
    }

    step(by) {
      this.stop();
      this.draw(Math.max(0, Math.min(this.days().length - 1, this.day + by)));
    }
    toggle() { if (this.timer) this.stop(); else this.play(); }
    play() {
      const total = this.days().length;
      if (total < 2) return;
      if (this.day >= total - 1) this.draw(0);
      this.timer = window.setInterval(() => {
        if (this.day >= total - 1) this.stop(); else this.draw(this.day + 1);
      }, 1100);
      this.sync();
    }
    stop() {
      if (this.timer) window.clearInterval(this.timer);
      this.timer = null;
      if (this.playButton) this.sync();
    }

    // The still view: every day side by side, with no motion.
    setStill(on) {
      this.stop();
      this.still = on;
      this.fillStills();
      this.sync();
    }
    fillStills() {
      this.stills.textContent = '';
      const days = this.days();
      if (!this.still || days.length < 2) return;
      const counts = list(this.event.counts);
      days.forEach((frame, day) => {
        const picture = s('svg', { class: 'map', viewBox: '0 0 ' + this.width + ' ' + this.height.toFixed(1), 'aria-hidden': 'true' },
          s('use', { href: '#' + this.baseId }), this.dayGroup(frame, true),
          s('rect', { class: 'map-edge', width: this.width, height: this.height.toFixed(1) }));
        this.stills.append(h('button', { type: 'button', class: 'still',
          'aria-label': longDate(frame.date) + ': ' + count(counts[day], 'cluster'), onclick: () => this.draw(day) },
        picture, h('span', { text: shortDate(frame.date) + ' · ' + count(counts[day], 'cluster') })));
      });
    }

    hover(event) {
      const frame = this.shown;
      const box = this.map.getBoundingClientRect();
      const view = this.map.getScreenCTM();
      if (!frame || !box.width || !view) return;
      const grid = this.data.grid;
      // A region taller than wide is drawn narrower than its element and
      // centred in it, so the picture itself says where the pointer is.
      const at = this.map.createSVGPoint();
      at.x = event.clientX;
      at.y = event.clientY;
      const here = at.matrixTransform(view.inverse());
      const lon = this.ext[0] + here.x / this.scale;
      const lat = this.ext[3] - here.y / this.scale;
      const column = Math.floor((lon - grid.lon) / grid.lon_step + 0.5);
      const row = Math.floor((lat - grid.lat) / grid.lat_step + 0.5);
      const k = column < 0 || column >= grid.columns ? undefined : this.lookup.get(column + grid.columns * row);
      if (k === undefined) { this.tip.hidden = true; return; }
      // A step such as 0.1 degrees is not exact in binary: print 35.3, not 35.300000000000004.
      const tidy = value => Math.round(value * 1e4) / 1e4;
      // The longitude as the run holds it, like the labels of the map.
      const east = tidy(grid.lon + column * grid.lon_step);
      const north = tidy(grid.lat + row * grid.lat_step);
      // The cluster number is the one in the catalogs (dbscan_label), which count from 0.
      const label = list(frame.cluster)[k];
      this.tip.textContent = list(frame.tmax)[k].toFixed(1) + ' °C · ' + Math.abs(north) + '°' + (north < 0 ? 'S' : 'N') + ' ' +
        Math.abs(east) + '°' + (east < 0 ? 'W' : 'E') + ' · ' + (label < 0 ? 'outside every cluster' : 'cluster ' + label);
      this.tip.hidden = false;
      const wrap = this.map.parentNode.getBoundingClientRect();
      this.tip.style.left = Math.min(event.clientX - wrap.left + 12, wrap.width - this.tip.offsetWidth - 4) + 'px';
      this.tip.style.top = Math.max(event.clientY - wrap.top - 30, 2) + 'px';
    }
  }

  // The demonstration is part of the package; the events of a run come from R.
  const explorers = {};
  function mountDemonstration() {
    const root = document.getElementById('explorer-demo');
    const main = document.querySelector('main.scorch');
    if (!root || !main || !main.dataset.showcase) return;
    fetch(main.dataset.showcase).then(response => {
      if (!response.ok) throw new Error(String(response.status));
      return response.json();
    }).then(data => {
      explorers.demo = new Explorer(root);
      explorers.demo.load(data);
    }).catch(() => {
      root.textContent = '';
      root.append(h('p', { class: 'fine', text: 'The demonstration map could not be loaded. The analysis is not affected.' }));
    });
  }
  function mountRun(data, attempt) {
    const root = document.getElementById('explorer-run');
    if (!root) {
      if ((attempt || 0) < 60) window.setTimeout(() => mountRun(data, (attempt || 0) + 1), 50);
      return;
    }
    explorers.run = new Explorer(root, id => window.Shiny.setInputValue('explorer_event', { id, at: Date.now() }, { priority: 'event' }));
    explorers.run.load(data);
  }

  // The file dialog. R lists the folders; this part selects and opens rows.
  function act(what, path) {
    if (!window.Shiny) return;
    window.Shiny.setInputValue('chooser_action', { do: what, path: path == null ? null : String(path), at: Date.now() },
      { priority: 'event' });
  }
  const picked = () => document.querySelector('.chooser-row[aria-selected="true"]');
  // A long path shows its end, where the name of the folder or file is.
  function showEnd(field) {
    if (field) field.scrollLeft = field.scrollWidth;
  }
  function pick(row) {
    const rows = row.closest('.chooser-list');
    rows.querySelectorAll('[aria-selected="true"]').forEach(other => other.setAttribute('aria-selected', 'false'));
    row.setAttribute('aria-selected', 'true');
    rows.setAttribute('aria-activedescendant', row.id);
    const field = document.getElementById('chooser_path');
    if (field) field.value = row.dataset.path;
    showEnd(field);
    row.scrollIntoView({ block: 'nearest' });
  }
  function naming(show) {
    const row = document.querySelector('.chooser-new');
    if (!row) return;
    row.hidden = !show;
    if (!show) return;
    // What is highlighted or written in the path field when "Use this folder"
    // is clicked must have been chosen after the name: both start afresh.
    const open = document.querySelector('.chooser'), field = document.getElementById('chooser_path');
    document.querySelectorAll('.chooser-row[aria-selected="true"]').forEach(other => other.setAttribute('aria-selected', 'false'));
    const rows = document.getElementById('chooser_list');
    if (rows) rows.removeAttribute('aria-activedescendant');
    if (open && field) { field.value = open.dataset.folder; showEnd(field); }
    document.getElementById('chooser_name').focus();
  }
  document.addEventListener('click', event => {
    if (!event.target.closest('.chooser')) return;
    const place = event.target.closest('[data-go]');
    if (place) return act('go', place.dataset.go);
    const row = event.target.closest('.chooser-row');
    if (row) return pick(row);
    const button = event.target.closest('[data-do]');
    if (!button || button.disabled) return;
    const what = button.dataset.do;
    if (what === 'name') naming(true);
    else if (what === 'unname') naming(false);
    else if (what === 'mkdir') act('mkdir', document.getElementById('chooser_name').value);
    else if (what === 'go') act('go', document.getElementById('chooser_path').value);
    // "Use this folder" means the folder named in the path field: the open
    // folder, a highlighted row, or a path typed or pasted there. A name still
    // waiting in the New folder row is not dropped: that folder is made and
    // opened first, as Create would do, and the next click uses it. A folder
    // that was highlighted or typed after the name is the choice, not the name.
    else if (what === 'use') {
      const row = document.querySelector('.chooser-new');
      const name = row && !row.hidden ? document.getElementById('chooser_name').value.trim() : '';
      const field = document.getElementById('chooser_path').value.trim();
      const open = document.querySelector('.chooser');
      if (name && !picked() && open && field === open.dataset.folder.trim()) act('mkdir', name); else act('use', field);
    } else act(what);
  });
  document.addEventListener('dblclick', event => {
    const row = event.target.closest('.chooser-row');
    if (row) act('go', row.dataset.path);
  });
  document.addEventListener('keydown', event => {
    const target = event.target;
    if (target.id === 'chooser_path' && event.key === 'Enter') { event.preventDefault(); return act('go', target.value); }
    if (target.id === 'chooser_name' && event.key === 'Enter') { event.preventDefault(); return act('mkdir', target.value); }
    if (!target.classList || !target.classList.contains('chooser-list')) return;
    // Backspace goes up from every folder, also from one that lists nothing.
    if (event.key === 'Backspace') { event.preventDefault(); return act('up'); }
    const rows = Array.from(target.querySelectorAll('.chooser-row'));
    if (!rows.length) return;
    const at = rows.indexOf(picked());
    const to = { ArrowDown: at + 1, ArrowUp: at - 1, Home: 0, End: rows.length - 1 }[event.key];
    if (to !== undefined) {
      event.preventDefault();
      pick(rows[Math.max(0, Math.min(rows.length - 1, to))]);
    } else if (event.key === 'Enter' && at >= 0) {
      event.preventDefault();
      act('go', rows[at].dataset.path);
    } else if (event.key.length === 1 && !event.ctrlKey && !event.metaKey && !event.altKey) {
      const letter = event.key.toLowerCase();
      const next = rows.slice(at + 1).concat(rows.slice(0, at + 1))
        .find(row => row.querySelector('.chooser-name').textContent.toLowerCase().startsWith(letter));
      if (next) pick(next);
    }
  });

  if (window.Shiny) {
    window.Shiny.addCustomMessageHandler('scorch-events', data => mountRun(data));
    window.Shiny.addCustomMessageHandler('scorch-frames', message => {
      if (explorers.run) explorers.run.receive(Number(message.id), message.frames);
    });
    window.Shiny.addCustomMessageHandler('scorch-busy', busy => {
      document.body.classList.toggle('scorch-busy', !!busy);
      const main = document.querySelector('main.scorch');
      if (main) main.setAttribute('aria-busy', String(!!busy));
    });
  }
  if (window.jQuery) {
    // Keep the keyboard in the list after each folder is opened, unless the
    // person is already typing a path or a folder name.
    window.jQuery(document).on('shiny:value', event => {
      if (event.name !== 'chooser') return;
      window.setTimeout(() => {
        const rows = document.getElementById('chooser_list');
        const typing = document.activeElement && document.activeElement.closest('.chooser') &&
          document.activeElement.tagName === 'INPUT';
        if (rows && !typing) rows.focus();
        showEnd(document.getElementById('chooser_path'));
      }, 0);
    });
    // A refused advanced setting is reported above the Advanced section, in
    // words that quote the label of its field. Mark the fields so named, and
    // open the section when a new report arrives, so that the field is in view.
    let reported = '';
    window.jQuery(document).on('shiny:value', event => {
      if (event.name !== 'advanced_problem') return;
      const section = document.querySelector('details.advanced');
      if (!section) return;
      const html = event.value && typeof event.value.html === 'string' ? event.value.html : '';
      const said = new DOMParser().parseFromString(html, 'text/html').body.textContent.trim();
      section.querySelectorAll('[aria-invalid]').forEach(field => field.removeAttribute('aria-invalid'));
      section.querySelectorAll('label[for]').forEach(label => {
        const field = document.getElementById(label.getAttribute('for'));
        if (!said || !field || !said.includes("'" + label.textContent.trim() + "'")) return;
        // A group of checkboxes is marked box by box.
        const fields = field.matches('input, select, textarea') ? [field] : field.querySelectorAll('input');
        fields.forEach(one => one.setAttribute('aria-invalid', 'true'));
      });
      if (said && said !== reported) section.open = true;
      reported = said;
    });
    // A result has replaced the settings, whether the run finished or stopped:
    // the keyboard and a screen reader continue from its heading, not from the
    // top of the page. The heading is on the page a moment after its value arrives.
    window.jQuery(document).on('shiny:value', event => {
      if (event.name !== 'completion' || !event.value) return;
      const wait = attempt => {
        const title = document.getElementById('results-title');
        if (title && title.offsetParent !== null) title.focus();
        else if (attempt < 60) window.setTimeout(() => wait(attempt + 1), 50);
      };
      window.setTimeout(() => wait(0), 0);
    });
    // Bootstrap puts the keyboard on the dialog itself when it has faded in,
    // after the list above was focused. Hand it on to the list, where the
    // arrow keys choose a row, or back to the field the person was typing in.
    let taken = null;
    document.addEventListener('focusin', event => {
      if (event.target.id === 'shiny-modal') taken = event.relatedTarget;
    });
    window.jQuery(document).on('shown.bs.modal', '#shiny-modal', () => {
      const rows = document.getElementById('chooser_list');
      const typing = taken && taken.tagName === 'INPUT' && taken.isConnected && taken.closest('.chooser');
      if (typing) taken.focus(); else if (rows) rows.focus();
      taken = null;
    });
    // When the dialog has gone, the keyboard returns to the button that opened
    // it, if that button is still on the screen, and not to the top of the page.
    let opener = null;
    document.addEventListener('click', event => {
      const button = event.target.closest && event.target.closest('#browse_file, #load_config, #browse_output');
      if (button) opener = button;
    }, true);
    window.jQuery(document).on('hidden.bs.modal', '#shiny-modal', () => {
      const button = opener;
      opener = null;
      window.setTimeout(() => {
        if (button && button.isConnected && button.getClientRects().length && !document.getElementById('shiny-modal')) button.focus();
      }, 0);
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', mountDemonstration);
  else mountDemonstration();
})();
