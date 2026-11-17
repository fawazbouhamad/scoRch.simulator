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
  // Type 4 mixes a one-cluster day, a several-cluster day and a last day with one large ellipse.
  function glyph(type) {
    const days = [[1], [3], [1, 1, 1], [1, 2, 1]][type - 1];
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

  // The four type symbols of Figure 4 of the study, redrawn from its slide (figure_4.pptx): each
  // panel is 240 points wide, and positions, sizes and colours are those of the slide's shapes.
  // Labels and dots are drawn in the colour of the text, so that they read in both displays.
  const FIG4 = { red: '#ff0000', orange: '#ff7401', gold: '#ffbc01', pale: '#ffce43', purple: '#7030a0' };
  function typeSymbol(type) {
    const picture = s('svg', { class: 'type-symbol', viewBox: '0 180 240 262', role: 'img',
      'aria-label': 'Type ' + type + ' symbol' });
    const dot = (cx, cy, r, fill) => s('circle', { cx, cy, r, fill });
    // A right arrow of the slide (26 by 11 points), turned about its centre.
    const arrow = (x, y, turn, fill) => s('polygon', { fill,
      points: [[0, 2.75], [20.5, 2.75], [20.5, 0], [26, 5.5], [20.5, 11], [20.5, 8.25], [0, 8.25]]
        .map(([px, py]) => (x + px) + ',' + (y + py)).join(' '),
      transform: turn ? 'rotate(' + turn + ' ' + (x + 13) + ' ' + (y + 5.5) + ')' : null });
    const label = (x, y, ...parts) => s('text', { x, y, class: 'symbol-label', 'text-anchor': 'middle' }, ...parts);
    // (x1, y1): italic letters with lowered digits, as in the slide's equation text.
    const point = n => [s('tspan', { text: '(' }), s('tspan', { 'font-style': 'italic', text: 'x' }),
      s('tspan', { class: 'symbol-sub', dy: 3, text: n }), s('tspan', { dy: -3, text: ', ' }),
      s('tspan', { 'font-style': 'italic', text: 'y' }), s('tspan', { class: 'symbol-sub', dy: 3, text: n }),
      s('tspan', { dy: -3, text: ')' })];
    if (type === 1) picture.append(dot(120, 287, 85, FIG4.red));
    if (type === 2) {
      picture.append(dot(46.5, 328.5, 42.5, FIG4.orange), dot(151.5, 256.5, 42.5, FIG4.orange),
        label(94, 378, ...point('1')), label(194, 311, ...point('2')));
    }
    if (type === 3) {
      [[208.5, 202, FIG4.gold], [303.5, 298, FIG4.pale], [382.5, 377, FIG4.pale]].forEach(([cy, top, fill]) => {
        [42.5, 117.5, 191.5].forEach(cx => picture.append(dot(cx, cy, 21.5, fill)));
        [67, 141].forEach(x => picture.append(arrow(x, top, 0, FIG4.gold)));
      });
      [42, 117, 191].forEach(cx => [336, 344, 353].forEach(cy => picture.append(dot(cx, cy, 3, 'currentColor'))));
      [249, 428].forEach(y => ['t', 't + 1', 't + 2'].forEach((text, k) =>
        picture.append(label([42.5, 116.5, 191.5][k], y, text))));
    }
    if (type === 4) {
      [[62.5, 236.5], [177.5, 236.5], [120.5, 294.5], [62.5, 352.5], [177.5, 352.5]]
        .forEach(([cx, cy]) => picture.append(dot(cx, cy, 21.5, FIG4.purple)));
      picture.append(arrow(78, 260, 45.37, FIG4.purple), arrow(78, 318, -45.37, FIG4.purple),
        arrow(136, 318, 45.37, FIG4.purple), arrow(136, 260, -45.37, FIG4.purple));
    }
    return picture;
  }

  // The five statistics of the event panel: columns of the compound event catalog (Catalog 5),
  // shown as R sent them. A value that is missing or not a number is shown as a dash.
  const EVENT_STATS = [
    ['duration_days', 'Total Duration (days)', 0],
    ['total_clusters', 'Total Heatwaves', 0],
    ['mean_axis_ratio', 'Average Heatwave ratio (L2/L1)', 3],
    ['mean_area_km2', 'Average Heatwave Area (km²)', 1],
    ['axial_mean_azimuth_deg', 'Average Heatwave orientation (°)', 2]];
  function statValue(value, decimals) {
    if (typeof value !== 'number' || !isFinite(value)) return '–';
    return value.toLocaleString('en-US', { minimumFractionDigits: decimals, maximumFractionDigits: decimals });
  }

  // Is the point (x, y) inside the closed outline given as flat x, y pairs? (Even-odd rule.)
  function holds(flat, x, y) {
    let inside = false;
    for (let i = 0, j = flat.length - 2; i + 1 < flat.length; j = i, i += 2) {
      const xi = flat[i], yi = flat[i + 1], xj = flat[j], yj = flat[j + 1];
      if ((yi > y) !== (yj > y) && x < (xj - xi) * (y - yi) / (yj - yi) + xi) inside = !inside;
    }
    return inside;
  }
  // Is the point within `reach` of the outline itself, where its drawn line lies?
  function touches(flat, x, y, reach) {
    for (let i = 2; i + 1 < flat.length; i += 2) {
      const ax = flat[i - 2], ay = flat[i - 1], dx = flat[i] - ax, dy = flat[i + 1] - ay;
      const t = Math.max(0, Math.min(1, ((x - ax) * dx + (y - ay) * dy) / (dx * dx + dy * dy || 1)));
      if (Math.hypot(x - ax - t * dx, y - ay - t * dy) <= reach) return true;
    }
    return false;
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
      // The map shows the analyzed region with a margin of geographical context around it
      // (`view`, from R); outside the region there are no cells. Older data without a view
      // show the region itself.
      this.ext = list(data.view != null ? data.view : data.extent);
      // The picture is 720 units wide and at most 520 high, the height the window
      // gives it at that scale. A taller region is drawn narrower, not smaller, so
      // that its labels, ellipses and centroids keep their size. The frame of the map
      // leaves room on the left for latitudes and below for longitudes.
      const pad = { left: 40, right: 10, top: 6, bottom: 20 };
      const across = this.ext[1] - this.ext[0];
      const down = this.ext[3] - this.ext[2];
      this.scale = Math.min((720 - pad.left - pad.right) / across, (520 - pad.top - pad.bottom) / down);
      this.box = { x: pad.left, y: pad.top, w: Math.round(across * this.scale * 10) / 10, h: Math.round(down * this.scale * 10) / 10 };
      this.width = Math.round((pad.left + this.box.w + pad.right) * 10) / 10;
      this.height = pad.top + this.box.h + pad.bottom;
      this.breaks = list(data.breaks);
      this.colours = heat(this.breaks.length - 1);
      this.render();
      const longest = this.events.slice().sort((a, b) => list(b.counts).length - list(a.counts).length || a.id - b.id)[0];
      if (longest) this.showEvent(longest.id);
    }

    x(lon) { return this.box.x + (lon - this.ext[0]) * this.scale; }
    y(lat) { return this.box.y + (this.ext[3] - lat) * this.scale; }
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
      this.demo = demo;
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
        // The completed run keeps its own counts under each definition; the demonstration shows the definitions only.
        demo ? null : h('span', { class: 'type-total', text: count(type.events, 'event') +
            (type.events ? ' over ' + count(type.days, 'day') : '') }));
        return card;
      });

      this.picker = h('select', { class: 'form-control', 'aria-label': 'Event', onchange: () => this.showEvent(Number(this.picker.value)) });
      this.eventLine = h('p', { class: 'event-line' });
      // The selected event: its type, the type's symbol from Figure 4, and its values in Catalog 5.
      this.panel = h('section', { class: 'event-panel', 'aria-label': 'Selected event' });

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
      this.stills = h('div', { class: 'stills', hidden: true });
      this.live = h('p', { class: 'sr-only', 'aria-live': 'polite' });
      this.waiting = h('p', { class: 'fine', hidden: true, text: 'Loading this event…' });

      this.root.append(
        h('header', { class: 'explorer-head' },
          h('span', { class: 'badge-tag', text: demo ? 'Demonstration' : 'This run' }),
          // The results page names no heading or file here: the dropdown identifies each event.
          demo ? h('h2', { text: 'How SCORCH works' }) : null,
          demo ? h('p', { class: 'explorer-source' }, 'Explore the ', h('strong', { text: 'space-time typologies' }),
                   ' of compound heatwaves using 30 years of temperature data.') : null),
        h('div', { class: 'explorer-grid' },
          h('div', { class: 'stage' },
            h('div', { class: 'event-bar' }, h('div', { class: 'event-pick' }, this.picker), demo ? this.eventLine : null),
            h('div', { class: 'map-head' }, this.dateLabel, this.dayLabel),
            h('div', { class: 'map-wrap' }, this.map, this.tip),
            this.waiting,
            h('div', { class: 'controls', role: 'group', 'aria-label': 'Step through the days of this event' },
              this.previous, this.playButton, this.next, this.slider, this.stillButton),
            this.strip, this.panel),
          h('div', { class: 'type-cards', role: 'group', 'aria-label': 'The four event types' }, ...this.typeCards)),
        this.stills, this.legend(), this.live);
    }

    drawBase() {
      const base = this.data.basemap || {};
      const all = (shapes, close) => list(shapes).map(shape => this.line(list(shape), close)).join('');
      const clip = this.baseId + '-clip';
      const frame = { x: this.box.x, y: this.box.y, width: this.box.w, height: this.box.h };
      const lines = s('g', { class: 'graticule' });
      // Latitudes stand in the margin left of the frame and longitudes below it, clear of the cells.
      const labels = s('g', { class: 'graticule axis-labels' });
      // The lines are spaced for the scale of the picture: by the degrees that the frame
      // spans, about seven lines across, so that the labels below it have room.
      const span = Math.max(this.ext[1] - this.ext[0], (this.ext[3] - this.ext[2]) * this.box.w / Math.max(this.box.h, 1));
      const step = [1, 2, 5, 10, 15, 20, 30, 45].find(value => span / value <= 7) || 60;
      const right = this.box.x + this.box.w;
      const bottom = this.box.y + this.box.h;
      for (let lon = Math.ceil(this.ext[0] / step) * step; lon < this.ext[1]; lon += step) {
        if (lon <= this.ext[0]) continue;
        // A longitude is printed as the run holds it, 255°E in a file that counts from 0 to 360:
        // the dataset card, Summary.txt and the catalogs name the same cell the same way.
        const label = Math.abs(lon) + '°' + (lon < 0 ? 'W' : lon > 0 ? 'E' : '');
        // Centred under its line, and kept inside the picture at either end.
        const half = label.length * 3.4;
        const at = Math.min(Math.max(this.x(lon), this.box.x + half), right - half);
        lines.append(s('path', { d: 'M' + this.x(lon).toFixed(1) + ' ' + this.box.y + 'V' + bottom.toFixed(1) }));
        labels.append(s('text', { x: at.toFixed(1), y: (bottom + 14).toFixed(1), 'text-anchor': 'middle', text: label }));
      }
      for (let lat = Math.ceil(this.ext[2] / step) * step; lat < this.ext[3]; lat += step) {
        if (lat <= this.ext[2]) continue;
        lines.append(s('path', { d: 'M' + this.box.x + ' ' + this.y(lat).toFixed(1) + 'H' + right.toFixed(1) }));
        labels.append(s('text', { x: this.box.x - 5, y: (this.y(lat) + 3.5).toFixed(1), 'text-anchor': 'end',
          text: Math.abs(lat) + '°' + (lat < 0 ? 'S' : lat > 0 ? 'N' : '') }));
      }
      this.layer = s('g', { class: 'frame' });
      this.map.append(
        s('defs', null,
          s('clipPath', { id: clip }, s('rect', frame)),
          s('g', { id: this.baseId },
            s('rect', Object.assign({ class: 'map-sea' }, frame)),
            s('path', { class: 'map-land', d: all(base.land, true) }),
            s('path', { class: 'map-lake', d: all(base.lakes, true) }),
            s('path', { class: 'map-state', d: all(base.states, false) }))),
        s('g', { 'clip-path': 'url(#' + clip + ')' }, s('use', { href: '#' + this.baseId }), lines, this.layer),
        s('rect', Object.assign({ class: 'map-edge' }, frame)), labels);
    }

    legend() {
      // The short label is what the key shows; the pointer and a screen reader also get what it stands for.
      const swatch = (cls, text, meaning) => h('li', { title: meaning },
        h('span', { class: 'swatch ' + cls }), h('span', { text }), h('span', { class: 'sr-only', text: ': ' + meaning }));
      const classes = h('ol', { class: 'heat-scale', 'aria-label': 'Colours of clustered heatwave cells by daily maximum temperature' });
      this.colours.forEach((colour, i) => classes.append(h('li', { style: 'background:' + colour },
        h('span', { text: this.breaks[i] + (i === this.colours.length - 1 ? '–' + this.breaks[i + 1] + ' °C' : '') }))));
      return h('div', { class: 'legend' },
        h('div', { class: 'legend-heat' },
          h('span', { class: 'legend-title', text: 'Daily maximum temperature (°C)' }), classes),
        h('ul', { class: 'legend-marks' },
          // A clustered cell as the map draws it: a heat-coloured square inside the outline of the day's heatwave cells.
          swatch('cluster', 'Heatwave Cluster', 'Heatwave cell in a cluster, coloured by its daily maximum temperature'),
          swatch('outside', 'Heatwave Noise', 'Heatwave cell outside every cluster'),
          swatch('ring', 'Ellipse', 'Ellipse fitted to a cluster, with the snapshot mesh inside'),
          swatch('axis-major', 'Major axis (L1)', 'Major axis of the ellipse, L1'),
          swatch('axis-minor', 'Minor axis (L2)', 'Minor axis of the ellipse, L2'),
          swatch('dot', 'Centroid', 'Temperature-weighted centroid')));
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
          this.mesh(ellipse, outline, width, height),
          s('path', { class: 'ellipse', d: outline, style: thin ? 'stroke-width:' + (5 * thin).toFixed(2) : null }));
        if (small) continue;
        const centre = list(ellipse.centroid);
        group.append(
          s('path', { class: 'axis major', d: this.line(list(ellipse.major)) }),
          s('path', { class: 'axis minor', d: this.line(list(ellipse.minor)) }),
          s('circle', { class: 'centroid', cx: this.x(centre[0]).toFixed(1), cy: this.y(centre[1]).toFixed(1), r: 3.6 }));
      }
      return group;
    }

    // The mesh of the exported event snapshots (draw_ellipse_mesh() in R, snapshot_maps.py of the
    // study): lines every quarter of a grid cell and stronger lines on the cell borders, on the
    // grid's own longitudes and latitudes, clipped to the fitted ellipse. The fine lines are
    // 4/55.897 of a cell wide, as in the snapshots.
    mesh(ellipse, outline, cellWidth, cellHeight) {
      const grid = this.data.grid;
      const flat = list(ellipse.outline);
      const lons = flat.filter((_, i) => i % 2 === 0);
      const lats = flat.filter((_, i) => i % 2 === 1);
      const id = this.baseId + '-mesh-' + (++serial);
      let fine = '';
      let borders = '';
      // The snapshots divide each cell in four. On the window's smaller map the lines keep at
      // least 5 units apart, or they would merge into a tint over the cells: a one-degree cell
      // of 10 units is divided in two, a smaller cell less or not at all.
      const cell = Math.min(cellWidth, cellHeight);
      const parts = Math.max(1, Math.min(4, Math.floor(cell / 5)));
      const lines = (origin, step, low, high, draw) => {
        const spacing = step / parts;
        for (let k = Math.ceil((low - origin) / spacing); k <= Math.floor((high - origin) / spacing); k++) {
          const d = draw(origin + k * spacing);
          // As in the snapshots, the fine mesh runs on every line, cell borders included; the borders
          // add a hairline of black on top.
          fine += d;
          if (k % parts === 0) borders += d;
        }
      };
      const west = Math.min(...lons), east = Math.max(...lons), south = Math.min(...lats), north = Math.max(...lats);
      lines(grid.lon - grid.lon_step / 2, grid.lon_step, west, east,
        lon => 'M' + this.x(lon).toFixed(2) + ' ' + this.y(north).toFixed(1) + 'V' + this.y(south).toFixed(1));
      lines(grid.lat - grid.lat_step / 2, grid.lat_step, south, north,
        lat => 'M' + this.x(west).toFixed(1) + ' ' + this.y(lat).toFixed(2) + 'H' + this.x(east).toFixed(1));
      return s('g', { class: 'mesh' },
        s('clipPath', { id }, s('path', { d: outline })),
        s('g', { 'clip-path': 'url(#' + id + ')' },
          s('path', { class: 'mesh-fine', d: fine, style: 'stroke-width:' + (cell * 4 / 55.897).toFixed(3) }),
          // In the snapshots the borders are 0.24 points against fine lines of 4 of 55.897 pixels a cell:
          // about a sixteenth of their width.
          s('path', { class: 'mesh-cell', d: borders, style: 'stroke-width:' + (cell * 0.25 / 55.897).toFixed(3) })));
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
      // On the results page the dropdown stays, even with a single event of the type.
      this.picker.parentNode.hidden = this.demo && peers.length < 2;
      const counts = list(event.counts);
      // The number of heatwaves is the catalog's total_clusters: the clusters of all its days added up.
      this.eventLine.textContent = 'Event ' + event.id + ', ' + dateRange(event.start, event.end) + ': ' +
        count(counts.length, 'day') + ', ' + count(event.ellipses, 'heatwave');
      this.fillPanel(event, type);
      // Every tile of the calendar takes the colour of the event's type, whatever the count of one
      // day. The ellipses on the map keep the black style of the study's snapshot figures.
      this.root.style.setProperty('--event-type', type.colour);
      this.strip.textContent = '';
      this.strip.setAttribute('aria-label', 'Heatwaves on each day of event ' + event.id);
      counts.forEach((clusters, day) => {
        const date = addDays(event.start, day);
        this.strip.append(h('li', null, h('button', { type: 'button',
          class: 'day-chip ' + (clusters === 0 ? 'none' : clusters === 1 ? 'one' : 'many'),
          'aria-label': longDate(date) + ': ' + count(clusters, 'heatwave'),
          onclick: () => { this.stop(); this.draw(day); } },
        h('span', { class: 'day-date', text: shortDate(date) }), h('strong', { text: String(clusters) }),
        h('span', { class: 'day-word', text: clusters === 1 ? 'Heatwave' : 'Heatwaves' }))));
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

    // The event panel describes the whole event, so it changes with the event and not with the day.
    fillPanel(event, type) {
      const stats = event.stats || {};
      this.panel.textContent = '';
      this.panel.style.setProperty('--type', type.colour);
      this.panel.append(
        h('h3', { class: 'type-tag', text: 'Type ' + type.type + ': ' + type.name }),
        h('div', { class: 'event-panel-body' },
          h('div', { class: 'event-symbol' }, typeSymbol(type.type)),
          h('table', { class: 'event-stats' },
            h('caption', { class: 'sr-only', text: 'Event ' + event.id + ' in the compound event catalog' }),
            h('tbody', null, ...EVENT_STATS.map(([key, label, decimals]) =>
              h('tr', { 'data-stat': key }, h('th', { scope: 'row', text: label }),
                h('td', { text: statValue(stats[key], decimals) })))))));
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
      // Not shown on the page: what a screen reader says about the map of this day.
      const summary = [
        formed === 0 ? 'no heatwave cluster' : count(formed, 'heatwave') + (formed === 1 ? ' with its ellipse' : ', each with its ellipse'),
        count(cells.length, 'heatwave cell') + ' of ' + thousands(this.data.domain_cells),
        outside ? count(outside, 'cell') + ' of Heatwave Noise' : null,
        tmax.length ? 'daily maximum ' + coolest.toFixed(1) + ' to ' + hottest.toFixed(1) + ' °C' : null
      ].filter(Boolean).join(' · ');
      this.dateLabel.textContent = longDate(frame.date);
      this.dayLabel.textContent = days.length === 1 ? 'Single-day event' : 'Day ' + (index + 1) + ' of ' + days.length;
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
        // A still shows the map's frame only, without the margins for the coordinates.
        const box = this.box;
        const picture = s('svg', { class: 'map', viewBox: [box.x, box.y, box.w, box.h].join(' '), 'aria-hidden': 'true' },
          s('use', { href: '#' + this.baseId }), this.dayGroup(frame, true),
          s('rect', { class: 'map-edge', x: box.x, y: box.y, width: box.w, height: box.h }));
        this.stills.append(h('button', { type: 'button', class: 'still',
          'aria-label': longDate(frame.date) + ': ' + count(counts[day], 'heatwave'), onclick: () => this.draw(day) },
        picture, h('span', { text: shortDate(frame.date) + ' · ' + count(counts[day], 'heatwave') })));
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
      // Only the frame of the map answers; its margins hold the coordinates.
      if (here.x < this.box.x || here.x > this.box.x + this.box.w || here.y < this.box.y || here.y > this.box.y + this.box.h) {
        this.tip.hidden = true;
        return;
      }
      const lon = this.ext[0] + (here.x - this.box.x) / this.scale;
      const lat = this.ext[3] - (here.y - this.box.y) / this.scale;
      const ellipses = list(frame.ellipses);
      // The heatwave of the day under the pointer: an ellipse whose outline holds the point. Where
      // ellipses overlap, the smallest is named, the one the pointer is most clearly inside.
      // Its drawn line counts as part of it, as far as its white halo reaches (2.5 units from the centre of the line).
      const reach = 2.5 / this.scale;
      const inside = ellipses.filter(ellipse => holds(list(ellipse.outline), lon, lat) || touches(list(ellipse.outline), lon, lat, reach))
        .sort((a, b) => (a.stats || {}).area_km2 - (b.stats || {}).area_km2);
      this.tip.textContent = '';
      if (inside.length) {
        const ellipse = inside[0];
        const stats = ellipse.stats || {};
        this.tip.append(h('strong', { text: 'Heatwave ' + ellipse.number }),
          h('span', { text: 'Axis ratio (L2/L1): ' + statValue(stats.axis_ratio, 3) }),
          h('span', { text: 'Area (km²): ' + statValue(stats.area_km2, 1) }),
          h('span', { text: 'Orientation (°): ' + statValue(stats.azimuth_north_cw_deg, 2) }));
      } else {
        // Only an ellipse answers the pointer: cells, clusters and noise have no tooltip of their own.
        this.tip.hidden = true;
        return;
      }
      this.tip.hidden = false;
      const wrap = this.map.parentNode.getBoundingClientRect();
      this.tip.style.left = Math.min(event.clientX - wrap.left + 12, wrap.width - this.tip.offsetWidth - 4) + 'px';
      this.tip.style.top = Math.max(event.clientY - wrap.top - 30, 2) + 'px';
    }
  }

  // The running view: the whole page while a run is under way, driven only by the run's own messages
  // (preparation, the four stages, saving). Its picture illustrates the method; it shows no result.
  const runView = (() => {
    let timer = null, started = 0, finished = false, pending = false;
    const view = () => document.getElementById('running');
    function drawArt() {
      const box = view().querySelector('.running-art');
      if (!box || box.firstChild) return;
      const cols = 18, rows = 9, size = 20;
      const svg = s('svg', { viewBox: '0 0 ' + cols * size + ' ' + rows * size, class: 'running-grid' });
      // Two patches of heat: each becomes a cluster with its fitted ellipse, and together one event.
      const blobs = [[5, 4, 3.4, 2.3, -12], [12.5, 4.5, 2.7, 2.1, 18]];
      for (let r = 0; r < rows; r++) for (let c = 0; c < cols; c++) {
        let heat = 0;
        blobs.forEach(([x, y, a, b]) => { heat = Math.max(heat, 1 - (((c - x) / a) ** 2 + ((r - y) / b) ** 2)); });
        const level = heat > 0 ? Math.min(7, Math.floor(heat * 8)) : -1;
        svg.append(s('rect', { x: c * size + 1.5, y: r * size + 1.5, width: size - 3, height: size - 3, rx: 2.5,
          class: 'run-cell' + (level >= 0 ? ' hot' : ''),
          style: (level >= 0 ? 'fill:' + HEAT[level] + ';' : '') + '--delay:' + ((c + r) * 45) + 'ms' }));
      }
      blobs.forEach(([x, y, a, b, turn]) => {
        const cx = (x + 0.5) * size, cy = (y + 0.5) * size;
        svg.append(s('ellipse', { class: 'run-ellipse', cx, cy, rx: (a + 0.2) * size, ry: (b + 0.2) * size, pathLength: 100,
          transform: 'rotate(' + turn + ' ' + cx + ' ' + cy + ')' }),
          s('circle', { class: 'run-centroid', cx, cy, r: 3.5 }));
      });
      svg.append(s('path', { class: 'run-link', pathLength: 100,
        d: 'M' + 5.5 * size + ' ' + 4.5 * size + 'L' + 13 * size + ' ' + 5 * size }));
      box.append(svg);
    }
    function tick() {
      const seconds = Math.floor((Date.now() - started) / 1000);
      view().querySelector('.running-elapsed').textContent = Math.floor(seconds / 60) + ':' + String(seconds % 60).padStart(2, '0');
    }
    // 0: reading the data, 1 to 4: the stages, 5: saving the results.
    function phase(p) {
      const n = p === 'saving' ? 5 : p === 'prep' ? 0 : Number(p);
      const v = view();
      v.dataset.stage = String(n);
      // The phases shown during this run, in order (0 reading, 1-4 stages, 5 saving).
      v.dataset.seen = (v.dataset.seen || '') + n;
      v.querySelectorAll('.running-stages li').forEach(item => {
        const k = Number(item.dataset.stage);
        item.classList.toggle('done', k < n);
        item.classList.toggle('active', k === n);
        if (k === n) item.setAttribute('aria-current', 'step'); else item.removeAttribute('aria-current');
      });
      v.querySelector('.running-note').textContent = n === 0 ? 'Reading the selected data' : n === 5 ? 'Saving the results' : '';
    }
    function close() {
      clearInterval(timer);
      timer = null;
      pending = false;
      document.body.classList.remove('scorch-running');
      view().hidden = true;
    }
    return {
      start() {
        drawArt();
        finished = false;
        const v = view();
        v.hidden = false;
        v.querySelector('.running-error').hidden = true;
        v.dataset.seen = '';
        document.body.classList.add('scorch-running');
        window.scrollTo(0, 0);
        started = Date.now();
        tick();
        clearInterval(timer);
        timer = setInterval(tick, 1000);
        phase('prep');
      },
      stage: phase,
      // The results are saved; the view goes when they are on the page (or after a moment at most).
      done() {
        finished = true;
        clearInterval(timer);
        setTimeout(() => { if (finished && !view().hidden) close(); }, 4000);
      },
      shown() { if (finished) close(); },
      failed(message) {
        clearInterval(timer);
        pending = false;
        const v = view();
        v.dataset.stage = 'failed';
        v.querySelector('.running-error-text').textContent = 'The analysis stopped: ' + message;
        v.querySelector('.running-error').hidden = false;
        v.querySelector('.running-back').focus();
      },
      back: close,
      // A second click on Run analysis while a run starts or runs is not passed on.
      claim() {
        if (pending || document.body.classList.contains('scorch-running')) return false;
        pending = true;
        // A run that R refused to start (a setting it cannot use) sends nothing: the guard lets go.
        setTimeout(() => { if (!document.body.classList.contains('scorch-running')) pending = false; }, 3000);
        return true;
      },
      release() { pending = false; }
    };
  })();
  document.addEventListener('click', event => {
    if (event.target.closest && event.target.closest('#start') && !runView.claim()) {
      event.preventDefault();
      event.stopImmediatePropagation();
    }
    if (event.target.closest && event.target.closest('.running-back')) runView.back();
  }, true);

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
    window.Shiny.addCustomMessageHandler('scorch-run', message => {
      if (message.state === 'start') runView.start();
      else if (message.state === 'stage') runView.stage(message.phase);
      else if (message.state === 'done') runView.done();
      else if (message.state === 'failed') runView.failed(message.message);
    });
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
      runView.shown();
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
  // SCORCH has one, light display. A dark choice saved by an earlier version is cleared.
  function start() {
    try { window.localStorage.removeItem('scorch-theme'); } catch (e) { /* nothing was saved */ }
    mountDemonstration();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
