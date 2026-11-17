// Drive the installed SCORCH window in a real browser, as a first-time user would.
// The window must already be open, from an R session whose working folder is
// SCORCH_UI_PROJECT. The test reads:
//   SCORCH_UI_URL       the address R gave the window's viewer, with its ?token=...
//                       (the address alone answers "403" and opens nothing)
//   SCORCH_UI_PROJECT   the working folder of that R session, new for every run of this
//                       test; it holds temperatures.zip (the bundled example in a ZIP)
//   SCORCH_UI_DATA      the bundled example as a prepared NetCDF file
//   SCORCH_UI_FIXTURES  optional: the folder written by fixtures.R (a file that holds
//                       July and August only, a daily minimum, undeclared missing values,
//                       numbers without units, raw ERA5 points on one row of cells)
//   SCORCH_UI_SHOTS     optional: a folder for screenshots
//   CHROME_BIN          the browser to drive
const { chromium } = require("playwright-core");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

async function run() {
  const address = process.env.SCORCH_UI_URL;
  assert.ok(address && /[?&]token=[^&]+/.test(address),
            "SCORCH_UI_URL must be the address R gave the window's viewer, including its ?token=");
  // R shows folders with links and short Windows names resolved; resolve them the same way here.
  const real = file => fs.realpathSync.native(file);
  const dir = real(process.env.SCORCH_UI_PROJECT);
  const nc = real(process.env.SCORCH_UI_DATA);
  const fixtures = process.env.SCORCH_UI_FIXTURES ? real(process.env.SCORCH_UI_FIXTURES) : null;
  for (const name of fixtures ? ["july_august.nc", "daily_minimum.nc", "undeclared_missing.nc", "no_units_66.nc",
                                 "era5_one_row.nc", "single_precision.nc"] : []) {
    assert.ok(fs.existsSync(path.join(fixtures, name)), name + " is not in SCORCH_UI_FIXTURES; fixtures.R writes it");
  }
  const browser = await chromium.launch({ executablePath: process.env.CHROME_BIN, headless: true });
  let page;
  try {
    // Start as someone who asked their system for reduced motion.
    page = await browser.newPage({ viewport: { width: 1060, height: 860 }, reducedMotion: "reduce" });
    const errors = [];
    page.on("pageerror", error => errors.push(String(error)));
    page.setDefaultTimeout(30000);
    const button = name => page.getByRole("button", { name, exact: true });
    const shots = process.env.SCORCH_UI_SHOTS;
    const shot = async (name, options = {}) => {
      if (shots) await page.screenshot({ path: path.join(shots, name + ".png"), ...options });
    };
    const output = path.join(dir, "reviewer results");
    const chooser = page.locator(".chooser");
    const hero = page.locator(".hero");
    const selection = page.locator("#selection");
    const connected = () => page.evaluate(() => window.Shiny.shinyapp.isConnected());
    // What R sends arrives a moment after a click: wait for it instead of reading at once.
    const shows = (where, text) => where.filter({ hasText: text }).first().waitFor();
    const until = (check, given) => page.waitForFunction(check, given);
    // Type a folder into the chooser's path field, as one would paste it.
    async function goTo(folder, expected) {
      await chooser.locator("#chooser_path").fill(folder);
      await chooser.locator("#chooser_path").press("Enter");
      await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: path.basename(folder) }).waitFor();
      if (expected) await chooser.locator(".chooser-row").filter({ hasText: expected }).first().waitFor();
    }
    // Open a file by its path: in the dialog that is open, or from the opening screen.
    async function openData(file, dialogIsOpen = false) {
      if (!dialogIsOpen) {
        await button("Select Your Data").click();
        await chooser.waitFor();
      }
      await chooser.locator("#chooser_path").fill(file);
      await chooser.locator("#chooser_path").press("Enter");
      await shows(selection, path.basename(file));
    }
    // Close the dialog with its own button; the page behind it is as it was.
    async function cancelDialog() {
      await chooser.locator(".chooser-actions").getByRole("button", { name: "Cancel", exact: true }).click();
      await page.locator("#shiny-modal").waitFor({ state: "detached" });
    }
    async function analysisDates(first, last) {
      for (const [index, date] of [first, last].entries()) {
        const field = page.locator("#dates input").nth(index);
        await field.fill(date.slice(0, -1));
        await field.press(date.slice(-1));
        await field.press("Tab");
      }
    }

    await page.goto(address);
    await until(() => window.Shiny?.shinyapp?.isConnected());

    // 1. The opening screen: the name spelled out, two choices, nothing running.
    await hero.waitFor();
    assert.equal(await page.evaluate(() => document.documentElement.lang), "en");
    assert.match(await page.locator(".masthead").innerText(), /Spatiotemporal Classification Of Regional Compound Heatwaves/);
    assert.deepEqual(await hero.getByRole("button").allInnerTexts(), ["Select Your Data", "Use included example"]);
    assert.match(await button("Select Your Data").getAttribute("class"), /btn-primary/);
    assert.doesNotMatch(await button("Use included example").getAttribute("class"), /btn-primary/);
    assert.equal(await page.locator("#start:visible").count(), 0);
    // One light display: no switch, and neither a dark system appearance nor a dark choice
    // saved by an earlier version changes it.
    assert.equal(await page.locator("#theme-toggle").count(), 0);
    assert.equal(await page.getByRole("button", { name: /dark mode/i }).count(), 0);
    await page.emulateMedia({ colorScheme: "dark" });
    await page.evaluate(() => localStorage.setItem("scorch-theme", "dark"));
    await page.reload();
    await until(() => window.Shiny?.shinyapp?.isConnected());
    await hero.waitFor();
    assert.equal(await page.evaluate(() => document.documentElement.getAttribute("data-theme")), null);
    assert.equal(await page.evaluate(() => getComputedStyle(document.body).backgroundColor), "rgb(246, 244, 238)");
    assert.equal(await page.evaluate(() => localStorage.getItem("scorch-theme")), null);
    await page.emulateMedia({ colorScheme: "light" });
    assert.equal(fs.existsSync(path.join(dir, "output")), false);
    assert.equal(fs.existsSync(output), false, "SCORCH_UI_PROJECT must be a folder that no earlier run of this test has used");
    // Without its key the address is no window: one plain sentence and status 403.
    const origin = new URL(address).origin;
    for (const keyless of [origin + "/", origin + "/?token=not-the-key"]) {
      const refused = await page.request.get(keyless);
      assert.equal(refused.status(), 403);
      assert.match(await refused.text(), /scoRch\.simulator::run_scorch\(\)/);
    }

    // 2. The demonstration map: labelled as bundled, four types, still by default under reduced motion.
    const demo = page.locator("#explorer-demo");
    await demo.locator("svg.map").first().waitFor();
    assert.match(await demo.locator(".explorer-head").innerText(), /demonstration/i);
    assert.match(await demo.locator(".explorer-head").innerText(), /How SCORCH works.*space-time typologies/s);
    assert.equal(await demo.locator(".type-card").count(), 4);
    for (const name of ["Widespread (Isolated)", "Spatially Clustered", "Temporally Clustered", "Compound Clustering (Multi-Type)"]) {
      assert.match(await demo.locator(".type-cards").innerText(), new RegExp(name.replace(/[()]/g, "\\$&")));
    }
    assert.match(await demo.locator(".legend-title").innerText(), /^Daily maximum temperature \(°C\)$/);
    // The key starts with the clustered cells, then the noise, and holds no "What the map shows" section.
    assert.deepEqual((await demo.locator(".legend-marks li > span:nth-child(2)").allTextContents()).slice(0, 2), ["Heatwave Cluster", "Heatwave Noise"]);
    assert.equal(await demo.locator(".terms").count(), 0);
    assert.equal(await demo.locator(".map-day").innerText(), "Day 1 of 7");
    // The demonstration is drawn 720 units wide, and its longitudes read as the example holds them.
    // The frame keeps a margin of context around the region, and room for the coordinates outside it.
    assert.equal(await demo.locator(".map-wrap svg.map").getAttribute("viewBox"), "0 0 720 293.1");
    assert.deepEqual(await demo.locator(".map-wrap .graticule text").allTextContents(),
                     ["120°W", "105°W", "90°W", "75°W", "60°W", "30°N", "45°N"]);
    assert.equal(await demo.locator(".stills .still").count(), 7);
    assert.equal(await demo.locator(".play").isVisible(), false);
    await demo.locator(".stills .still").nth(2).click();
    assert.equal(await demo.locator(".map-day").innerText(), "Day 3 of 7");
    await shot("still-view", { fullPage: true });

    // With motion allowed again the days can be played, paused and stepped through.
    await page.emulateMedia({ reducedMotion: "no-preference" });
    await demo.locator(".still-toggle").click();
    assert.equal(await demo.locator(".stills").isVisible(), false);
    await demo.locator(".day-chip").first().click();
    const firstDay = await demo.locator(".map-date").innerText();
    // The picture is taken when the change of day has finished, not halfway through its fade.
    await until(() => !document.querySelector("#explorer-demo .map .day.leaving"));
    await page.waitForTimeout(350);
    await shot("opening-screen", { fullPage: true });
    await demo.locator(".play").click();
    await until(day => document.querySelector("#explorer-demo .map-date").textContent !== day, firstDay);
    assert.match(await demo.locator(".play").innerText(), /Pause/);
    await demo.locator(".play").click();
    assert.match(await demo.locator(".play").innerText(), /Play/);
    const paused = await demo.locator(".map-day").innerText();
    await page.waitForTimeout(1500);
    assert.equal(await demo.locator(".map-day").innerText(), paused);
    await demo.locator(".day-chip").nth(4).click();
    await demo.getByRole("button", { name: "Next day" }).click();
    assert.equal(await demo.locator(".map-day").innerText(), "Day 6 of 7");
    await demo.getByRole("button", { name: "Previous day" }).click();
    assert.equal(await demo.locator(".map-day").innerText(), "Day 5 of 7");
    // The slider moves the map; it used to snap back to the day already shown.
    await demo.getByRole("slider", { name: "Day of the event" }).fill("4");
    assert.equal(await demo.locator(".map-day").innerText(), "Day 4 of 7");
    await demo.getByRole("slider", { name: "Day of the event" }).fill("5");
    assert.equal(await demo.locator(".map-day").innerText(), "Day 5 of 7");
    // "Next day" disables itself on the last day: the keyboard goes on to "Previous day", not to the page.
    await demo.getByRole("button", { name: "Next day" }).focus();
    await page.keyboard.press("Enter");
    await page.keyboard.press("Enter");
    assert.equal(await demo.locator(".map-day").innerText(), "Day 7 of 7");
    assert.equal(await page.evaluate(() => document.activeElement.getAttribute("aria-label")), "Previous day");
    await page.keyboard.press("Enter");
    await page.keyboard.press("Enter");
    assert.equal(await demo.locator(".map-day").innerText(), "Day 5 of 7");
    assert.equal(await demo.locator(".event-panel h3").innerText(), "Type 3: Temporally Clustered");
    assert.doesNotMatch(await demo.innerText(), /consecutive regionally extensive days|a regionally extensive day needs/);
    assert.equal(await demo.locator(".event-line").innerText(), "Event 7, 28 June to 4 July 2012: 7 days, 7 heatwaves");
    assert.ok(await demo.locator(".map .cell").count() > 0);
    assert.equal(await demo.locator(".map .frame .day:not(.leaving) .ellipse").count(), 1);
    // One outline surrounds the heatwave cells of a day, so the palest classes do not vanish into the land.
    assert.equal(await demo.locator(".map .frame .day:not(.leaving) .footprint").count(), 1);
    // The white line between cells is given as a share of the cell (a cell is 8.6 units in the padded view),
    // so that it does not wash the colours out of the small cells of a fine grid.
    assert.equal(await demo.locator(".map .frame .day:not(.leaving) .cell").first().getAttribute("style"), "stroke-width:0.479");
    // A single-day event is one frame without playback.
    await demo.locator(".type-card").first().click();
    assert.equal(await demo.locator(".map-day").innerText(), "Single-day event");
    assert.equal(await demo.locator(".play").isVisible(), false);
    assert.equal(await demo.getByRole("button", { name: "Next day" }).isVisible(), false);
    assert.equal(await demo.locator(".event-panel h3").innerText(), "Type 1: Widespread (Isolated)");
    assert.equal(await demo.locator(".day-chip").first().getAttribute("aria-label"), "20 July 2011: 1 heatwave");
    // Only an ellipse has a tooltip: its heatwave, numbered from 1 on the day, with its values in Catalog 4.
    // Where on the screen a point of the map is, and how wide the map is drawn inside its element.
    const onScreen = (x, y) => demo.locator(".map-wrap svg.map").evaluate((svg, [x, y]) => {
      svg.scrollIntoView({ block: "center" });
      const at = svg.createSVGPoint();
      at.x = x; at.y = y;
      const on = at.matrixTransform(svg.getScreenCTM());
      return { x: on.x, y: on.y, drawn: 720 * svg.getScreenCTM().a, element: svg.getBoundingClientRect().width };
    }, [x, y]);
    const cell = await onScreen(267.3, 53.4);   // 47.5°N 106.5°W: a cell of the heatwave, just outside its ellipse
    await page.mouse.move(cell.x, cell.y);
    await page.waitForTimeout(200);
    assert.equal(await demo.locator(".map-tip").isVisible(), false, "a cell outside the ellipse has no tooltip");
    const centre = await onScreen(418.7, 102.3);   // the centroid of the ellipse of 20 July 2011
    await page.mouse.move(centre.x, centre.y);
    await shows(demo.locator(".map-tip"), "Axis ratio");
    assert.equal(await demo.locator(".map-tip").innerText(),
                 "Heatwave 1\nAxis ratio (L2/L1): 0.611\nArea (km²): 1,679,118.4\nOrientation (°): -88.27");
    await page.mouse.move(5, 5);
    // A region taller than wide is drawn narrower than its element, with blank bands beside it.
    // Squeezed into that shape, this map must still name the ellipse under the pointer.
    const squeeze = await page.addStyleTag({ content: "#explorer-demo .map-wrap svg.map { max-height: 150px; }" });
    const banded = await onScreen(418.7, 102.3);
    assert.ok(banded.element - banded.drawn > 100, "the squeezed map is drawn narrower than its element");
    await page.mouse.move(banded.x, banded.y);
    await shows(demo.locator(".map-tip"), "Axis ratio");
    assert.match(await demo.locator(".map-tip").innerText(), /^Heatwave 1\n/);
    await page.mouse.move(5, 5);
    await squeeze.evaluate(style => style.remove());
    await demo.locator(".type-card").nth(3).click();
    assert.equal(await demo.locator(".event-panel h3").innerText(), "Type 4: Compound Clustering (Multi-Type)");
    // The panel shows the five values of Catalog 5 for the whole event.
    assert.deepEqual(await demo.locator(".event-stats td").allInnerTexts(), ["6", "18", "0.489", "746,185.6", "77.50"]);

    // 3. The window belongs to the page that holds its key. Another client gets no session, and
    // its leaving does not close the window; a reload keeps the window too.
    const turnedAway = await page.evaluate(() => new Promise(resolve => {
      const socket = new WebSocket(location.origin.replace(/^http/, "ws") + "/websocket/");
      // Turned away means that the socket did open and that R then closed it: a socket that
      // never opened also ends with a close, and proves nothing about the window.
      let opened = false;
      socket.onopen = () => {
        opened = true;
        socket.send(JSON.stringify({ method: "init", data: { ".clientdata_url_search": "" } }));
      };
      socket.onclose = () => resolve(opened);
      setTimeout(() => resolve(false), 5000);
    }));
    assert.equal(turnedAway, true);
    await page.waitForTimeout(2500);
    assert.equal(await connected(), true);
    await page.reload();
    await until(() => window.Shiny?.shinyapp?.isConnected());
    await page.waitForTimeout(2500);
    assert.equal(await connected(), true);
    await hero.waitFor();

    // 4. The first data of a session. A file that holds July and August only starts from those
    // months, not from the April to September of the study.
    await button("Select Your Data").click();
    await chooser.waitFor();
    assert.deepEqual(await chooser.locator(".chooser-actions").getByRole("button").allInnerTexts(),
                     ["Cancel", "Use this folder", "Open"]);
    assert.ok(await chooser.locator(".chooser-place").count() >= 2);
    // The dialog is a named dialog, and a newly opened folder is not read out as a whole.
    assert.equal(await page.getByRole("dialog", { name: "Choose your temperature data" }).count(), 1);
    assert.equal(await page.locator("#shiny-modal").getAttribute("aria-modal"), "true");
    assert.equal(await page.locator("#chooser").getAttribute("aria-live"), "off");
    if (fixtures) {
      await openData(path.join(fixtures, "july_august.nc"), true);
      await shows(page.locator("#period_note"), "July–August 2012: 62 days.");
      assert.equal(await page.getByLabel("First month each year").inputValue(), "7");
      assert.equal(await page.getByLabel("Last month each year").inputValue(), "8");
      assert.match(await selection.innerText(), /Contains\s+daily maximum temperature/);
      assert.match(await selection.innerText(), /2012-07-01 to 2012-08-31: 62 days, July–August/);
      await page.locator("#start:visible").waitFor();
      await button("Change data").click();
      await hero.waitFor();

      // A temperature that the file does not call a daily maximum is asked about, not assumed.
      await openData(path.join(fixtures, "daily_minimum.nc"));
      const asked = page.locator("#question");
      await shows(asked.locator("h2"), "Is this the temperature to analyze?");
      assert.equal(await asked.locator("input[type=radio]").count(), 1);
      assert.equal(await asked.locator("input[type=radio]:checked").count(), 0);
      assert.match(await asked.innerText(), /does not identify it as daily maximum temperature/);
      assert.doesNotMatch(await asked.innerText(), /suggested answer/);
      assert.equal(await page.locator("#start:visible").count(), 0);
      // "Use this answer" before an answer is chosen says what is missing; choosing one takes that away.
      assert.equal(await asked.getByRole("alert").count(), 0);
      await button("Use this answer").click();
      await shows(asked.getByRole("alert"), "Choose an answer first.");
      await asked.locator("input[type=radio]").check();
      await until(() => !document.querySelector("#question [role=alert]"));
      await button("Use this answer").click();
      await page.locator("#start:visible").waitFor();
      await shows(selection, "Daily temperature (variable tasmin, not identified as daily maximum temperature)");
      await shows(selection, "Your answer: variable = tasmin");
      await button("Change data").click();
      await hero.waitFor();

      // Numbers that cannot be air temperature stop the selection: no question and no Run.
      await openData(path.join(fixtures, "undeclared_missing.nc"));
      await shows(selection.getByRole("alert"), "which cannot be air temperature");
      await page.locator("#run_button button[disabled]").waitFor();
      assert.equal(await page.locator("#start").count(), 0);
      assert.equal(await page.locator("#question .question").count(), 0);
      await button("Change data").click();
      await hero.waitFor();

      // Unlabelled numbers above 60 may be degrees Celsius or Fahrenheit: the units are asked
      // and neither answer is suggested.
      await openData(path.join(fixtures, "no_units_66.nc"));
      await shows(asked.locator("h2"), "Which units are these temperatures in?");
      assert.equal(await asked.locator("input[type=radio]").count(), 2);
      assert.equal(await asked.locator("input[type=radio]:checked").count(), 0);
      assert.doesNotMatch(await asked.innerText(), /suggested answer/);
      assert.equal(await page.locator("#start:visible").count(), 0);
      await button("Change data").click();
      await hero.waitFor();

      // Coordinates stored in single precision on a 0.1-degree grid near 250 E: their spacing is
      // even only to that rounding. The readers take the even grid, and so does the window.
      await openData(path.join(fixtures, "single_precision.nc"));
      await shows(selection, "6 by 4 cells at 0.1°");
      await page.locator("#start:visible").waitFor();
      await button("Change data").click();
      await hero.waitFor();

      // Raw hourly ERA5 whose points fill one row of one-degree cells: the reader needs two cells
      // along each axis, so the selection is refused before Run, with the setting to change.
      await openData(path.join(fixtures, "era5_one_row.nc"));
      await shows(selection, "8 by 4 points at 0.25°, 20°E to 21.75°E and 10°N to 10.75°N");
      await shows(selection, "SCORCH takes the maximum of each UTC day and averages complete blocks to 1° cells.");
      await shows(page.locator("#review").getByRole("alert"),
                  "The ERA5 grid in era5_one_row.nc gives fewer than two 1-degree cells along its latitude. " +
                  "Choose a smaller 'Output grid size (degrees)', or tick 'Keep the input grid resolution'.");
      await page.locator("#run_button button[disabled]").waitFor();
      // The grid of the file itself can be analyzed.
      await page.getByLabel("Keep the input grid resolution").check();
      await page.locator("#start:visible").waitFor();
      await shows(selection, "SCORCH takes the maximum of each UTC day and keeps this grid.");
      // An emptied grid size is refused for raw data, where its field is on screen. It stays empty
      // from here on: for the example and the prepared files below that field is not shown and
      // must stop nothing.
      await page.getByLabel("Keep the input grid resolution").uncheck();
      await page.getByLabel("Output grid size (degrees)").fill("");
      await page.getByLabel("Output grid size (degrees)").press("Tab");
      await shows(page.locator("#review").getByRole("alert"), "'Output grid size (degrees)' must be one number");
      await page.locator("#run_button button[disabled]").waitFor();
      // The section opened itself for these messages; closed again, it is as the steps below expect it.
      await page.getByText("Advanced settings", { exact: true }).click();
      await until(() => !document.querySelector("details.advanced").open);
      await button("Change data").click();
      await hero.waitFor();
    } else {
      await cancelDialog();
      console.log("SCORCH_UI_FIXTURES is not set: the part-year file, the daily minimum, the undeclared " +
                  "missing values, the numbers without units and the raw ERA5 file were not tried.");
    }

    // 5. The included example goes straight to its settings and says what it is.
    await button("Use included example").click();
    await page.locator("#start:visible").waitFor();
    // Run analysis is in view when the settings appear, without scrolling: its card stays at the
    // lower edge of the window (the button used to lie under that edge).
    await page.evaluate(() => window.scrollTo(0, 0));
    const runButton = await page.locator("#start").boundingBox();
    assert.ok(runButton.y >= 0 && runButton.y + runButton.height <= page.viewportSize().height,
              "Run analysis is in view when the settings appear");
    assert.match(await selection.innerText(), /included example/i);
    assert.match(await selection.innerText(), /North America example/);
    assert.match(await selection.innerText(), /1996-04-01 to 2025-09-30: 5,490 days, April–September/);
    assert.match(await selection.innerText(), /Temperature\s+°C/);
    await shows(page.locator("#period_note"), "April–September in every selected year: 30 years (1996–2025), 5,490 days.");
    assert.equal(await page.locator("details.advanced").getAttribute("open"), null);
    assert.equal(await page.getByLabel("Bootstrap repetitions").isVisible(), false);
    // How the file is read is detected: the window has no fields for it and no date limit, and the
    // card names the data plainly.
    for (const id of ["input_format", "input_units", "input_variable", "input_time", "input_longitude", "input_latitude",
                      "limit_dates"]) {
      assert.equal(await page.locator("#" + id).count(), 0, id);
    }
    assert.equal(await page.getByText("How the file is read").count(), 0);
    assert.doesNotMatch(await selection.innerText(), /variable tmax|first time step|Prepared/);
    assert.match(await page.locator("#destination").innerText(), /output[\\/]SCORCH_run_001$/);
    await shot("settings", { fullPage: true });

    // 6. One chooser for files and folders: type or browse to the folder, select the file, Open.
    await button("Change data").click();
    await hero.waitFor();
    await button("Select Your Data").click();
    await chooser.waitFor();
    await goTo(path.dirname(nc), path.basename(nc));
    // The list is named after its folder, and every row carries its whole name.
    assert.equal(await page.locator("#chooser_list").getAttribute("aria-label"),
                 "Folders and files in " + path.basename(path.dirname(nc)));
    const row = chooser.locator(".chooser-row").filter({ hasText: path.basename(nc) }).first();
    assert.equal(await row.locator(".chooser-name").getAttribute("title"), path.basename(nc));
    // In a window 900 px wide the names keep their room (they had 18 px), and in a short window
    // (the smallest RStudio dialog, and shorter) the buttons stay in view.
    await page.setViewportSize({ width: 900, height: 860 });
    assert.ok((await row.locator(".chooser-name").boundingBox()).width > 200, "file names have room in a 900 px window");
    for (const height of [530, 420]) {
      await page.setViewportSize({ width: 1060, height });
      for (const action of await chooser.locator(".chooser-actions").getByRole("button").all()) {
        const box = await action.boundingBox();
        assert.ok(box.y >= 0 && box.y + box.height <= height, (await action.innerText()) + " is in view at 1060 by " + height);
      }
    }
    await page.setViewportSize({ width: 1060, height: 860 });
    // On Windows a drive letter typed without a slash is the top of that drive.
    const drive = path.parse(dir).root.slice(0, 2);
    if (process.platform === "win32" && /^[A-Za-z]:$/.test(drive)) {
      await chooser.locator("#chooser_path").fill(drive);
      await chooser.locator("#chooser_path").press("Enter");
      await until(top => document.querySelector(".chooser").dataset.folder === top, drive.toUpperCase() + "/");
      assert.equal(await chooser.getByRole("alert").count(), 0);
    }
    // A folder without NetCDF files cannot be used as data.
    await goTo(dir, "temperatures.zip");
    await button("Use this folder").click();
    await shows(chooser.getByRole("alert"), "no NetCDF");
    // Where the files are one level down, one folder per year under one file name, the folder is
    // read with the files of its subfolders. These two are daily values that are not ERA5's: the
    // window says how they can be read together.
    const yearly = path.join(dir, "yearly");
    for (const year of ["2001", "2002"]) {
      fs.mkdirSync(path.join(yearly, year), { recursive: true });
      fs.copyFileSync(nc, path.join(yearly, year, "data.nc"));
    }
    await goTo(yearly, "2001");
    await button("Use this folder").click();
    await shows(selection, "To read these 2 files of daily values together, set 'format: era5' in a configuration file");
    await button("Change data").click();
    await hero.waitFor();
    // A folder of .nc4 files only is taken the same way.
    const nc4 = path.join(dir, "nc4 only");
    fs.mkdirSync(nc4);
    fs.copyFileSync(nc, path.join(nc4, "daily.nc4"));
    await button("Select Your Data").click();
    await chooser.waitFor();
    await goTo(nc4, "daily.nc4");
    await shows(chooser, "This folder has 1 .nc4 file. Use this folder analyzes it as one ERA5 dataset.");
    await button("Use this folder").click();
    await shows(selection, "daily.nc4");
    await button("Change data").click();
    await hero.waitFor();
    await button("Select Your Data").click();
    await chooser.waitFor();
    // Text that ends in half a character, as a cut emoji does, is answered in the dialog; it used
    // to end the session. The field is given that text directly: the typing of this test would repair it.
    const halved = await chooser.locator("#chooser_path").evaluate(field => {
      field.value = "results " + String.fromCharCode(0xDE00);
      return field.value.charCodeAt(field.value.length - 1);
    });
    assert.equal(halved, 0xDE00);
    await chooser.locator("#chooser_path").press("Enter");
    await shows(chooser.getByRole("alert"), "That text holds characters that cannot be read. Type the path again.");
    assert.equal(await connected(), true);
    // A file that arrived after its folder was listed: "Use this folder" takes the folder at the first click.
    const arriving = path.join(dir, "arriving");
    fs.mkdirSync(arriving);
    await goTo(arriving);
    fs.copyFileSync(nc, path.join(arriving, "arrived.nc"));
    await button("Use this folder").click();
    await shows(selection, "arrived.nc");
    await button("Change data").click();
    await hero.waitFor();
    await button("Select Your Data").click();
    await chooser.waitFor();
    // A folder that disappears while the dialog shows it: a message, and the window stays.
    const unplugged = path.join(dir, "unplugged");
    fs.mkdirSync(unplugged);
    await goTo(unplugged);
    // Renamed, as in a file manager. Windows can hold a new folder for a moment: try for two seconds.
    for (let attempt = 1; ; attempt++) {
      try {
        fs.renameSync(unplugged, unplugged + " renamed");
        break;
      } catch (error) {
        if (attempt >= 20) throw error;
        await page.waitForTimeout(100);
      }
    }
    await button("Use this folder").click();
    await shows(chooser.getByRole("alert"), "That folder could not be found");
    await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: path.basename(dir) }).waitFor();
    assert.equal(await connected(), true);
    await goTo(path.dirname(nc), path.basename(nc));
    await chooser.locator(".chooser-row").filter({ hasText: path.basename(nc) }).first().click();
    await button("Open").click();
    await page.locator("#start:visible").waitFor();
    assert.match(await selection.innerText(), /your data/i);
    assert.match(await selection.innerText(), new RegExp(path.basename(nc).replace(/[.]/g, "\\.")));
    assert.match(await selection.innerText(), /Contains\s+daily maximum temperature/);

    // 7. June to August of a file that holds other months: stated for every selected year.
    await page.getByLabel("First month each year").selectOption("6");
    await page.getByLabel("Last month each year").selectOption("8");
    await shows(page.locator("#period_note"), "June–August in every selected year: 30 years (1996–2025), 2,760 days.");
    await shows(page.locator("#period_note"), "This file also holds April–May, September. Those months are not analyzed.");
    // A saved configuration with explicit dates limits the run to them; the summary names the period.
    const limited = path.join(dir, "limited.yaml");
    fs.writeFileSync(limited, ["period:", "  warm_season_months: [6, 7, 8]", "  start_date: '2012-01-01'",
                               "  end_date: '2012-12-31'", ""].join("\n"));
    await button("Reuse a saved configuration").click();
    await chooser.waitFor();
    await goTo(dir, "limited.yaml");
    await chooser.locator(".chooser-row").filter({ hasText: "limited.yaml" }).first().dblclick();
    await shows(page.locator("#period_note"), "June–August 2012: 92 days.");
    await shows(page.locator("#review"), "92 selected dates");

    // 8. Results go to a folder typed, chosen or made in the same chooser; nothing is created before Run.
    await button("Choose or create folder").click();
    await chooser.waitFor();
    // "Use this folder" takes the folder written in the field, without Enter: a missing one is refused...
    await chooser.locator("#chooser_path").fill(path.join(dir, "no such folder"));
    await button("Use this folder").click();
    await shows(chooser.getByRole("alert"), "That folder could not be found");
    // An open folder whose name ends in a blank is taken too. The page sends its text without the
    // blanks at its ends, and that text alone names no folder. The blank is a no-break space, the
    // one that Windows keeps at the end of a name, and the step is for Windows, where R reads such
    // a name in every session; elsewhere that depends on the locale of the R session, and the
    // package tests try names that end in a blank there.
    if (process.platform === "win32") {
      const ending = path.join(dir, "ends in a blank" + String.fromCharCode(160));
      fs.mkdirSync(ending);
      await chooser.locator("#chooser_path").fill(ending);
      await chooser.locator("#chooser_path").press("Enter");
      await until(name => document.querySelector(".chooser").dataset.folder.endsWith(name), path.basename(ending));
      await button("Use this folder").click();
      await shows(page.locator("#destination"), /ends in a blank\s[\\/]SCORCH_run_001$/);
      await button("Choose or create folder").click();
      await chooser.waitFor();
    }
    // ...and an existing one becomes the destination, not the folder that happens to be open.
    const typed = path.join(dir, "typed folder");
    fs.mkdirSync(typed);
    await chooser.locator("#chooser_path").fill(typed);
    await button("Use this folder").click();
    await shows(page.locator("#destination"), /[\\/]typed folder[\\/]SCORCH_run_001$/);
    await button("Choose or create folder").click();
    await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: "typed folder" }).waitFor();
    // The keyboard is in the list when the dialog has finished opening, not on the dialog itself.
    // The dialog is closed and opened again on the same folder: R then sends no new list, so the
    // list that is there is the one that must hold the keyboard.
    await cancelDialog();
    // The dialog gone, the keyboard is back on the button that opened it.
    await until(() => document.activeElement && document.activeElement.id === "browse_output");
    const opened = page.evaluate(() => new Promise((done, fail) => {
      window.jQuery(document).one("shown.bs.modal", () => done());
      setTimeout(() => fail(new Error("the dialog did not finish opening")), 10000);
    }));
    opened.catch(() => {});
    await button("Choose or create folder").click();
    await opened;
    await until(() => document.activeElement && document.activeElement.id === "chooser_list");
    await goTo(dir);
    // Backspace goes up from every folder, also from one that lists nothing.
    fs.mkdirSync(path.join(dir, "nothing in it"));
    await goTo(path.join(dir, "nothing in it"));
    await shows(chooser.locator(".chooser-empty"), "This folder has no subfolders.");
    await page.locator("#chooser_list").focus();
    await page.keyboard.press("Backspace");
    await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: path.basename(dir) }).waitFor();
    // A name typed for a new folder is not dropped by "Use this folder": the folder is made and
    // opened, as Create does, and the dialog stays for the next click.
    await button("New folder").click();
    await page.getByLabel("Name of the new folder").fill("made on the way");
    await button("Use this folder").click();
    await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: "made on the way" }).waitFor();
    assert.equal(fs.existsSync(path.join(dir, "made on the way")), true);
    assert.equal(await page.locator("#shiny-modal").count(), 1);
    await goTo(dir);
    // A folder highlighted after a name was typed is the choice: "Use this folder" takes that
    // folder and makes none of the typed name.
    await button("New folder").click();
    await page.getByLabel("Name of the new folder").fill("left behind");
    await chooser.locator(".chooser-row").filter({ hasText: "made on the way" }).first().click();
    await button("Use this folder").click();
    await shows(page.locator("#destination"), /[\\/]made on the way[\\/]SCORCH_run_001$/);
    assert.equal(fs.existsSync(path.join(dir, "left behind")), false);
    await button("Choose or create folder").click();
    await chooser.waitFor();
    await goTo(dir);
    await button("New folder").click();
    await page.getByLabel("Name of the new folder").fill("reviewer results");
    await button("Create").click();
    await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: "reviewer results" }).waitFor();
    await button("Use this folder").click();
    await shows(page.locator("#destination"), /[\\/]reviewer results[\\/]SCORCH_run_001$/);
    assert.equal(fs.existsSync(path.join(output, "SCORCH_run_001")), false);
    assert.equal(fs.existsSync(path.join(typed, "SCORCH_run_001")), false);

    // 9. Advanced settings stay collapsed until asked for, start at the study's values and keep
    // seeds and predictors.
    await page.getByText("Advanced settings", { exact: true }).click();
    for (const label of ["Random seed for daily maxima", "Random seed for event maxima", "Local temperature percentile",
                         "Candidate distances (grid cells)", "Raster cell size (km)"]) {
      assert.equal(await page.getByLabel(label).isVisible(), true, label);
    }
    for (const predictor of ["Longitude", "Latitude", "Mean Tmax", "Tmax standard deviation"]) {
      assert.equal(await page.getByLabel(predictor, { exact: true }).isChecked(), true, predictor);
    }
    assert.equal(await page.getByLabel("Bootstrap repetitions").inputValue(), "5000");
    // An emptied field is reported by its label, above the section and beside a disabled Run
    // button, and the field itself is marked; the card used to go blank.
    const distances = page.getByLabel("Candidate distances (grid cells)");
    const candidates = await distances.inputValue();
    await distances.fill("");
    await distances.press("Tab");
    await shows(page.locator("#advanced_problem"), "'Candidate distances (grid cells)' must list distinct, finite positive numbers.");
    await shows(page.locator("#review"), "'Candidate distances (grid cells)' must list distinct, finite positive numbers.");
    await page.locator("#run_button button[disabled]").waitFor();
    assert.equal(await page.locator("#start").count(), 0);
    await until(() => document.getElementById("clustering_eps_values").getAttribute("aria-invalid") === "true");
    // Under a contrast theme, which drops the red border, the refused field keeps a mark.
    await page.emulateMedia({ forcedColors: "active" });
    assert.equal(await distances.evaluate(field => getComputedStyle(field).outlineStyle), "dashed");
    await page.emulateMedia({ forcedColors: "none" });
    assert.equal(await page.locator("details.advanced [aria-invalid]").count(), 1);
    await distances.fill(candidates);
    await distances.press("Tab");
    await page.locator("#start:visible").waitFor();
    await until(() => !document.querySelector("#advanced_problem .problem") && !document.querySelector("details.advanced [aria-invalid]"));
    // A setting that is refused while the section is closed opens it.
    await page.getByText("Advanced settings", { exact: true }).click();
    assert.equal(await page.locator("details.advanced").getAttribute("open"), null);
    const scale = page.locator("#ellipses_scale_factor");
    const factor = await scale.inputValue();
    await scale.evaluate(field => { field.value = "0"; field.dispatchEvent(new Event("change", { bubbles: true })); });
    await shows(page.locator("#advanced_problem"), "'Ellipse scale factor' must be positive");
    await until(() => document.querySelector("details.advanced").open &&
      document.getElementById("ellipses_scale_factor").getAttribute("aria-invalid") === "true");
    // The field goes back to the value the window gave it, which the run below must record.
    await scale.fill(factor);
    await scale.press("Tab");
    await page.locator("#start:visible").waitFor();
    await until(() => !document.querySelector("#advanced_problem .problem") && !document.querySelector("details.advanced [aria-invalid]"));
    await page.getByLabel("Bootstrap repetitions").fill("10");
    await page.getByLabel("Bootstrap repetitions").press("Tab");
    await shows(page.locator("#review"), "92 selected dates");

    // 10. Run: progress, then the summary in the window and the files on disk.
    // The running view takes the page: its heading, the four stages, then the results once saved.
    await button("Run analysis").click();
    await page.locator("#running").waitFor();
    assert.equal(await page.locator("#running .running-title").innerText(), "Running scoRch.simulator");
    assert.deepEqual(await page.locator("#running .stage-label").allInnerTexts(),
                     ["Stage 1: Heatwave Detection", "Stage 2: Heatwave Clustering", "Stage 3: Heatwave Classification",
                      "Stage 4: Heatwave Modeling"]);
    // A second click cannot start a second run: the settings, and their button, are hidden.
    assert.equal(await page.locator("#start").isVisible(), false);
    await button("Open results folder").waitFor({ timeout: 300000 });
    await page.locator("#running").waitFor({ state: "hidden" });
    // Reading, the four stages in order, then saving: the run's own messages, nothing estimated.
    assert.match(await page.locator("#running").getAttribute("data-seen"), /^0+1+2+3+4+5+$/);
    const completion = page.locator("#completion");
    const run = (await completion.locator(".outcome .path").innerText()).trim();
    assert.equal(real(run), real(path.join(output, "SCORCH_run_001")));
    assert.match(await completion.locator(".outcome").innerText(), /Analysis complete/);
    assert.match(await completion.locator(".outcome").innerText(), /3 compound events from 8 ellipses on 3 regionally extensive days/);
    assert.deepEqual((await completion.locator(".type-counts li strong").allInnerTexts()).map(Number), [0, 3, 0, 0]);
    assert.deepEqual(await completion.locator(".actions").getByRole("button").allInnerTexts(),
                     ["Open summary", "Open results folder", "Set up another run"]);
    // The three buttons follow the event counts and the results path in the card of the outcome;
    // what was analyzed is listed below them.
    const top = async where => (await where.first().boundingBox()).y;
    assert.equal(await completion.locator("section.card.outcome .actions button").count(), 3);
    assert.ok(await top(completion.locator(".outcome .type-counts")) < await top(completion.locator(".outcome .path")));
    assert.ok(await top(completion.locator(".outcome .path")) < await top(page.locator("#open_summary")));
    // What was analyzed is not on the page; Summary.txt records it, with the repetitions asked for above.
    assert.equal(await completion.locator(".summary").count(), 0);
    assert.doesNotMatch(await completion.innerText(), /What was analyzed|Results by stage/);
    assert.match(fs.readFileSync(path.join(run, "Summary.txt"), "utf8"), /10 bootstrap repetitions/);
    for (const file of ["Summary.txt", "config.yaml", "Heatwave_Statistics.xlsx", "catalogs/05_compound_heatwave_events.xlsx",
                        "modeling/Power_Law_Model.xlsx", "figures/geometry/Ellipses_by_type.png"]) {
      assert.ok(fs.existsSync(path.join(run, file)), file);
    }
    assert.match(fs.readFileSync(path.join(run, "Summary.txt"), "utf8"), /92 dates/);
    const saved = fs.readFileSync(path.join(run, "config.yaml"), "utf8");
    assert.match(saved, /bootstrap_repetitions: 10/);
    // Whole numbers are saved as a scripted run saves them, so the two kinds of run compare equal.
    assert.match(saved, /min_samples_values:\r?\n\s*- 4\r?\n/);
    // The grid size that the raw ERA5 file left empty is not read for a prepared file: the saved
    // settings keep the one-degree cells of the configuration in use.
    assert.match(saved, /era5_cell_size_deg: 1(\.0)?\r?\n/);

    // The map now shows the events of this run, not the demonstration, and the keyboard
    // continues at the heading of the results.
    const mine = page.locator("#explorer-run");
    await mine.locator("svg.map").first().waitFor();
    await until(() => document.activeElement && document.activeElement.id === "results-title");
    assert.equal(await page.locator("#results-title").getAttribute("tabindex"), "-1");
    assert.match(await mine.locator(".explorer-head").innerText(), /this run/i);
    assert.doesNotMatch(await mine.locator(".explorer-head").innerText(), /demonstration/i);
    // No heading, file name or event caption: the dropdown names each event.
    assert.equal(await mine.locator(".explorer-head h2, .explorer-source, .event-line").count(), 0);
    assert.doesNotMatch(await mine.innerText(), /Events from this run/);
    assert.doesNotMatch(await mine.innerText(), new RegExp(path.basename(nc).replace(/[.]/g, "\\.")));
    assert.ok(await mine.locator(".event-pick select").isVisible());
    assert.match(await mine.locator(".type-cards").innerText(), /3 events over 3 days/);
    assert.equal(await mine.locator(".type-card[disabled]").count(), 3);
    // Types without events keep their definitions readable, and the temperature classes of this
    // run each have a colour of their own. The run has fewer classes than the ramp has colours
    // (eight), so this guards the legend of such a run only: the blending that keeps nine or more
    // classes apart is not tried here.
    assert.equal(await mine.locator(".type-card[disabled]").first().evaluate(card => getComputedStyle(card).opacity), "1");
    const classes = await mine.locator(".heat-scale li").evaluateAll(items => items.map(item => getComputedStyle(item).backgroundColor));
    assert.equal(new Set(classes).size, classes.length);
    await shows(mine.locator(".map-day"), "Single-day event");
    assert.equal(await mine.locator(".event-pick option").count(), 3);
    const shown = await mine.locator(".map-date").innerText();
    await mine.locator(".event-pick select").selectOption({ index: 2 });
    await until(day => document.querySelector("#explorer-run .map-date").textContent !== day, shown);
    assert.ok(await mine.locator(".map .frame .day:not(.leaving) .ellipse").count() >= 2);
    await shot("results", { fullPage: true });

    // 11. Another run gets its own folder; saved settings can be reused on other data.
    await button("Set up another run").click();
    await shows(page.locator("#destination"), "SCORCH_run_002");
    await page.getByLabel("First month each year").selectOption("4");
    await button("Reuse a saved configuration").click();
    await chooser.waitFor();
    await goTo(run, "config.yaml");
    await chooser.locator(".chooser-row").filter({ hasText: "config.yaml" }).first().dblclick();
    await shows(page.locator("#review"), "92 selected dates");
    assert.equal(await page.getByLabel("First month each year").inputValue(), "6");
    await shows(page.locator("#loaded"), "Settings loaded from config.yaml: the advanced settings and the results folder " +
                                         "stay in use when you change the data. The months, the analysis dates and " +
                                         "how the file is read start again with other data; reuse the file to apply " +
                                         "its months and dates again.");
    // The same file reused brings its values back. A month and the repetitions are changed by hand,
    // then the file is loaded a second time: it holds the settings already in use, and the form
    // must show them again all the same.
    await page.getByLabel("First month each year").selectOption("5");
    await page.getByText("Advanced settings", { exact: true }).click();
    await page.getByLabel("Bootstrap repetitions").fill("12");
    await page.getByLabel("Bootstrap repetitions").press("Tab");
    await shows(page.locator("#review"), "123 selected dates");
    await button("Reuse a saved configuration").click();
    await chooser.waitFor();
    await goTo(run, "config.yaml");
    await chooser.locator(".chooser-row").filter({ hasText: "config.yaml" }).first().dblclick();
    await shows(page.locator("#review"), "92 selected dates");
    assert.equal(await page.getByLabel("First month each year").inputValue(), "6");
    assert.equal(await page.getByLabel("Bootstrap repetitions").inputValue(), "10");
    await button("Change data").click();
    await hero.waitFor();
    await button("Select Your Data").click();
    await chooser.waitFor();
    // In double quotes, as "Copy as path" in Windows copies a file.
    await chooser.locator("#chooser_path").fill('"' + path.join(dir, "temperatures.zip") + '"');
    await chooser.locator("#chooser_path").press("Enter");
    await shows(selection, "temperatures.zip");
    // Other data: the loaded settings stay in use and the note still says so. The months are the
    // configuration's, the record is the whole of the new data until it is limited again.
    await shows(page.locator("#loaded"), "Settings loaded from config.yaml: the advanced settings and the results folder " +
                                         "stay in use when you change the data.");
    await shows(page.locator("#review"), "2,760 selected dates");
    assert.equal(await page.getByLabel("First month each year").inputValue(), "6");
    assert.equal(await page.getByLabel("Last month each year").inputValue(), "8");
    await shows(page.locator("#destination"), /[\\/]reviewer results[\\/]SCORCH_run_002$/);
    // Reused again, the configuration brings its dates back for these data.
    await button("Reuse a saved configuration").click();
    await chooser.waitFor();
    await goTo(run, "config.yaml");
    await chooser.locator(".chooser-row").filter({ hasText: "config.yaml" }).first().dblclick();
    await shows(selection, "temperatures.zip");
    await page.getByText("Advanced settings", { exact: true }).click();
    assert.equal(await page.getByLabel("Bootstrap repetitions").inputValue(), "10");
    await shows(page.locator("#review"), "92 selected dates");
    await button("Run analysis").click();
    await button("Open results folder").waitFor({ timeout: 300000 });
    const next = (await completion.locator(".outcome .path").innerText()).trim();
    assert.equal(real(next), real(path.join(output, "SCORCH_run_002")));
    assert.match(fs.readFileSync(path.join(next, "config.yaml"), "utf8"), /temperatures[.]zip/);
    assert.match(fs.readFileSync(path.join(next, "config.yaml"), "utf8"), /bootstrap_repetitions: 10/);
    assert.match(fs.readFileSync(path.join(next, "Summary.txt"), "utf8"), /92 dates/);
    assert.ok(fs.existsSync(path.join(run, "config.yaml")));
    // The ZIP and the file it holds give the same catalog of events.
    const events = folder => fs.readFileSync(path.join(folder, "Summary.txt"), "utf8").split(/\r?\n/)
      .filter(line => /^ {2}Stage \d|^ {11}Type \d/.test(line));
    assert.deepEqual(events(next), events(run));
    assert.ok(events(run).length >= 9);

    // 12. A reload of the window keeps the finished run on screen: its outcome, its folder and its map.
    await page.reload();
    await until(() => window.Shiny?.shinyapp?.isConnected());
    await shows(completion.locator(".outcome"), "Analysis complete");
    assert.equal(real((await completion.locator(".outcome .path").innerText()).trim()), real(next));
    assert.deepEqual((await completion.locator(".type-counts li strong").allInnerTexts()).map(Number), [0, 3, 0, 0]);
    assert.deepEqual(await completion.locator(".actions").getByRole("button").allInnerTexts(),
                     ["Open summary", "Open results folder", "Set up another run"]);
    await mine.locator("svg.map").first().waitFor();
    assert.equal(await mine.locator(".event-pick option").count(), 3);

    // 13. A run that stops says why, and the keyboard continues at its heading as after a finished run.
    if (fixtures) {
      // "Set up another run" lets go of the run that the reload kept: the window is at its opening
      // screen, and another reload stays there.
      await button("Set up another run").click();
      await hero.waitFor();
      await page.reload();
      await until(() => window.Shiny?.shinyapp?.isConnected());
      await hero.waitFor();
      assert.equal(await page.locator("#completion .outcome").count(), 0);
      await openData(path.join(fixtures, "daily_minimum.nc"));
      await page.locator("#question input[type=radio]").check();
      await button("Use this answer").click();
      await page.locator("#start:visible").waitFor();
      await button("Run analysis").click();
      await shows(completion.locator(".outcome"), "Analysis stopped");
      await shows(completion.locator(".outcome").getByRole("alert"), "The regional threshold is zero");
      await until(() => document.activeElement && document.activeElement.id === "results-title");
      assert.equal(await page.locator("#explorer-run").count(), 0);
    }
    assert.deepEqual(errors, []);
    await button("Close").click();
    console.log("Address key, opening screen, demonstration map, " +
                (fixtures ? "first data, a question and a refusal, " : "") +
                "file chooser, seasonal settings, two runs, results summary and saved settings passed.");
  } catch (error) {
    if (page && !page.isClosed()) {
      if (process.env.SCORCH_UI_SHOTS) await page.screenshot({ path: path.join(process.env.SCORCH_UI_SHOTS, "failure.png"), fullPage: true }).catch(() => {});
      console.error((await page.locator("body").innerText({ timeout: 3000 }).catch(() => "")).slice(0, 6000));
    }
    throw error;
  } finally { await browser.close(); }
}
run().catch(error => { console.error(error); process.exitCode = 1; });
