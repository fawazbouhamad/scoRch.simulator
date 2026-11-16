// Drive the installed setup window in a real browser, as a first-time user would.
// The window must already be open on SCORCH_UI_URL (default http://127.0.0.1:7123),
// from an R session whose working folder is SCORCH_UI_PROJECT.
const { chromium } = require("playwright-core");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

async function run() {
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
    const dir = process.env.SCORCH_UI_PROJECT;
    const nc = process.env.SCORCH_UI_DATA;
    const output = path.join(dir, "reviewer results");
    const chooser = page.locator(".chooser");
    // Type a folder into the chooser's path field, as one would paste it.
    async function goTo(folder, expected) {
      await chooser.locator("#chooser_path").fill(folder);
      await chooser.locator("#chooser_path").press("Enter");
      await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: path.basename(folder) }).waitFor();
      if (expected) await chooser.locator(".chooser-row").filter({ hasText: expected }).first().waitFor();
    }

    await page.goto(process.env.SCORCH_UI_URL || "http://127.0.0.1:7123");
    await page.waitForFunction(() => window.Shiny?.shinyapp?.isConnected());

    // 1. The opening screen: the name spelled out, two choices, nothing running.
    const hero = page.locator(".hero");
    await hero.waitFor();
    assert.match(await page.locator(".masthead").innerText(), /Spatiotemporal Classification of Regional Compound Heatwaves/);
    assert.deepEqual(await hero.getByRole("button").allInnerTexts(), ["Choose your own data", "Use included example"]);
    assert.match(await button("Choose your own data").getAttribute("class"), /btn-primary/);
    assert.doesNotMatch(await button("Use included example").getAttribute("class"), /btn-primary/);
    assert.equal(await page.locator("#start:visible").count(), 0);
    assert.equal(fs.existsSync(path.join(dir, "output")), false);

    // 2. The demonstration map: labelled as bundled, four types, still by default under reduced motion.
    const demo = page.locator("#explorer-demo");
    await demo.locator("svg.map").first().waitFor();
    assert.match(await demo.locator(".explorer-head").innerText(), /demonstration/i);
    assert.match(await demo.locator(".explorer-head").innerText(), /Bundled North America example.*not your data/s);
    assert.equal(await demo.locator(".type-card").count(), 4);
    for (const name of ["Widespread (Isolated)", "Spatially Clustered", "Temporally Clustered", "Compound Clustering (Multi-Type)"]) {
      assert.match(await demo.locator(".type-cards").innerText(), new RegExp(name.replace(/[()]/g, "\\$&")));
    }
    assert.match(await demo.locator(".legend").innerText(), /daily maximum temperature \(°C\)/);
    assert.equal(await demo.locator(".map-day").innerText(), "Day 1 of 7");
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
    await shot("opening-screen", { fullPage: true });
    await demo.locator(".play").click();
    await page.waitForFunction(day => document.querySelector("#explorer-demo .map-date").textContent !== day, firstDay);
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
    assert.match(await demo.locator(".rule").innerText(), /one cluster on every day.*Type 3: Temporally Clustered/s);
    assert.ok(await demo.locator(".map .cell").count() > 0);
    assert.equal(await demo.locator(".map .frame .day:not(.leaving) .ellipse").count(), 1);
    // A single-day event is one frame without playback.
    await demo.locator(".type-card").first().click();
    assert.equal(await demo.locator(".map-day").innerText(), "Single-day event");
    assert.equal(await demo.locator(".play").isVisible(), false);
    assert.equal(await demo.getByRole("button", { name: "Next day" }).isVisible(), false);
    assert.match(await demo.locator(".rule").innerText(), /One regionally extensive day with one cluster.*Type 1/s);
    await demo.locator(".type-card").nth(3).click();
    assert.match(await demo.locator(".rule").innerText(), /a mix of one-cluster and several-cluster days.*Type 4/s);

    // 3. The included example goes straight to its settings and says what it is.
    await button("Use included example").click();
    await page.locator("#start:visible").waitFor();
    const selection = page.locator("#selection");
    assert.match(await selection.innerText(), /included example/i);
    assert.match(await selection.innerText(), /North America example/);
    assert.match(await selection.innerText(), /1996-04-01 to 2025-09-30: 5,490 days, April–September/);
    assert.match(await selection.innerText(), /°C in the file/);
    assert.match(await page.locator("#period_note").innerText(), /April–September in every selected year: 30 years \(1996–2025\), 5,490 days\./);
    assert.equal(await page.locator("details.advanced").getAttribute("open"), null);
    assert.equal(await page.getByLabel("Bootstrap repetitions").isVisible(), false);
    assert.match(await page.locator("#destination").innerText(), /output[\\/]SCORCH_run_001$/);
    await shot("settings", { fullPage: true });

    // 4. One chooser for files and folders: type or browse to the folder, select the file, Open.
    await button("Change data").click();
    await hero.waitFor();
    await button("Choose your own data").click();
    await chooser.waitFor();
    assert.deepEqual(await chooser.locator(".chooser-actions").getByRole("button").allInnerTexts(),
                     ["Cancel", "Use this folder", "Open"]);
    assert.ok(await chooser.locator(".chooser-place").count() >= 2);
    await goTo(path.dirname(nc), path.basename(nc));
    // A folder without NetCDF files cannot be used as data.
    await goTo(dir, "temperatures.zip");
    await button("Use this folder").click();
    await chooser.getByRole("alert").filter({ hasText: "no NetCDF" }).waitFor();
    await goTo(path.dirname(nc), path.basename(nc));
    await chooser.locator(".chooser-row").filter({ hasText: path.basename(nc) }).first().click();
    await button("Open").click();
    await page.locator("#start:visible").waitFor();
    assert.match(await selection.innerText(), /your data/i);
    assert.match(await selection.innerText(), new RegExp(path.basename(nc).replace(/[.]/g, "\\.")));
    assert.match(await selection.innerText(), /Prepared daily maximum temperature \(variable tmax\)/);

    // 5. June to August of a file that holds other months: stated for every selected year.
    await page.getByLabel("First month each year").selectOption("6");
    await page.getByLabel("Last month each year").selectOption("8");
    await page.locator("#period_note").filter({ hasText: "June–August in every selected year: 30 years (1996–2025), 2,760 days." }).waitFor();
    assert.match(await page.locator("#period_note").innerText(), /This file also holds April–May, September\. Those months are not analyzed\./);
    await page.getByLabel("Limit the analysis dates").check();
    const dates = page.locator("#dates input");
    for (const [index, date] of ["2012-01-01", "2012-12-31"].entries()) {
      const field = dates.nth(index);
      await field.fill(date.slice(0, -1));
      await field.press(date.slice(-1));
      await field.press("Tab");
    }
    await page.locator("#period_note").filter({ hasText: "June–August 2012: 92 days." }).waitFor();
    await page.locator("#review").filter({ hasText: "92 selected dates" }).waitFor();

    // 6. Results go to a folder made in the same chooser; nothing is created before Run.
    await button("Choose or create folder").click();
    await chooser.waitFor();
    await goTo(dir);
    await button("New folder").click();
    await page.getByLabel("Name of the new folder").fill("reviewer results");
    await button("Create").click();
    await chooser.locator(".chooser-crumb[aria-current]").filter({ hasText: "reviewer results" }).waitFor();
    await button("Use this folder").click();
    await page.locator("#destination").filter({ hasText: "reviewer results" }).waitFor();
    assert.equal(fs.existsSync(path.join(output, "SCORCH_run_001")), false);

    // 7. Advanced settings stay collapsed until asked for and keep seeds and predictors.
    await page.getByText("Advanced settings", { exact: true }).click();
    for (const label of ["Random seed for daily maxima", "Random seed for event maxima", "Local temperature percentile",
                         "Candidate distances (grid cells)", "Raster cell size (km)"]) {
      assert.equal(await page.getByLabel(label).isVisible(), true, label);
    }
    for (const predictor of ["Longitude", "Latitude", "Mean Tmax", "Tmax standard deviation"]) {
      assert.equal(await page.getByLabel(predictor, { exact: true }).isChecked(), true, predictor);
    }
    await page.getByLabel("Bootstrap repetitions").fill("10");
    await page.getByLabel("Bootstrap repetitions").press("Tab");

    // 8. Run: progress, then the summary in the window and the files on disk.
    await button("Run analysis").click();
    await page.locator(".shiny-notification").filter({ hasText: "Running SCORCH" }).waitFor();
    await button("Open results folder").waitFor({ timeout: 300000 });
    const completion = page.locator("#completion");
    const run = (await completion.locator(".outcome .path").innerText()).trim();
    assert.equal(path.resolve(run), path.join(output, "SCORCH_run_001"));
    assert.match(await completion.locator(".outcome").innerText(), /Analysis complete/);
    assert.match(await completion.locator(".outcome").innerText(), /3 compound events from 8 ellipses on 3 regionally extensive days/);
    assert.deepEqual((await completion.locator(".type-counts li strong").allInnerTexts()).map(Number), [0, 3, 0, 0]);
    assert.deepEqual(await completion.locator(".actions").getByRole("button").allInnerTexts(),
                     ["Open summary", "Open results folder", "Set up another run"]);
    const summary = await completion.locator(".summary").innerText();
    for (const expected of [path.basename(nc), "92 dates from 2012-06-01 to 2012-08-31", "June–August 2012",
                            "10 bootstrap repetitions (seeds 20261020 for daily maxima, 20261120 for event maxima)",
                            "covariates lon, lat, mean_tmax, std_tmax", "95th percentile"]) {
      assert.ok(summary.includes(expected), expected);
    }
    for (const file of ["Summary.txt", "config.yaml", "Heatwave_Statistics.xlsx", "catalogs/05_compound_heatwave_events.xlsx",
                        "modeling/Power_Law_Model.xlsx", "figures/geometry/Ellipses_by_type.png"]) {
      assert.ok(fs.existsSync(path.join(run, file)), file);
    }
    assert.match(fs.readFileSync(path.join(run, "Summary.txt"), "utf8"), /92 dates/);
    assert.match(fs.readFileSync(path.join(run, "config.yaml"), "utf8"), /bootstrap_repetitions: 10/);

    // The map now shows the events of this run, not the demonstration.
    const mine = page.locator("#explorer-run");
    await mine.locator("svg.map").first().waitFor();
    assert.match(await mine.locator(".explorer-head").innerText(), /this run/i);
    assert.doesNotMatch(await mine.locator(".explorer-head").innerText(), /demonstration/i);
    assert.match(await mine.locator(".explorer-head").innerText(), new RegExp(path.basename(nc).replace(/[.]/g, "\\.")));
    assert.match(await mine.locator(".type-cards").innerText(), /3 events over 3 days/);
    assert.equal(await mine.locator(".type-card[disabled]").count(), 3);
    assert.equal(await mine.locator(".map-day").innerText(), "Single-day event");
    assert.equal(await mine.locator(".event-pick option").count(), 3);
    const shown = await mine.locator(".map-date").innerText();
    await mine.locator(".event-pick select").selectOption({ index: 2 });
    await page.waitForFunction(day => document.querySelector("#explorer-run .map-date").textContent !== day, shown);
    assert.ok(await mine.locator(".map .frame .day:not(.leaving) .ellipse").count() >= 2);
    await shot("results", { fullPage: true });

    // 9. Another run gets its own folder; saved settings can be reused on other data.
    await button("Set up another run").click();
    await page.locator("#destination").filter({ hasText: "SCORCH_run_002" }).waitFor();
    await page.getByLabel("First month each year").selectOption("4");
    await button("Reuse a saved configuration").click();
    await chooser.waitFor();
    await goTo(run, "config.yaml");
    await chooser.locator(".chooser-row").filter({ hasText: "config.yaml" }).first().dblclick();
    await page.locator("#review").filter({ hasText: "92 selected dates" }).waitFor();
    assert.equal(await page.getByLabel("First month each year").inputValue(), "6");
    assert.match(await page.locator("#loaded").innerText(), /Settings loaded from config\.yaml/);
    await button("Change data").click();
    await hero.waitFor();
    await button("Choose your own data").click();
    await chooser.waitFor();
    await chooser.locator("#chooser_path").fill(path.join(dir, "temperatures.zip"));
    await chooser.locator("#chooser_path").press("Enter");
    await selection.filter({ hasText: "temperatures.zip" }).waitFor();
    await page.getByLabel("First month each year").selectOption("6");
    await page.getByLabel("Last month each year").selectOption("8");
    await page.getByLabel("Limit the analysis dates").check();
    for (const [index, date] of ["2012-01-01", "2012-12-31"].entries()) {
      const field = page.locator("#dates input").nth(index);
      await field.fill(date.slice(0, -1));
      await field.press(date.slice(-1));
      await field.press("Tab");
    }
    await page.getByText("Advanced settings", { exact: true }).click();
    await page.getByLabel("Bootstrap repetitions").fill("10");
    await page.getByLabel("Bootstrap repetitions").press("Tab");
    await page.locator("#review").filter({ hasText: "92 selected dates" }).waitFor();
    await button("Run analysis").click();
    await button("Open results folder").waitFor({ timeout: 300000 });
    const next = (await completion.locator(".outcome .path").innerText()).trim();
    assert.equal(path.resolve(next), path.join(output, "SCORCH_run_002"));
    assert.match(fs.readFileSync(path.join(next, "config.yaml"), "utf8"), /temperatures[.]zip/);
    assert.match(fs.readFileSync(path.join(next, "Summary.txt"), "utf8"), /92 dates/);
    assert.ok(fs.existsSync(path.join(run, "config.yaml")));
    // The ZIP and the file it holds give the same catalog of events.
    const events = folder => fs.readFileSync(path.join(folder, "Summary.txt"), "utf8").split(/\r?\n/)
      .filter(line => /^ {2}Stage \d|^ {11}Type \d/.test(line));
    assert.deepEqual(events(next), events(run));
    assert.ok(events(run).length >= 9);
    assert.deepEqual(errors, []);
    await button("Close").click();
    console.log("Opening screen, demonstration map, file chooser, seasonal settings, two runs, results summary and saved settings passed.");
  } catch (error) {
    if (page && !page.isClosed()) {
      if (process.env.SCORCH_UI_SHOTS) await page.screenshot({ path: path.join(process.env.SCORCH_UI_SHOTS, "failure.png"), fullPage: true }).catch(() => {});
      console.error(await page.locator("body").innerText({ timeout: 3000 }).catch(() => ""));
    }
    throw error;
  } finally { await browser.close(); }
}
run().catch(error => { console.error(error); process.exitCode = 1; });
