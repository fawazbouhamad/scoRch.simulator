// Exercise the installed RStudio entry point in a real local Shiny session.
const { chromium } = require("playwright-core");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

async function run() {
  const browser = await chromium.launch({ executablePath: process.env.CHROME_BIN, headless: true });
  try {
    const page = await browser.newPage({ viewport: { width: 940, height: 780 } });
    const errors = [];
    page.on("pageerror", error => errors.push(String(error)));
    page.setDefaultTimeout(30000);
    const button = name => page.getByRole("button", { name, exact: true });
    async function goTo(folder, expected) {
      await page.getByLabel("Current folder").fill(folder);
      await button("Go").click();
      await page.locator("#pick_entry option").filter({ hasText: expected }).first().waitFor({ state: "attached" });
    }
    const dir = process.env.SCORCH_UI_PROJECT;
    const nc = process.env.SCORCH_UI_DATA;
    const output = path.join(dir, "reviewer results");
    fs.mkdirSync(path.join(output, "keep"), { recursive: true });
    await page.goto("http://127.0.0.1:7123");
    await page.waitForFunction(() => window.Shiny?.shinyapp?.isConnected());
    assert.equal(await page.locator("#start:visible").count(), 0);
    await button("Use included example").click();
    await page.locator("#start:visible").waitFor();
    assert.match(await page.locator("#selection").innerText(), /Included North America example/);
    await button("Choose NetCDF or ZIP").click();
    await goTo(path.dirname(nc), path.basename(nc));
    await page.locator("#pick_entry").selectOption({ label: path.basename(nc) });
    await button("Open selected item").click();
    await page.locator("#start:visible").waitFor();
    await page.getByLabel("First month each year").selectOption("6");
    await page.getByLabel("Last month each year").selectOption("8");
    await page.getByLabel("Limit the analysis dates").check();
    const dates = page.locator("#dates input");
    for (const [index, date] of ["2012-01-01", "2012-12-31"].entries()) {
      const field = dates.nth(index);
      await field.fill(date.slice(0, -1));
      await field.press(date.slice(-1));
      await field.press("Tab");
    }
    await page.locator("#review").filter({ hasText: "92 selected dates" }).waitFor();
    await button("Choose folder").click();
    await goTo(output, "keep/");
    await button("Select current folder").click();
    await page.getByText("Advanced analysis settings", { exact: true }).click();
    await page.getByLabel("Bootstrap repetitions").fill("10");
    await page.getByLabel("Bootstrap repetitions").press("Tab");
    await page.locator("#destination").filter({ hasText: "reviewer results" }).waitFor();
    assert.equal(fs.existsSync(path.join(output, "SCORCH_run_001")), false);
    await button("Run analysis").click();
    await button("Open results folder").waitFor({ timeout: 240000 });
    const run = (await page.locator("#completion .path").innerText()).trim();
    assert.equal(path.resolve(run), path.join(output, "SCORCH_run_001"));
    assert.ok(fs.existsSync(path.join(run, "catalogs", "05_compound_heatwave_events.xlsx")));
    assert.match(fs.readFileSync(path.join(run, "Summary.txt"), "utf8"), /92 dates/);
    assert.match(fs.readFileSync(path.join(run, "config.yaml"), "utf8"), /bootstrap_repetitions: 10/);
    await button("Set up another run").click();
    await page.locator("#destination").filter({ hasText: "SCORCH_run_002" }).waitFor();
    await page.getByLabel("First month each year").selectOption("4");
    await button("Load saved settings").click();
    await goTo(run, "config.yaml");
    await page.locator("#pick_entry").selectOption({ label: "config.yaml" });
    await button("Open selected item").click();
    await page.locator("#review").filter({ hasText: "92 selected dates" }).waitFor();
    assert.equal(await page.getByLabel("First month each year").inputValue(), "6");
    await button("Choose NetCDF or ZIP").click();
    await goTo(dir, "temperatures.zip");
    await page.locator("#pick_entry").selectOption({ label: "temperatures.zip" });
    await button("Open selected item").click();
    await page.locator("#selection").filter({ hasText: "temperatures.zip" }).waitFor();
    await button("Run analysis").click();
    await button("Open results folder").waitFor({ timeout: 240000 });
    const next = (await page.locator("#completion .path").innerText()).trim();
    assert.equal(path.resolve(next), path.join(output, "SCORCH_run_002"));
    assert.match(fs.readFileSync(path.join(next, "config.yaml"), "utf8"), /temperatures[.]zip/);
    assert.match(fs.readFileSync(path.join(next, "Summary.txt"), "utf8"), /92 dates/);
    assert.ok(fs.existsSync(path.join(run, "config.yaml")));
    assert.deepEqual(errors, []);
    await button("Close").click();
    console.log("Guided file selection, seasonal review, completed run, new destination and YAML reload passed.");
  } finally { await browser.close(); }
}
run().catch(error => { console.error(error); process.exitCode = 1; });
