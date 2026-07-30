const { test, expect } = require("@playwright/test");

const SAMPLE_NAMES = [
  "newfrat_enaset.Rdata",
  "sample_enaset.Rdata",
  "student_enaset.RData",
];
const soakMinutes = Number(process.env.ENA3D_AUDIT_SOAK_MINUTES || "30");
if (!Number.isFinite(soakMinutes) || soakMinutes <= 0 || soakMinutes > 1440) {
  throw new Error("ENA3D_AUDIT_SOAK_MINUTES must be greater than 0 and at most 1440.");
}
const soakDurationMs = Math.ceil(soakMinutes * 60_000);

test.beforeEach(async ({ page }) => {
  await page.route("**/_vercel/insights/script.js", (route) =>
    route.fulfill({
      status: 200,
      contentType: "application/javascript",
      body: "// Vercel Web Analytics test stub.\n",
    })
  );
});

function captureBrowserErrors(page) {
  const errors = [];
  page.on("console", (message) => {
    if (message.type() === "error") {
      errors.push(`console.error: ${message.text()}`);
    }
  });
  page.on("pageerror", (error) => {
    errors.push(`pageerror: ${error.stack || error.message}`);
  });
  return errors;
}

async function waitForShinyIdle(page) {
  await expect(page.locator("html")).not.toHaveClass(/shiny-busy/, {
    timeout: 30_000,
  });
  await page.waitForTimeout(300);
}

async function selectSample(page, sampleName) {
  const activeName = page.locator(".active-dataset-card strong");
  if ((await activeName.count()) && (await activeName.innerText()) === sampleName) {
    return;
  }
  const selector = page.getByRole("combobox", { name: "Trusted sample dataset" });
  await page.waitForFunction(
    (name) => {
      const select = document.querySelector("#main_app-sample_data");
      return Boolean(select?.selectize?.options?.[name]);
    },
    sampleName,
    { timeout: 15_000 }
  );
  await selector.click();
  await page.getByRole("option", { name: sampleName, exact: true }).click();
  await expect(activeName).toHaveText(sampleName, { timeout: 30_000 });
  await waitForShinyIdle(page);
}

async function openTrajectory(page) {
  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  await expect(modelTab).toHaveAttribute("aria-selected", "true");
  const trajectoryTab = page.getByRole("tab", { name: "Trajectory", exact: true });
  await trajectoryTab.click();
  await expect(trajectoryTab).toHaveAttribute("aria-selected", "true");
  await expect(
    page.getByRole("combobox", { name: "Time / order variable" })
  ).toBeVisible({ timeout: 30_000 });
  const isLongitudinal = await selectRepeatedTrajectoryId(page);
  await waitForShinyIdle(page);
  return isLongitudinal;
}

async function selectRepeatedTrajectoryId(page) {
  const coverage = page.locator("#main_app-trajectory-id_coverage_status");
  await expect(coverage).not.toBeEmpty({ timeout: 30_000 });
  if ((await coverage.innerText()).startsWith("0 of ")) {
    await page
      .getByRole("combobox", { name: "Entity ID (repeated unit)" })
      .click();
    const repeatedOption = page
      .getByRole("option", {
        name: /[1-9][0-9]*\/[0-9]+ repeated ID profiles/,
      })
      .first();
    if ((await repeatedOption.count()) === 0) {
      await page.keyboard.press("Escape");
      await expect(coverage).toContainText("cross-sectional only");
      return false;
    }
    await repeatedOption.click();
  }
  await expect(coverage).toContainText(
    /[1-9][0-9]* of [0-9]+ ID profiles repeat across time/,
    { timeout: 30_000 }
  );
  return true;
}

test("load, switch, calculate, and cancel soak remains responsive", async ({
  page,
  request,
}, testInfo) => {
  test.skip(
    process.env.ENA3D_AUDIT_SOAK !== "1",
    "Set ENA3D_AUDIT_SOAK=1 to run the pre-release soak audit."
  );
  test.setTimeout(soakDurationMs + 300_000);
  const browserErrors = captureBrowserErrors(page);

  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await page
    .getByRole("button", { name: "Open the 3D ENA research workspace" })
    .click();
  await selectSample(page, SAMPLE_NAMES[0]);
  expect(await openTrajectory(page)).toBe(true);

  // Exercise the asynchronous cancellation path once before the steady-state
  // cycles. Changing a bootstrap setting invalidates and cancels the active
  // isolated worker without requiring a dedicated Cancel button.
  const uncertainty = page.locator("#main_app-trajectory-show_uncertainty");
  await uncertainty.check();
  await page.locator("#main_app-trajectory-bootstrap_reps").fill("200");
  await page
    .getByRole("button", { name: "Run / recompute trajectory" })
    .click();
  const trajectoryStatus = page.locator("#main_app-trajectory-status");
  await expect(trajectoryStatus).toContainText("Running centroid trajectory analysis", {
    timeout: 30_000,
  });
  await page.waitForTimeout(1_000);
  await page.locator("#main_app-trajectory-bootstrap_reps").fill("250");
  await expect(trajectoryStatus).toContainText("Trajectory settings changed", {
    timeout: 30_000,
  });
  await uncertainty.uncheck();

  const deadline = Date.now() + soakDurationMs;
  let iteration = 0;
  do {
    const dataTab = page.getByRole("tab", { name: "Data", exact: true });
    await dataTab.click();
    await selectSample(page, SAMPLE_NAMES[iteration % SAMPLE_NAMES.length]);
    const isLongitudinal = await openTrajectory(page);

    await page
      .getByRole("button", { name: "Run / recompute trajectory" })
      .click();
    if (isLongitudinal) {
      await expect(trajectoryStatus).toContainText("Completed", {
        timeout: 60_000,
      });
      await expect(
        page.locator("#main_app-trajectory-trajectory_plot.js-plotly-plot")
      ).toBeVisible({ timeout: 30_000 });
    } else {
      await expect(trajectoryStatus).toContainText("cross-sectional only", {
        timeout: 30_000,
      });
    }

    const statsTab = page.getByRole("tab", { name: "Stats", exact: true });
    await statsTab.click();
    await expect(statsTab).toHaveAttribute("aria-selected", "true");
    await expect(page.locator(".stats-panel")).toBeVisible();
    expect(await openTrajectory(page)).toBe(isLongitudinal);
    await expect(trajectoryStatus).toContainText(
      isLongitudinal ? "Completed" : "cross-sectional only"
    );

    const health = await request.get("/ena3d-health/healthz.json", {
      timeout: 10_000,
      // Long-lived APIRequestContext connections can be reset by the local
      // HTTP stack even while the Shiny page remains live. Playwright retries
      // only ECONNRESET; persistent failure and every non-OK response still
      // fail this health gate.
      maxRetries: 2,
    });
    expect(health.ok()).toBeTruthy();
    expect(await health.json()).toMatchObject({ status: "ok", ai_enabled: false });
    iteration += 1;
  } while (Date.now() < deadline);

  await testInfo.attach("soak-summary.json", {
    body: Buffer.from(
      JSON.stringify({ duration_minutes: soakMinutes, iterations: iteration }, null, 2)
    ),
    contentType: "application/json",
  });
  expect(
    browserErrors,
    `Unexpected browser errors during soak:\n${browserErrors.join("\n")}`
  ).toEqual([]);
});
