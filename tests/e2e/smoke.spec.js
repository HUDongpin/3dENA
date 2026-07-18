const { test, expect } = require("@playwright/test");
const path = require("node:path");

const SAMPLE_NAME = "newfrat_enaset.Rdata";
const EXCHANGE_FIXTURE = path.join(
  __dirname,
  "fixtures",
  "small-valid.ena3d.json"
);

function captureBrowserErrors(page) {
  const messages = [];

  page.on("console", (message) => {
    if (message.type() !== "error") return;
    const location = message.location();
    messages.push({
      kind: "console.error",
      text: message.text(),
      url: location.url || "",
      line: location.lineNumber,
      column: location.columnNumber,
    });
  });

  page.on("pageerror", (error) => {
    messages.push({
      kind: "pageerror",
      text: error.stack || error.message,
      url: "",
      line: 0,
      column: 0,
    });
  });

  return messages;
}

async function waitForShinyIdle(page) {
  await expect(page.locator("html")).not.toHaveClass(/shiny-busy/, {
    timeout: 30_000,
  });
  // Some inactive Shiny outputs remain marked recalculating until their tab is
  // first shown. The document busy state plus a short event-loop settle is the
  // reliable readiness signal for this tabbed application.
  await page.waitForTimeout(500);
}

async function selectTrustedSample(page) {
  const sample = page.getByRole("combobox", {
    name: "Trusted sample dataset",
  });
  await expect(sample).toBeVisible();
  // Selectize keeps unselected choices in its option store rather than as
  // native <option> children. Wait for the Shiny update before opening it.
  await page.waitForFunction(
    (sampleName) => {
      const select = document.querySelector("#main_app-sample_data");
      return Boolean(select?.selectize?.options?.[sampleName]);
    },
    SAMPLE_NAME,
    { timeout: 15_000 }
  );
  await sample.click();
  await page.getByRole("option", { name: SAMPLE_NAME, exact: true }).click();
  await expect(page.getByRole("heading", { name: "Active dataset" })).toBeVisible({
    timeout: 30_000,
  });
  await expect(page.getByRole("status").filter({ hasText: SAMPLE_NAME })).toBeVisible();
  await waitForShinyIdle(page);
}

async function openModelTab(page, name, target) {
  const tab = page.getByRole("tab", { name, exact: true });
  await tab.click();
  await expect(tab).toHaveAttribute("aria-selected", "true");
  await expect(target).toBeVisible({ timeout: 30_000 });
  await waitForShinyIdle(page);
}

test("home foregrounds trajectory analysis in a compact responsive hero", async ({
  page,
}, testInfo) => {
  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);

  const home = page.locator(".ena3d-home-page");
  const hero = page.locator(".ena3d-hero");
  const heading = hero.getByRole("heading", {
    name: "Make epistemic connections visible in three dimensions.",
  });
  const visual = page.locator(".ena3d-hero-visual");

  await expect(home).toBeVisible();
  await expect(heading).toBeVisible();
  await expect(
    page.getByRole("heading", { name: "Follow change through time" })
  ).toBeVisible();
  await expect(visual).toContainText("TRAJECTORY ANALYSIS");
  await expect(visual).toContainText("Ordered nodes");
  await expect(visual).toContainText("Direction");
  await expect(visual).toContainText("Uncertainty");
  await expect(
    visual.getByRole("img", {
      name: /three-dimensional centroid trajectory/i,
    })
  ).toBeVisible();

  const measurements = await page.evaluate(() => {
    const heroElement = document.querySelector(".ena3d-hero");
    const headingElement = document.querySelector(".ena3d-hero h1");
    const visualElement = document.querySelector(".ena3d-hero-visual");
    const headingStyle = getComputedStyle(headingElement);
    const lineHeight = Number.parseFloat(headingStyle.lineHeight);
    return {
      heroHeight: heroElement.getBoundingClientRect().height,
      headingLines: headingElement.getBoundingClientRect().height / lineHeight,
      visualBackground: getComputedStyle(visualElement).backgroundImage,
      viewportWidth: window.innerWidth,
      documentWidth: document.documentElement.scrollWidth,
    };
  });

  expect(measurements.documentWidth).toBeLessThanOrEqual(
    measurements.viewportWidth
  );
  expect(measurements.visualBackground).toContain("linear-gradient");
  if (testInfo.project.name === "desktop-chromium") {
    expect(measurements.heroHeight).toBeLessThanOrEqual(700);
    expect(measurements.headingLines).toBeLessThanOrEqual(3.1);
  } else {
    expect(measurements.headingLines).toBeLessThanOrEqual(4.1);
  }

  if (process.env.CAPTURE_HOME === "1") {
    await page.screenshot({
      path: path.join(
        __dirname,
        "..",
        "..",
        "output",
        "playwright",
        `home-${testInfo.project.name}.png`
      ),
      fullPage: false,
    });
  }

  await page
    .getByRole("button", {
      name: "Open the centroid trajectory analysis workspace",
    })
    .click();
  await expect(page.locator('#site_nav a[data-value="tool"]')).toHaveAttribute(
    "aria-selected",
    "true"
  );
  await expect(
    page.locator('#workspace_sections a[data-value="Model"]')
  ).toHaveAttribute("aria-selected", "true");
  await expect(page.locator('#main_app-mytabs a[data-value="trajectory"]')).toHaveAttribute(
    "aria-selected",
    "true"
  );
});

test("trusted sample traverses every model view and trajectory controls", async ({
  page,
  request,
}, testInfo) => {
  const browserErrors = captureBrowserErrors(page);

  const health = await request.get("/ena3d-health/healthz.json", {
    timeout: 10_000,
  });
  expect(health.ok()).toBeTruthy();
  expect(await health.json()).toMatchObject({ status: "ok", app: "ENA 3D" });

  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await page
    .getByRole("button", { name: "Open the 3D ENA research workspace" })
    .click();
  await expect(page.getByRole("heading", { name: "ENA 3D", exact: true })).toBeVisible();
  const typography = await page.evaluate(() => {
    const fontSize = (selector) =>
      Number.parseFloat(getComputedStyle(document.querySelector(selector)).fontSize);
    return {
      root: fontSize("html"),
      navigation: fontSize(".navbar-nav a"),
      workspaceTab: fontSize(".mysidebar .left-side .nav a"),
      workspaceBody: fontSize(".raw-import-workflow"),
      formLabel: fontSize(".raw-import-workflow .form-group label"),
    };
  });
  expect(typography.root).toBeGreaterThanOrEqual(16);
  expect(typography.navigation).toBeGreaterThanOrEqual(
    testInfo.project.name === "mobile-390px-chromium" ? 16 : 15
  );
  expect(typography.workspaceTab).toBeGreaterThanOrEqual(15);
  expect(typography.workspaceBody).toBeGreaterThanOrEqual(16);
  expect(typography.formLabel).toBeGreaterThanOrEqual(16);
  await expect(page.getByRole("tab", { name: "Data", exact: true })).toHaveAttribute(
    "aria-selected",
    "true"
  );
  await expect(page.locator(".data-security-notice")).toHaveCount(0);

  const rawUpload = page.locator('input[type="file"][accept=".csv,.xlsx,.xls"]');
  await expect(rawUpload).toHaveCount(1);
  const exchangeUpload = page.locator('input[type="file"][accept=".ena3d.json"]');
  await expect(exchangeUpload).toHaveCount(1);
  await expect(exchangeUpload).toHaveAttribute("accept", ".ena3d.json");
  const acceptedExtensions = (await exchangeUpload.getAttribute("accept"))
    .split(",")
    .map((value) => value.trim().toLowerCase())
    .filter(Boolean);
  expect(acceptedExtensions).toEqual([".ena3d.json"]);
  expect(acceptedExtensions.some((value) => /rdata|rds|rda/.test(value))).toBeFalsy();

  // High-value accessibility contracts used by keyboard and assistive-tech users.
  await expect(page.getByRole("radiogroup", { name: "Camera Position:" })).toBeVisible();
  await expect(page.getByRole("button", { name: "Hide ENA controls" })).toHaveAttribute(
    "aria-expanded",
    "true"
  );
  await expect(
    page.getByRole("button", { name: "Enter fullscreen for the visible ENA plot" })
  ).toBeVisible();

  await selectTrustedSample(page);

  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  await expect(modelTab).toHaveAttribute("aria-selected", "true");

  await openModelTab(page, "Overall", page.locator("#main_app-group_colors_container"));

  await openModelTab(
    page,
    "Networks",
    page.getByRole("combobox", { name: "Show Network" })
  );
  await expect(page.locator("#main_app-network_selector")).toHaveValue(
    "ena3d-network-v1:none"
  );
  await expect(page.getByRole("combobox", { name: "Show Network" })).toBeVisible();

  await openModelTab(
    page,
    "Comparison",
    page.getByRole("combobox", { name: "Group 1", exact: true })
  );
  await expect(page.locator("#main_app-compare_group_2")).toBeAttached();

  await openModelTab(
    page,
    "Change",
    page.getByRole("combobox", { name: "Select Group Variable" })
  );
  await expect(page.locator("#main_app-unit_change")).toBeAttached();

  await openModelTab(
    page,
    "Trajectory",
    page.getByRole("combobox", { name: "Time / order variable" })
  );
  await expect(page.getByRole("heading", { name: "Centroid trajectory" })).toBeVisible();
  await expect(
    page.getByRole("combobox", { name: "Entity ID (repeated unit)" })
  ).toBeVisible();
  await expect(
    page.getByRole("combobox", { name: "Group / condition (optional)" })
  ).toBeVisible();
  await expect(page.locator("#main_app-trajectory-id_coverage_status")).not.toBeEmpty();
  await expect(
    page.getByRole("note").filter({ hasText: "Plot Tools scope" })
  ).toBeVisible();
  const directionArrows = page.locator("#main_app-trajectory-show_direction");
  await expect(directionArrows).toBeChecked();

  const uncertainty = page.locator("#main_app-trajectory-show_uncertainty");
  await uncertainty.check();
  const bootstrapReps = page.locator("#main_app-trajectory-bootstrap_reps");
  await expect(bootstrapReps).toBeVisible();
  await expect(bootstrapReps).toHaveAttribute("max", "500");
  await expect(bootstrapReps).toHaveAttribute("min", "200");
  await expect(
    page.getByRole("combobox", { name: "Participant resampling design" })
  ).toBeVisible();

  // Run the real analysis without a bootstrap so this smoke path exercises the
  // Shiny computation/plot/download lifecycle without making CI needlessly
  // expensive. Statistical bootstrap contracts are covered by the R suites.
  await uncertainty.uncheck();
  const runTrajectory = page.getByRole("button", {
    name: "Run / recompute trajectory",
  });
  await runTrajectory.click();
  await expect(page.locator("#main_app-trajectory-status")).toContainText(
    "Completed",
    { timeout: 60_000 }
  );
  await expect(
    page.locator("#main_app-trajectory-trajectory_plot.js-plotly-plot")
  ).toBeVisible({ timeout: 30_000 });
  const nodeLegend = page.locator(
    "#main_app-trajectory-node_legend .trajectory-node-legend"
  );
  await expect(nodeLegend).toBeVisible();
  await expect(nodeLegend.getByRole("heading", { name: "Trajectory nodes" })).toBeVisible();
  await expect(nodeLegend).toContainText("Ordered period \u00b7 Week");
  const nodeLegendItems = nodeLegend.locator(".trajectory-node-legend-item");
  await expect(nodeLegendItems).toHaveCount(15);
  const nodeLegendSummary = await nodeLegendItems.evaluateAll((items) =>
    items.map((item) => ({
      key: item.dataset.nodeKey,
      color: item.dataset.nodeColor,
      label: item.querySelector(".trajectory-node-legend-label")?.textContent?.trim(),
    }))
  );
  expect(new Set(nodeLegendSummary.map((item) => item.key)).size).toBe(15);
  expect(new Set(nodeLegendSummary.map((item) => item.color)).size).toBe(15);
  expect(nodeLegendSummary[0].label).toBe("Order 1 \u00b7 0");
  expect(nodeLegendSummary[14].label).toBe("Order 15 \u00b7 14");

  const directionTraceSummary = await page
    .locator("#main_app-trajectory-trajectory_plot")
    .evaluate((plot) => {
      const traces = plot.data || [];
      const arrows = traces.filter(
        (trace) => trace.meta?.trajectory_role === "direction_arrows"
      );
      const paths = traces.filter(
        (trace) => trace.meta?.trajectory_role === "path"
      );
      const nodeMarkers = traces.filter(
        (trace) => trace.meta?.trajectory_role === "node_markers"
      );
      const samePoint = (left, right) =>
        ["x", "y", "z"].every(
          (axis) => Math.abs(Number(left[axis]) - Number(right[axis])) < 1e-12
        );
      const pointAt = (trace, index) => ({
        x: trace.x[index],
        y: trace.y[index],
        z: trace.z[index],
      });
      const wingRuns = arrows.map((trace) => {
        const runs = [];
        let run = [];
        for (let index = 0; index < trace.x.length; index += 1) {
          if (trace.x[index] == null) {
            if (run.length) runs.push(run);
            run = [];
          } else {
            run.push(pointAt(trace, index));
          }
        }
        if (run.length) runs.push(run);
        return runs;
      });
      const tipsReachDestinationCenters = arrows.every((arrow, arrowIndex) => {
        const path = paths.find(
          (candidate) =>
            candidate.meta.trajectory_key === arrow.meta.trajectory_key
        );
        const runs = wingRuns[arrowIndex];
        return (
          path &&
          runs.length === 2 * arrow.meta.segment_count &&
          runs.every((run) => run.length === 2) &&
          Array.from({ length: arrow.meta.segment_count }, (_, segment) => {
            const firstTip = runs[2 * segment][1];
            const secondTip = runs[2 * segment + 1][1];
            const destination = pointAt(path, segment + 1);
            return (
              samePoint(firstTip, secondTip) &&
              samePoint(firstTip, destination)
            );
          }).every(Boolean)
        );
      });
      const markersCoverArrows = arrows.every((arrow) => {
        const marker = nodeMarkers.find(
          (candidate) =>
            candidate.meta.trajectory_key === arrow.meta.trajectory_key
        );
        return marker && traces.indexOf(marker) > traces.indexOf(arrow);
      });
      const nodeColorsMatch = paths.every((path) => {
        const marker = nodeMarkers.find(
          (candidate) =>
            candidate.meta.trajectory_key === path.meta.trajectory_key
        );
        return (
          marker &&
          JSON.stringify(marker.marker.color) === JSON.stringify(path.marker.color)
        );
      });
      const uniqueNodeColors = new Set(
        paths.flatMap((path) => Array.from(path.marker.color || []))
      ).size;
      const relativeLuminance = (hexColor) => {
        const channels = String(hexColor)
          .slice(1)
          .match(/../g)
          .map((channel) => Number.parseInt(channel, 16) / 255)
          .map((channel) =>
            channel <= 0.04045
              ? channel / 12.92
              : ((channel + 0.055) / 1.055) ** 2.4
          );
        return (
          0.2126 * channels[0] +
          0.7152 * channels[1] +
          0.0722 * channels[2]
        );
      };
      const nodeColors = [
        ...new Set(paths.flatMap((path) => Array.from(path.marker.color || []))),
      ];
      const hoverlabel = plot._fullLayout?.hoverlabel || {};
      return {
        traceCount: arrows.length,
        segmentCounts: arrows.map((trace) => trace.meta.segment_count),
        wingCounts: wingRuns.map((runs) => runs.length),
        legendEntries: arrows.filter((trace) => trace.showlegend !== false).length,
        nodeMarkerCount: nodeMarkers.length,
        tipsReachDestinationCenters,
        markersCoverArrows,
        nodeColorsMatch,
        uniqueNodeColors,
        minimumNodeLuminance: Math.min(...nodeColors.map(relativeLuminance)),
        hoverLabel: {
          bgcolor: hoverlabel.bgcolor,
          bordercolor: hoverlabel.bordercolor,
          align: hoverlabel.align,
          fontColor: hoverlabel.font?.color,
        },
      };
    });
  expect(directionTraceSummary).toMatchObject({
    traceCount: 1,
    segmentCounts: [14],
    wingCounts: [28],
    legendEntries: 0,
    nodeMarkerCount: 1,
    tipsReachDestinationCenters: true,
    markersCoverArrows: true,
    nodeColorsMatch: true,
    uniqueNodeColors: 15,
    hoverLabel: {
      bgcolor: "#FFFFFF",
      bordercolor: "#526777",
      align: "left",
      fontColor: "#102A43",
    },
  });
  expect(directionTraceSummary.minimumNodeLuminance).toBeGreaterThanOrEqual(0.16);

  const plotBox = await page
    .locator("#main_app-trajectory-trajectory_plot")
    .boundingBox();
  const legendBox = await nodeLegend.boundingBox();
  expect(plotBox).not.toBeNull();
  expect(legendBox).not.toBeNull();
  const viewportWidth = page.viewportSize().width;
  const stackedLegend = [
    "tablet-768px-chromium",
    "mobile-390px-chromium",
  ].includes(testInfo.project.name);
  if (stackedLegend) {
    expect(legendBox.y).toBeGreaterThanOrEqual(plotBox.y + plotBox.height - 1);
    expect(legendBox.x + legendBox.width).toBeLessThanOrEqual(viewportWidth + 1);
  } else {
    expect(legendBox.x).toBeGreaterThanOrEqual(plotBox.x + plotBox.width - 1);
    expect(legendBox.y).toBeGreaterThanOrEqual(plotBox.y - 1);
  }
  const documentWidths = await page.evaluate(() => ({
    client: document.documentElement.clientWidth,
    scroll: document.documentElement.scrollWidth,
  }));
  expect(documentWidths.scroll).toBeLessThanOrEqual(documentWidths.client + 1);
  const trajectoryDownloads = page.locator(".trajectory-downloads");
  await expect(trajectoryDownloads).toBeVisible();
  await expect(trajectoryDownloads).toContainText("Analysis bundle ZIP");
  await expect(trajectoryDownloads).toContainText("Path CSV");
  await expect(trajectoryDownloads).toContainText("Metadata CSV");

  if (testInfo.project.name === "mobile-390px-chromium") {
    expect(await page.evaluate(() => window.innerWidth)).toBe(390);
    await expect(page.getByRole("tablist").first()).toBeVisible();
  } else {
    const dataTab = page.getByRole("tab", { name: "Data", exact: true });
    await dataTab.click();
    await expect(dataTab).toHaveAttribute("aria-selected", "true");
    await exchangeUpload.setInputFiles(EXCHANGE_FIXTURE);
    await expect(
      page.getByRole("status").filter({ hasText: "small-valid.ena3d.json" })
    ).toBeVisible({ timeout: 30_000 });
    await waitForShinyIdle(page);
  }

  if (browserErrors.length) {
    await testInfo.attach("browser-console-errors.json", {
      body: Buffer.from(JSON.stringify(browserErrors, null, 2)),
      contentType: "application/json",
    });
  }
  expect(
    browserErrors,
    `Unexpected browser errors:\n${JSON.stringify(browserErrors, null, 2)}`
  ).toEqual([]);
});
