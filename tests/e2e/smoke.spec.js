const { test, expect } = require("@playwright/test");
const path = require("node:path");
const { strFromU8, unzipSync } = require("fflate");

const SAMPLE_NAME = "newfrat_enaset.Rdata";
const EXCHANGE_FIXTURE = path.join(
  __dirname,
  "fixtures",
  "small-valid.ena3d.json"
);
const INVALID_EXCHANGE_FIXTURE = path.join(
  __dirname,
  "fixtures",
  "duplicate-format.ena3d.json"
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

async function captureDownload(page, locator) {
  await expect(locator).toBeVisible();
  const href = await locator.getAttribute("href");
  expect(href).toBeTruthy();
  // Request through the browser context so Shiny's session cookies and the
  // session-scoped download URL are both preserved. This avoids navigation
  // differences between attachment handling in Chromium, Firefox, and WebKit
  // while still exercising the exact href exposed to the user.
  const response = await page.context().request.get(new URL(href, page.url()).href);
  expect(response.ok()).toBeTruthy();
  const disposition = response.headers()["content-disposition"] || "";
  const encodedName = disposition.match(/filename\*=UTF-8''([^;]+)/i)?.[1];
  const ordinaryName = disposition.match(/filename="?([^";]+)"?/i)?.[1];
  return {
    bytes: await response.body(),
    filename: encodedName
      ? decodeURIComponent(encodedName)
      : ordinaryName || path.basename(new URL(href, page.url()).pathname),
  };
}

function normalizedText(bytes) {
  return bytes.toString("utf8").replace(/^\uFEFF/, "").replace(/\r\n/g, "\n");
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

  await expect(
    page.locator('script[src="/_vercel/insights/script.js"]')
  ).toHaveCount(0);

  await expect(home).toBeVisible();
  await expect(heading).toBeVisible();
  await expect(
    page.getByRole("heading", { name: "Follow change through time" })
  ).toBeVisible();
  await expect(visual).toContainText("TRAJECTORY ANALYSIS");
  await expect(visual).not.toContainText("Ordered nodes");
  await expect(visual).not.toContainText("Direction");
  await expect(visual).not.toContainText("Group comparison");
  await expect(visual.locator(".ena3d-trajectory-key")).toHaveCount(0);
  const trajectoryPreview = visual.getByRole("img", {
    name: /three-dimensional ENA visualization/i,
  });
  await expect(trajectoryPreview).toBeVisible();
  await expect(trajectoryPreview).toHaveAttribute(
    "src",
    "ena3d-assets/trajectory-home-preview-3d.png"
  );
  await expect
    .poll(() =>
      trajectoryPreview.evaluate(
        (image) => ({
          complete: image.complete,
          naturalWidth: image.naturalWidth,
          naturalHeight: image.naturalHeight,
        })
      )
    )
    .toEqual({ complete: true, naturalWidth: 1446, naturalHeight: 1310 });
  await expect(page.locator("#home_trajectory_plot")).toHaveCount(0);

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
  if (testInfo.project.name.startsWith("desktop-")) {
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
  await expect(page).toHaveURL((url) => url.pathname === "/app");
  await expect(
    page.locator('#workspace_sections a[data-value="Model"]')
  ).toHaveAttribute("aria-selected", "true");
  await expect(page.locator('#main_app-mytabs a[data-value="trajectory"]')).toHaveAttribute(
    "aria-selected",
    "true"
  );
});

test("papers page exposes three verified copy-ready APA citations", async ({
  browserName,
  page,
  context,
}) => {
  if (browserName === "chromium") {
    await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  }
  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);

  const papersTab = page.locator('#site_nav a[data-value="papers"]');
  if (!(await papersTab.isVisible())) {
    await page.getByRole("button", { name: "Toggle navigation" }).click();
  }
  await papersTab.click();
  await expect(papersTab).toHaveAttribute("aria-selected", "true");
  await expect(page).toHaveURL((url) => url.pathname === "/papers");
  await expect(
    page.getByRole("heading", { name: "Cite the work behind 3D ENA." })
  ).toBeVisible();

  const paperCards = page.locator(".ena3d-paper-card");
  await expect(paperCards).toHaveCount(3);
  await expect(page.locator(".ena3d-paper-card-featured")).toContainText(
    "FOUNDATIONAL METHOD"
  );
  await expect(page.locator(".ena3d-copy-citation")).toHaveCount(3);

  const methodCopy = page.locator(".ena3d-copy-citation").first();
  const citationTarget = await methodCopy.getAttribute("data-citation-target");
  const expectedCitation = await page
    .locator(`#${citationTarget}`)
    .getAttribute("data-citation-text");
  expect(expectedCitation).toContain("Yu, J., Hu, D., & Wang, C.-H. (2024).");
  expect(expectedCitation).toContain("10.1007/978-3-031-76335-9_11");
  await methodCopy.click();
  await expect(methodCopy).toHaveText("Copied");
  if (browserName === "chromium") {
    const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
    expect(clipboardText).toBe(expectedCitation);
  }

  const dimensions = await page.evaluate(() => ({
    viewportWidth: window.innerWidth,
    documentWidth: document.documentElement.scrollWidth,
  }));
  expect(dimensions.documentWidth).toBeLessThanOrEqual(dimensions.viewportWidth);
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
  expect(await health.json()).toMatchObject({
    status: "ok",
    app: "3D ENA",
    ai_enabled: false,
  });

  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await page
    .getByRole("button", { name: "Open the 3D ENA research workspace" })
    .click();
  await expect(page.getByRole("heading", { name: "3D ENA", exact: true })).toBeVisible();
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

  // A rejected exchange must be transactional: the current dataset card and
  // hash stay byte-for-byte stable, and subsequent analyses remain usable.
  const activeDatasetCard = page.locator(".active-dataset-card");
  const activeDatasetBeforeRejectedUpload = await activeDatasetCard.innerText();
  const activeHashBeforeRejectedUpload = await activeDatasetCard
    .locator(".dataset-hash")
    .innerText();
  await exchangeUpload.setInputFiles(INVALID_EXCHANGE_FIXTURE);
  await expect(page.locator(".shiny-notification-error").last()).toContainText(
    "The previously active dataset remains unchanged.",
    { timeout: 30_000 }
  );
  await expect(activeDatasetCard).toHaveText(activeDatasetBeforeRejectedUpload);
  await expect(activeDatasetCard.locator(".dataset-hash")).toHaveText(
    activeHashBeforeRejectedUpload
  );
  await waitForShinyIdle(page);

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
  await expect(page.locator("#main_app-ena_network_plot")).toBeVisible();
  // "No Network" suppresses network edges but intentionally retains the
  // group geometry. The global browser-error collector below is the BUG-010
  // regression gate for the former Plotly runtime failure on this path.
  await expect(
    page.locator("#main_app-ena_network_plot .main-svg").first()
  ).toBeVisible();
  await expect(
    page.locator("#main_app-ena_network_plot.shiny-output-error")
  ).toHaveCount(0);

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

  const trajectoryTraceSummary = await page
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
      const confidenceBoxes = traces.filter(
        (trace) => trace.meta?.trajectory_role === "confidence_boxes"
      );
      const unitPoints = traces.filter(
        (trace) => trace.meta?.trajectory_role === "unit_points"
      );
      const axisShafts = traces.filter(
        (trace) => trace.meta?.trajectory_role === "coordinate_axis_shaft"
      );
      const axisArrowheads = traces.filter(
        (trace) => trace.meta?.trajectory_role === "coordinate_axis_arrowhead"
      );
      const axisLabels = traces.filter(
        (trace) => trace.meta?.trajectory_role === "coordinate_axis_label"
      );
      const axes = ["x", "y", "z"];
      const nearlyEqual = (left, right, tolerance = 1e-10) =>
        Number.isFinite(Number(left)) &&
        Number.isFinite(Number(right)) &&
        Math.abs(Number(left) - Number(right)) <= tolerance;
      const samePoint = (left, right) =>
        axes.every((axis) => nearlyEqual(left[axis], right[axis]));
      const pointAt = (trace, index) => ({
        x: trace.x[index],
        y: trace.y[index],
        z: trace.z[index],
      });
      const vectorAt = (trace, index) => ({
        x: trace.u[index],
        y: trace.v[index],
        z: trace.w[index],
      });
      const solidColorScale = (trace, expectedColor) => {
        const stops = Array.from(trace.colorscale || []);
        return (
          stops.length === 2 &&
          stops.every(
            (stop) =>
              Array.isArray(stop) &&
              String(stop[1]).toUpperCase() === expectedColor.toUpperCase()
          )
        );
      };
      const skipsHover = (trace) =>
        trace.hoverinfo === "skip" ||
        (Array.isArray(trace.hoverinfo) &&
          trace.hoverinfo.length > 0 &&
          trace.hoverinfo.every((value) => value === "skip"));
      const coneGeometryMatchesPaths = arrows.every((arrow) => {
        const path = paths.find(
          (candidate) =>
            candidate.meta.trajectory_key === arrow.meta.trajectory_key
        );
        const segmentCount = Number(arrow.meta.segment_count);
        const coordinateLengthsMatch = ["x", "y", "z", "u", "v", "w"].every(
          (coordinate) => arrow[coordinate]?.length === segmentCount
        );
        const pathColor = String(path?.line?.color || "");
        return (
          path &&
          arrow.type === "cone" &&
          arrow.anchor === "center" &&
          arrow.sizemode === "absolute" &&
          nearlyEqual(arrow.sizeref, 0.13) &&
          nearlyEqual(arrow.meta.position, 0.62) &&
          arrow.showscale === false &&
          arrow.showlegend === false &&
          skipsHover(arrow) &&
          coordinateLengthsMatch &&
          solidColorScale(arrow, pathColor) &&
          path.x.length === segmentCount + 1 &&
          Array.from({ length: segmentCount }, (_, segment) => {
            const start = pointAt(path, segment);
            const destination = pointAt(path, segment + 1);
            const difference = Object.fromEntries(
              axes.map((axis) => [axis, Number(destination[axis]) - Number(start[axis])])
            );
            const length = Math.sqrt(
              axes.reduce((sum, axis) => sum + difference[axis] ** 2, 0)
            );
            const expectedAnchor = Object.fromEntries(
              axes.map((axis) => [axis, Number(start[axis]) + 0.62 * difference[axis]])
            );
            const expectedDirection = Object.fromEntries(
              axes.map((axis) => [axis, difference[axis] / length])
            );
            const actualDirection = vectorAt(arrow, segment);
            const directionLength = Math.sqrt(
              axes.reduce(
                (sum, axis) => sum + Number(actualDirection[axis]) ** 2,
                0
              )
            );
            return (
              Number.isFinite(length) &&
              length > 0 &&
              samePoint(pointAt(arrow, segment), expectedAnchor) &&
              samePoint(actualDirection, expectedDirection) &&
              nearlyEqual(directionLength, 1)
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
      const squareCentroids = [...paths, ...nodeMarkers].every(
        (trace) => trace.marker?.symbol === "square"
      );
      const confidenceBoxesValid = confidenceBoxes.every((trace) => {
        const boxCount = Number(trace.meta.box_count);
        const segmentCount = Number(trace.meta.segment_count);
        return (
          trace.type === "scatter3d" &&
          trace.mode === "lines" &&
          trace.line?.dash === "dot" &&
          trace.connectgaps === false &&
          trace.showlegend === false &&
          Number.isInteger(boxCount) &&
          boxCount > 0 &&
          segmentCount === boxCount * 12 &&
          axes.every(
            (axis) =>
              trace[axis]?.length === segmentCount * 3 &&
              trace[axis].filter((value) => value == null).length === segmentCount
          )
        );
      });
      const unitPointsValid = unitPoints.every((trace) => {
        const pointCount = Number(trace.meta.point_count);
        return (
          trace.type === "scatter3d" &&
          trace.mode === "markers" &&
          trace.showlegend === false &&
          trace.meta.time_var === "Week" &&
          Number.isInteger(pointCount) &&
          pointCount > 0 &&
          axes.every(
            (axis) =>
              trace[axis]?.length === pointCount &&
              trace[axis].every((value) => Number.isFinite(Number(value)))
          ) &&
          trace.marker?.color?.length === pointCount &&
          nearlyEqual(trace.marker?.size, 5.5) &&
          nearlyEqual(trace.marker?.opacity, 0.88) &&
          String(trace.marker?.line?.color).toUpperCase() === "#FFFFFF"
        );
      });
      const expectedOriginAxes = [
        { label: "SVD1", dimension: "x", color: "#E00000" },
        { label: "SVD2", dimension: "y", color: "#0000D0" },
        { label: "SVD3", dimension: "z", color: "#008B00" },
      ];
      const originAxesValid = expectedOriginAxes.every((expected) => {
        const shaft = axisShafts.find(
          (trace) => trace.meta.axis === expected.label
        );
        const arrowhead = axisArrowheads.find(
          (trace) => trace.meta.axis === expected.label
        );
        const label = axisLabels.find(
          (trace) => trace.meta.axis === expected.label
        );
        if (!shaft || !arrowhead || !label) return false;

        const origin = pointAt(shaft, 0);
        const tip = pointAt(shaft, 1);
        const direction = vectorAt(arrowhead, 0);
        const expectedDirection = Object.fromEntries(
          axes.map((axis) => [axis, axis === expected.dimension ? 1 : 0])
        );
        const labelText = Array.isArray(label.text) ? label.text[0] : label.text;
        return (
          shaft.type === "scatter3d" &&
          shaft.mode === "lines" &&
          samePoint(origin, { x: 0, y: 0, z: 0 }) &&
          Number(tip[expected.dimension]) > 0 &&
          axes
            .filter((axis) => axis !== expected.dimension)
            .every((axis) => nearlyEqual(tip[axis], 0)) &&
          String(shaft.line?.color).toUpperCase() === expected.color &&
          nearlyEqual(shaft.line?.width, 4.4) &&
          arrowhead.type === "cone" &&
          arrowhead.anchor === "tip" &&
          arrowhead.sizemode === "absolute" &&
          nearlyEqual(arrowhead.sizeref, 0.21) &&
          samePoint(pointAt(arrowhead, 0), tip) &&
          samePoint(direction, expectedDirection) &&
          solidColorScale(arrowhead, expected.color) &&
          label.type === "scatter3d" &&
          label.mode === "text" &&
          labelText === expected.label &&
          String(label.textfont?.color).toUpperCase() === expected.color &&
          String(label.textfont?.family).includes("Times New Roman") &&
          nearlyEqual(label.textfont?.size, 17) &&
          [shaft, arrowhead, label].every(
            (trace) => trace.showlegend === false && skipsHover(trace)
          )
        );
      });
      const hoverlabel = plot._fullLayout?.hoverlabel || {};
      return {
        traceCount: arrows.length,
        segmentCounts: arrows.map((trace) => trace.meta.segment_count),
        legendEntries: arrows.filter((trace) => trace.showlegend !== false).length,
        nodeMarkerCount: nodeMarkers.length,
        coneGeometryMatchesPaths,
        markersCoverArrows,
        nodeColorsMatch,
        squareCentroids,
        uniqueNodeColors,
        minimumNodeLuminance: Math.min(...nodeColors.map(relativeLuminance)),
        unitPointTraceCount: unitPoints.length,
        unitPointCounts: unitPoints.map((trace) => trace.meta.point_count),
        unitPointsValid,
        confidenceBoxTraceCount: confidenceBoxes.length,
        confidenceBoxesValid,
        originAxisTraceCounts: [
          axisShafts.length,
          axisArrowheads.length,
          axisLabels.length,
        ],
        originAxesValid,
        hoverLabel: {
          bgcolor: hoverlabel.bgcolor,
          bordercolor: hoverlabel.bordercolor,
          align: hoverlabel.align,
          fontColor: hoverlabel.font?.color,
        },
      };
    });
  expect(trajectoryTraceSummary).toMatchObject({
    traceCount: 1,
    segmentCounts: [14],
    legendEntries: 0,
    nodeMarkerCount: 1,
    coneGeometryMatchesPaths: true,
    markersCoverArrows: true,
    nodeColorsMatch: true,
    squareCentroids: true,
    uniqueNodeColors: 15,
    unitPointTraceCount: 1,
    unitPointCounts: [255],
    unitPointsValid: true,
    // This smoke path deliberately disables bootstrap above; confidence-box
    // presence and counts are therefore expected to be zero here. The generic
    // trace contract still guards any confidence-box trace that is emitted.
    confidenceBoxTraceCount: 0,
    confidenceBoxesValid: true,
    originAxisTraceCounts: [3, 3, 3],
    originAxesValid: true,
    hoverLabel: {
      bgcolor: "#FFFFFF",
      bordercolor: "#526777",
      align: "left",
      fontColor: "#102A43",
    },
  });
  expect(trajectoryTraceSummary.minimumNodeLuminance).toBeGreaterThanOrEqual(0.16);

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

  const pathDownload = await captureDownload(
    page,
    page.locator("#main_app-trajectory-download_path")
  );
  expect(pathDownload.filename).toMatch(/^centroid-path-\d{8}\.csv$/);
  const pathCsv = normalizedText(pathDownload.bytes);
  const pathCsvLines = pathCsv.trimEnd().split("\n");
  expect(pathCsvLines).toHaveLength(16);
  expect(pathCsvLines[0]).toContain("centroid_SVD1");
  expect(pathCsvLines[0]).toContain("n_used");
  expect(pathCsvLines[0]).toContain(".analysis_time_var");

  const metadataDownload = await captureDownload(
    page,
    page.locator("#main_app-trajectory-download_metadata")
  );
  expect(metadataDownload.filename).toMatch(/^centroid-path-metadata-\d{8}\.csv$/);
  const metadataCsv = normalizedText(metadataDownload.bytes);
  expect(metadataCsv).toMatch(/^"field","value"\n/);
  expect(metadataCsv).toContain('"dataset_sha256"');
  expect(metadataCsv).toContain('"time_var","Week"');
  expect(metadataCsv).toContain('"r_version"');

  const bundleDownload = await captureDownload(
    page,
    page.locator("#main_app-trajectory-download_bundle")
  );
  expect(bundleDownload.filename).toMatch(
    /^ena3d-trajectory-analysis-\d{8}\.zip$/
  );
  expect(bundleDownload.bytes.subarray(0, 2).toString("ascii")).toBe("PK");
  const archive = unzipSync(new Uint8Array(bundleDownload.bytes));
  expect(Object.keys(archive).sort()).toEqual([
    "diagnostics.csv",
    "manifest.json",
    "metadata.csv",
    "path.csv",
  ]);
  expect(strFromU8(archive["path.csv"]).replace(/\r\n/g, "\n")).toBe(pathCsv);
  const manifest = JSON.parse(strFromU8(archive["manifest.json"]));
  expect(manifest).toMatchObject({
    schema: "urn:3dena:trajectory-analysis-bundle:1",
    schema_version: 1,
    files: ["path.csv", "diagnostics.csv", "metadata.csv"],
    settings: { time_var: "Week" },
  });
  expect(manifest.metadata.dataset_sha256).toMatch(/^[0-9a-f]{64}$/);

  // Top-level view switches must not silently recompute or discard a completed
  // result. Exercise real Stats output, then compare the path trace on return.
  const trajectoryPathBeforeStats = await page
    .locator("#main_app-trajectory-trajectory_plot")
    .evaluate((plot) =>
      (plot.data || [])
        .filter((trace) => trace.meta?.trajectory_role === "path")
        .map((trace) => ({ x: trace.x, y: trace.y, z: trace.z }))
    );
  const statsTab = page.getByRole("tab", { name: "Stats", exact: true });
  await statsTab.click();
  await expect(statsTab).toHaveAttribute("aria-selected", "true");
  await page.getByRole("combobox", { name: "Study design" }).click();
  await page
    .getByRole("option", {
      name: "Independent groups (between participants)",
      exact: true,
    })
    .click();
  await expect(page.locator("#main_app-stats_design_status")).toContainText(
    "Independent-groups design selected.",
    { timeout: 30_000 }
  );
  await expect(page.locator("#main_app-stats_box_x_axis-data_table table")).toBeVisible();
  await expect(page.locator("#main_app-stats_box_x_axis-p_adjust_method")).toContainText(
    "Adjusted p (holm):"
  );

  await modelTab.click();
  await openModelTab(
    page,
    "Trajectory",
    page.getByRole("combobox", { name: "Time / order variable" })
  );
  await expect(page.locator("#main_app-trajectory-status")).toContainText("Completed");
  const trajectoryPathAfterStats = await page
    .locator("#main_app-trajectory-trajectory_plot")
    .evaluate((plot) =>
      (plot.data || [])
        .filter((trace) => trace.meta?.trajectory_role === "path")
        .map((trace) => ({ x: trace.x, y: trace.y, z: trace.z }))
    );
  expect(trajectoryPathAfterStats).toEqual(trajectoryPathBeforeStats);

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
    await expect(activeDatasetCard.locator("strong")).toHaveText(
      "small-valid.ena3d.json"
    );
    await expect(activeDatasetCard.locator(".dataset-hash")).not.toHaveText(
      activeHashBeforeRejectedUpload
    );
    await modelTab.click();
    await openModelTab(
      page,
      "Trajectory",
      page.getByRole("combobox", { name: "Time / order variable" })
    );
    await expect(page.locator("#main_app-trajectory-status")).toContainText(
      "Run the trajectory analysis again."
    );
    await expect(page.locator(".trajectory-downloads")).toHaveCount(0);
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
