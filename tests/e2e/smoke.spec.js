const { test, expect } = require("@playwright/test");
const path = require("node:path");
const { strFromU8, unzipSync } = require("fflate");

const SAMPLE_NAME = "newfrat_enaset.Rdata";
const CLASS1_SAMPLE_NAME = "class1_timepoints_enaset.RData";
const TRUSTED_SAMPLE_NAMES = [
  CLASS1_SAMPLE_NAME,
  SAMPLE_NAME,
  "sample_enaset.Rdata",
  "student_enaset.RData",
];
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
const RAW_FIXTURE = path.join(__dirname, "fixtures", "small-raw.csv");
const PAIRED_RAW_FIXTURE = path.join(
  __dirname,
  "fixtures",
  "small-paired-raw.csv"
);
const EXCEL_RAW_FIXTURE = path.join(
  __dirname,
  "..",
  "testthat",
  "test_data",
  "testing_data.xlsx"
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

async function selectTrustedSample(page, sampleName = SAMPLE_NAME) {
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
    sampleName,
    { timeout: 15_000 }
  );
  await sample.click();
  await page.getByRole("option", { name: sampleName, exact: true }).click();
  await expect(page.getByRole("heading", { name: "Active dataset" })).toBeVisible({
    timeout: 30_000,
  });
  await expect(page.getByRole("status").filter({ hasText: sampleName })).toBeVisible();
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

async function captureBrowserDownload(page, locator) {
  await expect(locator).toBeVisible();
  const [download] = await Promise.all([
    page.waitForEvent("download", { timeout: 20_000 }),
    locator.click(),
  ]);
  const stream = await download.createReadStream();
  const chunks = [];
  for await (const chunk of stream) chunks.push(Buffer.from(chunk));
  return {
    bytes: Buffer.concat(chunks),
    filename: download.suggestedFilename(),
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
    /^data:image\/png;base64,/
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
    .getByRole("link", {
      name: "Open the centroid trajectory analysis workspace",
    })
    .click();
  await waitForShinyIdle(page);
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

test("papers page exposes four copy-ready citations in the requested order", async ({
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
  await expect(paperCards).toHaveCount(4);
  await expect(paperCards.nth(0)).toContainText(
    "Design and development from rENA to jENA"
  );
  await expect(paperCards.nth(1)).toContainText("Development of ENA 3D");
  await expect(paperCards.nth(2)).toContainText("Effects on the Learning Achievement");
  await expect(paperCards.nth(3)).toContainText(
    "The Application of ENA to Political Discourse in Taiwan"
  );
  await expect(page.locator(".ena3d-paper-card-featured")).toContainText(
    "FOUNDATIONAL METHOD"
  );
  await expect(page.locator(".ena3d-copy-citation")).toHaveCount(4);
  await expect(page.locator(".ena3d-doi-link")).toHaveCount(3);

  const conferenceCopy = page.locator(".ena3d-copy-citation").first();
  const citationTarget = await conferenceCopy.getAttribute("data-citation-target");
  const expectedCitation = await page
    .locator(`#${citationTarget}`)
    .getAttribute("data-citation-text");
  expect(expectedCitation).toContain(
    "Hu, D., Hamilton, E., Tu, Y. F., & Xu, Q. (2026, November)."
  );
  expect(expectedCitation).toContain(
    "Design and development from rENA to jENA: Accelerating the creation of web-based Open ENA tools."
  );
  expect(expectedCitation).toContain(
    "[Conference paper]. International Conference on Quantitative Ethnography."
  );
  await conferenceCopy.click();
  await expect(conferenceCopy).toHaveText("Copied");
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

test("trajectory display levels recover when data arrives after control initialization", async ({
  page,
}) => {
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);

  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  await openModelTab(
    page,
    "Trajectory",
    page.getByRole("combobox", { name: "Time / order variable" })
  );

  const displayLevels = page.locator("#main_app-trajectory-display_levels");
  await expect(displayLevels.locator("option")).toHaveCount(0);

  const dataTab = page.getByRole("tab", { name: "Data", exact: true });
  await dataTab.click();
  await selectTrustedSample(page, CLASS1_SAMPLE_NAME);

  await modelTab.click();
  await openModelTab(
    page,
    "Trajectory",
    page.getByRole("combobox", { name: "Time / order variable" })
  );
  await expect
    .poll(() =>
      displayLevels.evaluate((select) => ({
        choices: Object.keys(select.selectize?.options || {}).sort(),
        selected: Array.from(select.selectize?.items || []).sort(),
      }))
    )
    .toEqual({
      choices: ["G1", "G2", "G3", "G6", "G7"],
      selected: ["G1", "G2", "G3", "G6", "G7"],
    });

  await page
    .getByRole("button", { name: "Run / recompute trajectory" })
    .click();
  await expect(page.locator("#main_app-trajectory-status")).toContainText(
    "Completed 15 centroid slices across 5 trajectories",
    { timeout: 60_000 }
  );
  await expect(
    page.locator("#main_app-trajectory-trajectory_plot .main-svg").first()
  ).toBeVisible({ timeout: 30_000 });
  await expect(
    page.locator("#main_app-trajectory-trajectory_plot.shiny-output-error")
  ).toHaveCount(0);

  // A rapid Selectize change followed immediately by the action button must
  // not let the event-priority Run click overtake the latest displayed levels.
  await displayLevels.evaluate((select) => {
    select.selectize.removeItem("G1");
    select.selectize.removeItem("G2");
  });
  await page
    .getByRole("button", { name: "Run / recompute trajectory" })
    .click();
  await expect
    .poll(() =>
      page
        .locator("#main_app-trajectory-trajectory_plot")
        .evaluate((plot) =>
          (plot.data || [])
            .filter((trace) => trace.meta?.trajectory_role === "path")
            .map((trace) => trace.name)
            .sort()
        )
    )
    .toEqual(["G3", "G6", "G7"]);
});

test("every trusted sample activates a renderable ENA dataset", async ({ page }) => {
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  const dataTab = page.getByRole("tab", { name: "Data", exact: true });
  const modelTab = page.getByRole("tab", { name: "Model", exact: true });

  for (const sampleName of TRUSTED_SAMPLE_NAMES) {
    await selectTrustedSample(page, sampleName);
    await expect(page.locator("#main_app-active_dataset_card strong")).toHaveText(
      sampleName
    );
    await expect
      .poll(() =>
        page.locator("#main_app-x").evaluate((select) =>
          select.selectize
            ? Object.keys(select.selectize.options || {}).length
            : Array.from(select.options).filter((option) => option.value).length
        )
      )
      .toBeGreaterThanOrEqual(3);

    await modelTab.click();
    await openModelTab(
      page,
      "Overall",
      page.locator("#main_app-ena_overall_plot")
    );
    await expect(
      page.locator("#main_app-ena_overall_plot.shiny-output-error")
    ).toHaveCount(0);
    await openModelTab(
      page,
      "Comparison",
      page.getByRole("combobox", { name: "Group 1", exact: true })
    );
    await expect(page.locator("#main_app-comparison_status")).toContainText(
      "Comparing"
    );
    await expect(page.locator("#main_app-ena_points_plot")).toBeVisible({
      timeout: 30_000,
    });
    await expect
      .poll(() =>
        page.locator("#main_app-ena_points_plot").evaluate((plot) =>
          Array.isArray(plot.data) ? plot.data.length : 0
        )
      )
      .toBeGreaterThan(0);
    await openModelTab(
      page,
      "Change",
      page.getByRole("combobox", { name: "Select Group Variable" })
    );
    await expect(page.locator("#main_app-change_value_status")).toContainText(
      "values available",
      { timeout: 30_000 }
    );
    await expect
      .poll(() =>
        page.locator("#main_app-unit_change").evaluate((select) =>
          select.selectize?.items?.[0] || select.value || ""
        )
      )
      .not.toBe("");
    await expect(page.locator("#main_app-ena_unit_group_change_plot")).toBeVisible({
      timeout: 30_000,
    });
    await expect(
      page.locator("#main_app-ena_unit_group_change_plot.shiny-output-error")
    ).toHaveCount(0);
    await dataTab.click();
    await expect(dataTab).toHaveAttribute("aria-selected", "true");
  }
});

test("fullscreen falls back when the browser rejects native fullscreen", async ({
  page,
}) => {
  await page.addInitScript(() => {
    Object.defineProperty(Element.prototype, "requestFullscreen", {
      configurable: true,
      value: () => Promise.reject(new DOMException("Not allowed", "NotAllowedError")),
    });
  });
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await selectTrustedSample(page);

  await page
    .getByRole("button", { name: "Enter fullscreen for the visible ENA plot" })
    .click();

  const fallbackPlot = page.locator(
    ".plotly.html-widget.ena3d-fullscreen-fallback"
  );
  await expect(fallbackPlot).toBeVisible();
  await expect(page.locator("#ena3d-fullscreen-status")).toHaveText(
    "Fullscreen fallback active."
  );

  const exitButton = page.getByRole("button", { name: "Exit fullscreen plot" });
  await expect(exitButton).toBeVisible();
  await exitButton.click();
  await expect(fallbackPlot).toHaveCount(0);
  await expect(page.locator("#ena3d-fullscreen-status")).toHaveText(
    "Fullscreen closed."
  );
});

test("Plotly modebars execute every 3D and 2D plot action", async (
  { page },
  testInfo
) => {
  test.skip(
    testInfo.project.name !== "desktop-chromium",
    "One Chromium execution covers the canvas-specific Plotly controls."
  );
  const browserErrors = captureBrowserErrors(page);
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await selectTrustedSample(page, CLASS1_SAMPLE_NAME);

  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  const overallPlot = page.locator("#main_app-ena_overall_plot");
  await openModelTab(page, "Overall", overallPlot);
  await overallPlot.hover({ position: { x: 220, y: 160 } });

  const modebarButton = (plot, title) =>
    plot.locator(`.modebar-btn[data-title="${title}"]`);
  const threeDimensionalActions = [
    "Download plot as a png",
    "Zoom",
    "Pan",
    "Orbital rotation",
    "Turntable rotation",
    "Reset camera to default",
    "Reset camera to last save",
    "Toggle show closest data on hover",
  ];
  for (const title of threeDimensionalActions) {
    await expect(modebarButton(overallPlot, title)).toBeVisible();
  }

  const pngDownload = await captureBrowserDownload(
    page,
    modebarButton(overallPlot, "Download plot as a png")
  );
  expect(pngDownload.filename).toMatch(/\.png$/i);
  expect(pngDownload.bytes.length).toBeGreaterThan(1_000);
  expect(pngDownload.bytes.subarray(0, 8).toString("hex")).toBe(
    "89504e470d0a1a0a"
  );

  const dragModes = [
    ["Zoom", "zoom"],
    ["Pan", "pan"],
    ["Orbital rotation", "orbit"],
    ["Turntable rotation", "turntable"],
  ];
  for (const [title, expectedMode] of dragModes) {
    await modebarButton(overallPlot, title).click();
    await expect
      .poll(() =>
        overallPlot.evaluate((plot) => plot._fullLayout?.scene?.dragmode || "")
      )
      .toBe(expectedMode);
  }
  await modebarButton(overallPlot, "Reset camera to default").click();
  await modebarButton(overallPlot, "Reset camera to last save").click();
  await modebarButton(overallPlot, "Toggle show closest data on hover").click();

  const plotlyLogo = overallPlot.locator(
    '.modebar-btn[data-title^="Produced with Plotly.js"]'
  );
  await expect(plotlyLogo).toBeVisible();
  await expect(plotlyLogo).toHaveAttribute("href", /plotly\.com/);
  const [plotlyPopup] = await Promise.all([
    page.waitForEvent("popup", { timeout: 15_000 }),
    plotlyLogo.click({ noWaitAfter: true }),
  ]);
  expect(plotlyPopup.url()).toMatch(/^https?:\/\/(www\.)?plotly\.com\/?/);
  await plotlyPopup.close();

  await openModelTab(
    page,
    "Trajectory",
    page.getByRole("combobox", { name: "Time / order variable" })
  );
  await page.getByRole("radio", { name: "2D projection" }).check();
  await page
    .getByRole("button", { name: "Run / recompute trajectory" })
    .click();
  await expect(page.locator("#main_app-trajectory-status")).toContainText(
    "Completed 15 centroid slices across 5 trajectories",
    { timeout: 60_000 }
  );
  const trajectoryPlot = page.locator("#main_app-trajectory-trajectory_plot");
  await trajectoryPlot.hover({ position: { x: 220, y: 160 } });
  const twoDimensionalActions = [
    "Zoom",
    "Pan",
    "Box Select",
    "Lasso Select",
    "Zoom in",
    "Zoom out",
    "Autoscale",
    "Reset axes",
    "Show closest data on hover",
    "Compare data on hover",
  ];
  for (const title of twoDimensionalActions) {
    const action = modebarButton(trajectoryPlot, title);
    await expect(action).toBeVisible();
    await action.click();
  }
  await expect(
    page.locator("#main_app-trajectory-trajectory_plot.shiny-output-error")
  ).toHaveCount(0);
  expect(browserErrors).toEqual([]);
});

test("global camera, sidebar, axis, slider, and plot toggles update the live plot", async (
  { page },
  testInfo
) => {
  test.skip(
    testInfo.project.name !== "desktop-chromium",
    "One desktop run covers global controls; responsive layout is tested separately."
  );
  const browserErrors = captureBrowserErrors(page);
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await selectTrustedSample(page, CLASS1_SAMPLE_NAME);

  const hide = page.getByRole("button", { name: "Hide ENA controls" });
  await hide.click();
  const show = page.getByRole("button", { name: "Show ENA controls" });
  await expect(show).toHaveAttribute("aria-expanded", "false");
  await show.click();
  await expect(hide).toHaveAttribute("aria-expanded", "true");

  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  const overallPlot = page.locator("#main_app-ena_overall_plot");
  await openModelTab(page, "Overall", overallPlot);
  const cameraViews = [
    ["Default 3D Camera", { x: 1.25, y: 1.25, z: 1.25 }],
    ["X-Y Plane", { x: 0, y: 0, z: 2.5 }],
    ["X-Z Plane", { x: 0, y: -2.5, z: 0 }],
    ["Y-Z Plane", { x: 2.5, y: 0, z: 0 }],
    ["Y-X Plane", { x: 0, y: 0, z: -2.5 }],
    ["Z-X Plane", { x: 0, y: 2.5, z: 0 }],
    ["Z-Y Plane", { x: -2.5, y: 0, z: 0 }],
  ];
  for (const [name, expectedEye] of cameraViews) {
    await page.getByRole("radio", { name }).check();
    await expect
      .poll(() =>
        overallPlot.evaluate((plot) => {
          const eye = plot._fullLayout?.scene?.camera?.eye || {};
          return { x: eye.x, y: eye.y, z: eye.z };
        })
      )
      .toEqual(expectedEye);
  }

  const plotToolsTab = page.getByRole("tab", { name: "Plot Tools", exact: true });
  await plotToolsTab.click();
  await expect(plotToolsTab).toHaveAttribute("aria-selected", "true");
  const chooseAxis = async (id, label, value) => {
    await page
      .locator(`#main_app-${id} + .selectize-control .selectize-input`)
      .click();
    await page.getByRole("option", { name: value, exact: true }).click();
    await expect(page.locator(`#main_app-${id}`)).toHaveValue(value);
    await expect(page.getByText(label, { exact: true })).toBeVisible();
  };
  await chooseAxis("x", "X axis", "SVD4");
  await chooseAxis("y", "Y axis", "SVD5");
  await chooseAxis("z", "Z axis", "SVD6");
  await expect
    .poll(() =>
      page.evaluate(() => [
        document.querySelector("#main_app-x")?.value,
        document.querySelector("#main_app-y")?.value,
        document.querySelector("#main_app-z")?.value,
      ])
    )
    .toEqual(["SVD4", "SVD5", "SVD6"]);

  const sliderHandles = page.locator(".irs-handle.single");
  await expect(sliderHandles).toHaveCount(2);
  await sliderHandles.nth(0).click();
  for (let step = 0; step < 3; step += 1) {
    await sliderHandles.nth(0).press("ArrowRight");
  }
  await sliderHandles.nth(1).click();
  for (let step = 0; step < 2; step += 1) {
    await sliderHandles.nth(1).press("ArrowRight");
  }
  await expect(page.locator("#main_app-scale_factor")).toHaveValue("4");
  await expect(page.locator("#main_app-line_width")).toHaveValue("5");

  const toggleIds = [
    "show_grid",
    "show_zeroline",
    "show_x_axis_arrow",
    "show_y_axis_arrow",
    "show_z_axis_arrow",
  ];
  for (const id of toggleIds) {
    await page.locator(`#main_app-${id}`).uncheck();
  }
  await expect
    .poll(() =>
      overallPlot.evaluate((plot) => ({
        xGrid: plot._fullLayout?.scene?.xaxis?.showgrid,
        yGrid: plot._fullLayout?.scene?.yaxis?.showgrid,
        zGrid: plot._fullLayout?.scene?.zaxis?.showgrid,
        xZero: plot._fullLayout?.scene?.xaxis?.zeroline,
        yZero: plot._fullLayout?.scene?.yaxis?.zeroline,
        zZero: plot._fullLayout?.scene?.zaxis?.zeroline,
      }))
    )
    .toEqual({
      xGrid: false,
      yGrid: false,
      zGrid: false,
      xZero: false,
      yZero: false,
      zZero: false,
    });
  await expect(page.locator("#main_app-ena_overall_plot.shiny-output-error")).toHaveCount(
    0
  );
  for (const id of toggleIds) {
    await page.locator(`#main_app-${id}`).check();
  }
  expect(browserErrors).toEqual([]);
});

test("every Model control updates Overall, Networks, Comparison, and Change safely", async (
  { page },
  testInfo
) => {
  test.skip(
    testInfo.project.name !== "desktop-chromium",
    "One desktop run covers the stateful Model control matrix."
  );
  const browserErrors = captureBrowserErrors(page);
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await selectTrustedSample(page, CLASS1_SAMPLE_NAME);

  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  const overallPlot = page.locator("#main_app-ena_overall_plot");
  await openModelTab(page, "Overall", overallPlot);
  const overallGroups = page.locator(
    '#main_app-select_group input[type="checkbox"]'
  );
  await expect(overallGroups).toHaveCount(2);
  await overallGroups.nth(0).uncheck();
  await overallGroups.nth(1).uncheck();
  await expect(overallPlot).toBeVisible();
  await expect(page.locator("#main_app-ena_overall_plot.shiny-output-error")).toHaveCount(
    0
  );
  await expect
    .poll(() => overallPlot.evaluate((plot) => (plot.data || []).length))
    .toBeGreaterThan(0);
  await overallGroups.nth(0).check();
  await overallGroups.nth(1).check();

  const networkPlot = page.locator("#main_app-ena_network_plot");
  await openModelTab(
    page,
    "Networks",
    page.getByRole("combobox", { name: "Show Network" })
  );
  const networkControl = page.locator(
    "#main_app-network_selector + .selectize-control .selectize-input"
  );
  const networkInput = page.getByRole("combobox", { name: "Show Network" });
  await networkControl.click();
  await networkInput.press("ArrowDown");
  await networkInput.press("Enter");
  await expect
    .poll(() =>
      networkPlot.evaluate((plot) => String(plot._fullLayout?.title?.text || ""))
    )
    .toContain("Network (Group): Non-GenAI group");
  await networkControl.click();
  await networkInput.press("ArrowDown");
  await networkInput.press("ArrowDown");
  await networkInput.press("Enter");
  await expect
    .poll(() =>
      networkPlot.evaluate((plot) => String(plot._fullLayout?.title?.text || ""))
    )
    .toContain("Network (Unit):");

  const firstNetworkLayers = [
    '[id="main_app-group-1-Non.GenAI.group-points-btn"]',
    '[id="main_app-group-1-Non.GenAI.group-show-mean-btn"]',
    '[id="main_app-group-1-Non.GenAI.group-show-conf-int-btn"]',
  ];
  for (const selector of firstNetworkLayers) {
    await page.locator(selector).uncheck();
  }
  const firstNetworkColor = page.locator(
    '[id="main_app-group-1-Non.GenAI.group-color-selector"]'
  );
  await firstNetworkColor.fill("not-a-color");
  await firstNetworkColor.press("Tab");
  await expect(page.locator("#main_app-ena_network_plot.shiny-output-error")).toHaveCount(
    0
  );
  await firstNetworkColor.fill("#112233");
  await firstNetworkColor.press("Tab");
  await expect(page.locator("#main_app-ena_network_plot.shiny-output-error")).toHaveCount(
    0
  );

  const comparisonPlot = page.locator("#main_app-ena_points_plot");
  await openModelTab(
    page,
    "Comparison",
    page.getByRole("combobox", { name: "Group 1", exact: true })
  );
  await expect(page.locator("#main_app-comparison_status")).toHaveText(
    "Comparing Non-GenAI group vs GenAI group."
  );
  await expect
    .poll(() => comparisonPlot.evaluate((plot) => (plot.data || []).length))
    .toBeGreaterThan(0);
  const comparisonState = await page.evaluate(() => {
    const group1 = document.querySelector("#main_app-compare_group_1")?.selectize;
    const group2 = document.querySelector("#main_app-compare_group_2")?.selectize;
    return {
      selected1: group1?.items?.[0],
      selected2: group2?.items?.[0],
      choices1: Object.keys(group1?.options || {}),
      choices2: Object.keys(group2?.options || {}),
    };
  });
  expect(comparisonState.selected1).not.toBe(comparisonState.selected2);
  expect(comparisonState.choices1).not.toContain(comparisonState.selected2);
  expect(comparisonState.choices2).not.toContain(comparisonState.selected1);
  await page.locator("#main_app-comparison_group_1_color").fill("invalid-color");
  await page.locator("#main_app-comparison_group_1_color").press("Tab");
  await page.locator("#main_app-comparison_group_2_color").fill("rgba(0,0,0,0.4)");
  await page.locator("#main_app-comparison_group_2_color").press("Tab");
  const comparisonToggles = [
    "compare_group_1_show_mean",
    "compare_group_1_show_confidence_interval",
    "compare_group_2_show_mean",
    "compare_group_2_show_confidence_interval",
  ];
  for (const id of comparisonToggles) {
    await page.locator(`#main_app-${id}`).check();
  }
  await expect(page.locator("#main_app-ena_points_plot.shiny-output-error")).toHaveCount(
    0
  );

  const changePlot = page.locator("#main_app-ena_unit_group_change_plot");
  await openModelTab(
    page,
    "Change",
    page.getByRole("combobox", { name: "Select Group Variable" })
  );
  const changeVariableControl = page.locator(
    "#main_app-group_change_var + .selectize-control .selectize-input"
  );
  const changeVariable = page.getByRole("combobox", {
    name: "Select Group Variable",
  });
  await changeVariableControl.click();
  await changeVariable.press("ArrowDown");
  await changeVariable.press("Enter");
  await expect(page.locator("#main_app-change_value_status")).toContainText(
    "5 values available"
  );
  const changeValueControl = page.locator(
    "#main_app-unit_change + .selectize-control .selectize-input"
  );
  const changeValue = page.getByRole("combobox", { name: "Selected value" });
  await changeValueControl.click();
  await changeValue.press("ArrowDown");
  await changeValue.press("Enter");
  await page.locator("#main_app-group_change_show_mean").uncheck();
  await page.locator("#main_app-group_change_show_confidence_interval").uncheck();
  await expect(changePlot).toBeVisible();
  await expect(
    page.locator("#main_app-ena_unit_group_change_plot.shiny-output-error")
  ).toHaveCount(0);
  await expect
    .poll(() => changePlot.evaluate((plot) => (plot.data || []).length))
    .toBeGreaterThan(0);
  expect(browserErrors).toEqual([]);
});

test("Stats controls run every compatible test family and guard invalid designs", async (
  { page },
  testInfo
) => {
  test.skip(
    testInfo.project.name !== "desktop-chromium",
    "One desktop run covers the inferential-control matrix."
  );
  const browserErrors = captureBrowserErrors(page);
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await selectTrustedSample(page, CLASS1_SAMPLE_NAME);
  const statsTab = page.getByRole("tab", { name: "Stats", exact: true });
  await statsTab.click();
  await expect(statsTab).toHaveAttribute("aria-selected", "true");

  const chooseVisibleOption = async (label, option) => {
    const input = page.getByRole("combobox", { name: label, exact: true });
    const selectizeSurface = input.locator("xpath=..");
    await selectizeSurface.click();
    await page.getByRole("option", { name: option, exact: true }).click();
  };
  await chooseVisibleOption(
    "Study design",
    "Independent groups (between participants)"
  );
  await expect(page.locator("#main_app-stats_design_status")).toContainText(
    "Independent-groups design selected."
  );
  await expect(page.locator("#main_app-stats_box_x_axis-data_table table")).toBeVisible();

  const rankSum = page.getByRole("tab", {
    name: "Rank-sum (independent)",
    exact: true,
  });
  await rankSum.click();
  await expect(rankSum).toHaveAttribute("aria-selected", "true");
  await expect(
    page.locator("#main_app-stats_box_x_axis_wilcox_unpaired-data_table table")
  ).toBeVisible();

  const adjustments = [
    ["Benjamini-Hochberg FDR", "BH"],
    ["Bonferroni", "bonferroni"],
    ["None (raw p-values)", "none"],
    ["Holm (recommended)", "holm"],
  ];
  for (const [label, value] of adjustments) {
    await chooseVisibleOption("Multiple-testing adjustment", label);
    await expect(
      page.locator(
        "#main_app-stats_box_x_axis_wilcox_unpaired-p_adjust_method"
      )
    ).toContainText(`Adjusted p (${value}):`);
  }

  await chooseVisibleOption("Group 2", "Non-GenAI group");
  await expect(page.locator("#main_app-stats_design_status")).toHaveText(
    "No inferential test has been run: Group 1 and Group 2 must differ."
  );
  await chooseVisibleOption("Group 2", "GenAI group");
  await expect(page.locator("#main_app-stats_design_status")).toContainText(
    "Independent-groups design selected."
  );

  await chooseVisibleOption(
    "Study design",
    "Repeated/paired groups (within participant)"
  );
  const signedRank = page.getByRole("tab", {
    name: "Signed-rank (paired)",
    exact: true,
  });
  await signedRank.click();
  await expect(signedRank).toHaveAttribute("aria-selected", "true");
  await expect(page.locator("#main_app-stats_design_status")).toContainText(
    "Repeated/paired design selected."
  );
  await chooseVisibleOption("Pairing ID for paired tests", "Speaker");
  await expect(page.locator("#main_app-stats_pair_status")).toContainText(
    "Paired test unavailable for the current selections."
  );
  const alternatives = [
    "Greater: Group 1 is greater than Group 2",
    "Less: Group 1 is less than Group 2",
    "Two-sided: Group 1 differs from Group 2",
  ];
  for (const alternative of alternatives) {
    await chooseVisibleOption("Paired Wilcoxon alternative hypothesis", alternative);
    await expect(page.locator("#main_app-stats_design_status")).toContainText(
      "Repeated/paired design selected."
    );
  }
  await expect(
    page.locator("#main_app-stats_box_x_axis_wilcox_paired-data_table table")
  ).toContainText("Pairing ID must identify one observation per selected group.");
  expect(browserErrors).toEqual([]);
});

test("AI interpretation controls expose an exact envelope and fail closed without a provider", async (
  { page },
  testInfo
) => {
  test.skip(
    testInfo.project.name !== "desktop-chromium",
    "One desktop run covers the AI drawer control matrix."
  );
  const browserErrors = captureBrowserErrors(page);
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await selectTrustedSample(page, CLASS1_SAMPLE_NAME);
  await page.getByRole("tab", { name: "Model", exact: true }).click();
  await openModelTab(
    page,
    "Overall",
    page.locator("#main_app-ena_overall_plot.js-plotly-plot")
  );

  const root = page.locator("[data-ena-ai-root]");
  const drawer = page.locator("[data-ena-ai-drawer]");
  const toggle = page.getByRole("button", {
    name: "AI interpretation",
    exact: true,
  });
  await expect(root).toHaveAttribute("data-state", "disabled");
  await toggle.click();
  await expect(toggle).toHaveAttribute("aria-expanded", "true");
  await expect(drawer).toHaveAttribute("aria-hidden", "false");
  await expect(
    page.getByRole("heading", { name: "Interpret ENA results", exact: true })
  ).toBeVisible();
  await expect(
    page.getByRole("heading", {
      name: "AI interpretation is unavailable",
      exact: true,
    })
  ).toBeVisible();

  for (const mode of ["Quick", "Deep", "Challenge"]) {
    const choice = page.getByRole("radio", { name: mode, exact: true });
    await choice.check();
    await expect(choice).toBeChecked();
  }
  for (const language of ["English", "中文"]) {
    const choice = page.getByRole("radio", { name: language, exact: true });
    await choice.check();
    await expect(choice).toBeChecked();
  }

  const context = "Synthetic aggregate study context; no participant identifiers.";
  await page
    .getByRole("textbox", { name: "Research context (optional)", exact: true })
    .fill(context);
  await expect(
    page.locator("[data-ena-ai-context-count]")
  ).toHaveText(`${context.length} / 1500`);

  const previewButton = page.getByRole("button", {
    name: "Review exact provider data envelope",
    exact: true,
  });
  await previewButton.click();
  await expect(previewButton).toHaveAttribute("aria-expanded", "true");
  const previewPanel = page.locator("[data-ena-ai-preview-panel]");
  await expect(previewPanel).toBeVisible();
  const preview = page.locator("#main_app-ai_interpretation-preview");
  await expect(preview).toContainText('"mode": "challenge"');
  await expect(preview).toContainText('"output_language": "Chinese"');
  await expect(preview).toContainText(context);

  const consent = page.getByRole("checkbox", {
    name: /I reviewed this exact provider data envelope/,
  });
  const interpret = page.getByRole("button", {
    name: "Interpret ENA results",
    exact: true,
  });
  await expect(consent).toBeDisabled();
  await expect(consent).not.toBeChecked();
  await expect(interpret).toBeDisabled();

  // Any option change invalidates the reviewed one-time envelope.
  await page.getByRole("radio", { name: "Quick", exact: true }).check();
  await expect(previewPanel).toBeHidden();
  await expect(root).toHaveAttribute("data-preview-ready", "false");
  await previewButton.click();
  await expect(preview).toContainText('"mode": "quick"');

  const close = page.getByRole("button", {
    name: "Close AI interpretation panel",
    exact: true,
  });
  await close.click();
  await expect(drawer).toHaveAttribute("aria-hidden", "true");
  await expect(toggle).toHaveAttribute("aria-expanded", "false");
  await toggle.click();
  await drawer.press("Escape");
  await expect(drawer).toHaveAttribute("aria-hidden", "true");
  expect(browserErrors).toEqual([]);
});

test("raw CSV mapping builds and activates a usable ENA model", async (
  { page },
  testInfo
) => {
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);

  const rawUpload = page.locator('input[type="file"][accept=".csv,.xlsx,.xls"]');
  await rawUpload.setInputFiles(RAW_FIXTURE);
  await expect(page.locator("#main_app-raw_upload_status")).toContainText(
    "Loaded small-raw.csv",
    { timeout: 30_000 }
  );

  await expect
    .poll(() =>
      page.evaluate(() => ({
        units: document.querySelector("#main_app-raw_unit_columns")?.selectize
          ?.items,
        sequence: document.querySelector("#main_app-raw_conversation_columns")
          ?.selectize?.items,
        codes: document.querySelector("#main_app-raw_code_columns")?.selectize
          ?.items,
        group: document.querySelector("#main_app-raw_group_column")?.value,
      }))
    )
    .toEqual({
      units: ["Group", "Name"],
      sequence: ["Lesson"],
      codes: ["EC", "ICT", "MCO", "ATT"],
      group: "Group",
    });

  await page
    .getByRole("button", { name: "Build and activate ENA model" })
    .click();
  await expect(page.locator("#main_app-raw_model_status")).toContainText(
    "ENA model is active",
    { timeout: 60_000 }
  );
  await expect(page.locator("#main_app-active_dataset_card")).toContainText(
    "small-raw.csv (modeled)"
  );

  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  await openModelTab(
    page,
    "Trajectory",
    page.getByRole("combobox", { name: "Time / order variable" })
  );
  await page
    .getByRole("button", { name: "Run / recompute trajectory" })
    .click();
  await expect(page.locator("#main_app-trajectory-status")).toContainText(
    "Completed 4 centroid slices across 2 trajectories",
    { timeout: 60_000 }
  );
  await expect(
    page.locator("#main_app-trajectory-trajectory_plot.shiny-output-error")
  ).toHaveCount(0);

  // One desktop browser also exercises the two expensive, conditional
  // download buttons. This fixture deliberately reuses Student IDs across
  // Experimental and Control so the exact paired A/B workflow is valid.
  if (testInfo.project.name === "desktop-chromium") {
    const pairedIdControl = page.locator(
      "#main_app-trajectory-id_var + .selectize-control .selectize-input"
    );
    await pairedIdControl.click();
    await page.getByRole("option", { name: /^Name — 4\/4 repeated ID profiles/ }).click();
    await expect(
      page.locator("#main_app-trajectory-comparison_overlap_status")
    ).toContainText("Raw-ID overlap: 4 IDs", { timeout: 30_000 });
    await page.locator("#main_app-trajectory-run_comparison").check();
    await page.locator("#main_app-trajectory-confirm_paired_ids").check();
    await page.locator("#main_app-trajectory-show_uncertainty").check();
    await page.locator("#main_app-trajectory-bootstrap_reps").fill("200");
    await page.locator("#main_app-trajectory-confidence").fill("0.9");
    await page.locator("#main_app-trajectory-bootstrap_seed").fill("2027");
    await page
      .getByRole("button", { name: "Run / recompute trajectory" })
      .click();
    await expect(page.locator("#main_app-trajectory-status")).toContainText(
      "Completed 4 centroid slices across 2 trajectories",
      { timeout: 60_000 }
    );

    const uncertaintyDownload = await captureBrowserDownload(
      page,
      page.locator("#main_app-trajectory-download_uncertainty")
    );
    expect(uncertaintyDownload.filename).toMatch(
      /^centroid-path-bootstrap-\d{8}\.csv$/
    );
    const uncertaintyCsv = normalizedText(uncertaintyDownload.bytes);
    expect(uncertaintyCsv).toContain("centroid_SVD1_lower");
    expect(uncertaintyCsv).toContain("centroid_SVD1_upper");

    const comparisonDownload = await captureBrowserDownload(
      page,
      page.locator("#main_app-trajectory-download_comparison")
    );
    expect(comparisonDownload.filename).toMatch(
      /^centroid-path-comparison-Experimental-vs-Control-\d{8}\.csv$/
    );
    const comparisonCsv = normalizedText(comparisonDownload.bytes);
    expect(comparisonCsv).toContain("n_matched");
    expect(comparisonCsv).toContain("difference_SVD1");

  }
});

test("adjacent numeric groups remain separate in general selectors", async ({
  page,
}) => {
  const browserErrors = captureBrowserErrors(page);
  const exchange = JSON.parse(
    require("node:fs").readFileSync(EXCHANGE_FIXTURE, "utf8")
  );
  for (const tableName of ["meta_data", "points", "line_weights"]) {
    const groupColumn = exchange.tables[tableName].columns.find(
      (column) => column.name === "groupid"
    );
    if (!groupColumn) throw new Error(`Missing groupid in ${tableName}.`);
    groupColumn.values = [
      1,
      1,
      1 + Number.EPSILON,
      1 + Number.EPSILON,
    ];
  }
  const expectedGroups = [
    "1 [exact=0x1p+0]",
    "1 [exact=0x1.0000000000001p+0]",
  ];

  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  const exchangeUpload = page.locator(
    'input[type="file"][accept=".ena3d.json"]'
  );
  await exchangeUpload.setInputFiles({
    name: "adjacent-double-groups.ena3d.json",
    mimeType: "application/json",
    buffer: Buffer.from(`${JSON.stringify(exchange)}\n`, "utf8"),
  });
  await expect(page.locator("#main_app-active_dataset_card strong")).toHaveText(
    "adjacent-double-groups.ena3d.json",
    { timeout: 30_000 }
  );
  await waitForShinyIdle(page);

  await page.getByRole("tab", { name: "Model", exact: true }).click();
  const overallPlot = page.locator("#main_app-ena_overall_plot");
  await openModelTab(page, "Overall", overallPlot);
  const groupInputs = page.locator(
    '#main_app-select_group input[type="checkbox"]'
  );
  await expect(groupInputs).toHaveCount(2);
  await expect
    .poll(() =>
      groupInputs.evaluateAll((inputs) => inputs.map((input) => input.value))
    )
    .toEqual(expectedGroups);

  const renderedGroupSizes = () =>
    overallPlot.evaluate((plot, groups) =>
      Object.fromEntries(
        (plot.data || [])
          .filter((trace) => groups.includes(String(trace.name || "")))
          .map((trace) => [String(trace.name), trace.x?.length || 0])
      ),
    expectedGroups);
  await expect.poll(renderedGroupSizes).toEqual({
    [expectedGroups[0]]: 2,
    [expectedGroups[1]]: 2,
  });

  await groupInputs.nth(1).uncheck();
  await waitForShinyIdle(page);
  await expect.poll(renderedGroupSizes).toEqual({
    [expectedGroups[0]]: 2,
  });
  await groupInputs.nth(1).check();
  await groupInputs.nth(0).uncheck();
  await waitForShinyIdle(page);
  await expect.poll(renderedGroupSizes).toEqual({
    [expectedGroups[1]]: 2,
  });
  expect(browserErrors).toEqual([]);
});

test("paired raw data produces positive signed-rank results", async (
  { page },
  testInfo
) => {
  test.skip(
    testInfo.project.name !== "desktop-chromium",
    "One desktop run closes the positive paired-inference path."
  );
  const browserErrors = captureBrowserErrors(page);
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await page
    .locator('input[type="file"][accept=".csv,.xlsx,.xls"]')
    .setInputFiles(PAIRED_RAW_FIXTURE);
  await expect(page.locator("#main_app-raw_upload_status")).toContainText(
    "Loaded small-paired-raw.csv",
    { timeout: 30_000 }
  );
  await page
    .getByRole("button", { name: "Build and activate ENA model" })
    .click();
  await expect(page.locator("#main_app-raw_model_status")).toContainText(
    "ENA model is active",
    { timeout: 60_000 }
  );
  await expect(page.locator("#main_app-active_dataset_card")).toContainText(
    "small-paired-raw.csv (modeled)"
  );

  await page.getByRole("tab", { name: "Stats", exact: true }).click();
  const studyDesign = page.getByRole("combobox", {
    name: "Study design",
    exact: true,
  });
  await studyDesign.locator("xpath=..").click();
  await page
    .getByRole("option", {
      name: "Repeated/paired groups (within participant)",
      exact: true,
    })
    .click();
  const pairId = page.getByRole("combobox", {
    name: "Pairing ID for paired tests",
    exact: true,
  });
  await pairId.locator("xpath=..").click();
  await page.getByRole("option", { name: "Name", exact: true }).click();
  await expect(page.locator("#main_app-stats_pair_status")).toContainText(
    "4 IDs matched",
    { timeout: 30_000 }
  );
  await page
    .getByRole("tab", { name: "Signed-rank (paired)", exact: true })
    .click();
  const pairedTable = page.locator(
    "#main_app-stats_box_x_axis_wilcox_paired-data_table table"
  );
  await expect(pairedTable).toContainText("Matched N");
  await expect(pairedTable).toContainText("4");
  const pairedAlternative = page.getByRole("combobox", {
    name: "Paired Wilcoxon alternative hypothesis",
    exact: true,
  });
  for (const [label, value] of [
    ["Greater: Group 1 is greater than Group 2", "greater"],
    ["Less: Group 1 is less than Group 2", "less"],
    ["Two-sided: Group 1 differs from Group 2", "two.sided"],
  ]) {
    await pairedAlternative.locator("xpath=..").click();
    await page.getByRole("option", { name: label, exact: true }).click();
    await expect(
      page.locator("#main_app-stats_box_x_axis_wilcox_paired-test_type")
    ).toContainText(`Paired Wilcoxon V (${value})`);
  }
  expect(browserErrors).toEqual([]);
});

test("raw Excel upload exposes and switches real worksheets", async ({ page }) => {
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);

  const rawUpload = page.locator('input[type="file"][accept=".csv,.xlsx,.xls"]');
  await rawUpload.setInputFiles(EXCEL_RAW_FIXTURE);
  const status = page.locator("#main_app-raw_upload_status");
  await expect(status).toContainText("Loaded testing_data.xlsx", {
    timeout: 30_000,
  });
  await expect(status).toContainText("20 columns");

  const worksheet = page.getByRole("combobox", { name: "Worksheet" });
  const worksheetSelect = page.locator("#main_app-raw_excel_sheet");
  await expect(worksheet).toBeVisible();
  await expect
    .poll(() =>
      worksheetSelect.evaluate((select) =>
        Object.keys(select.selectize?.options || {}).length
      )
    )
    .toBe(3);
  await worksheet.click();
  await page.getByRole("option", { name: "DATA_3組", exact: true }).click();
  await expect(status).toContainText("34 columns", { timeout: 30_000 });
  await expect(status).toContainText("DATA_3組");
  await expect(page.locator("#main_app-raw_data_preview table")).toContainText(
    "condition"
  );
});

test("trajectory CSV and ZIP controls trigger real browser downloads", async ({
  page,
}) => {
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await selectTrustedSample(page, CLASS1_SAMPLE_NAME);

  const modelTab = page.getByRole("tab", { name: "Model", exact: true });
  await modelTab.click();
  await openModelTab(
    page,
    "Trajectory",
    page.getByRole("combobox", { name: "Time / order variable" })
  );
  await page
    .getByRole("button", { name: "Run / recompute trajectory" })
    .click();
  await expect(page.locator("#main_app-trajectory-status")).toContainText(
    "Completed 15 centroid slices across 5 trajectories",
    { timeout: 60_000 }
  );

  const pathDownload = await captureBrowserDownload(
    page,
    page.locator("#main_app-trajectory-download_path")
  );
  expect(pathDownload.filename).toMatch(/^centroid-path-\d{8}\.csv$/);
  const pathCsv = normalizedText(pathDownload.bytes);
  expect(pathCsv.trimEnd().split("\n")).toHaveLength(16);
  expect(pathCsv).toContain("centroid_SVD1");

  const metadataDownload = await captureBrowserDownload(
    page,
    page.locator("#main_app-trajectory-download_metadata")
  );
  expect(metadataDownload.filename).toMatch(
    /^centroid-path-metadata-\d{8}\.csv$/
  );
  const metadataCsv = normalizedText(metadataDownload.bytes);
  expect(metadataCsv).toContain('"time_var","Period"');
  expect(metadataCsv).toContain('"dataset_sha256"');

  const bundleDownload = await captureBrowserDownload(
    page,
    page.locator("#main_app-trajectory-download_bundle")
  );
  expect(bundleDownload.filename).toMatch(
    /^ena3d-trajectory-analysis-\d{8}\.zip$/
  );
  const archive = unzipSync(new Uint8Array(bundleDownload.bytes));
  expect(Object.keys(archive).sort()).toEqual([
    "diagnostics.csv",
    "manifest.json",
    "metadata.csv",
    "path.csv",
  ]);
  expect(strFromU8(archive["path.csv"]).replace(/\r\n/g, "\n")).toBe(pathCsv);
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
  const healthJson = await health.json();
  expect(healthJson).toMatchObject({
    status: "ok",
    app: "3D ENA",
    ai_enabled: false,
  });

  await page.goto("/", { waitUntil: "domcontentloaded" });
  await page
    .getByRole("link", { name: "Open the 3D ENA research workspace" })
    .click();
  await waitForShinyIdle(page);
  await expect(page.getByRole("heading", { name: "3D ENA", exact: true })).toBeVisible();
  await expect(page.locator(".ena3d-build-id")).toHaveCount(0);
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
