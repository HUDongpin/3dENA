const { defineConfig } = require("@playwright/test");

const soakEnabled = process.env.ENA3D_AUDIT_SOAK === "1";
const accessibilityAuditEnabled = process.env.ENA3D_AUDIT_A11Y === "1";
const defaultPort = soakEnabled ? "43840" : "3838";
const port = Number.parseInt(process.env.E2E_PORT || defaultPort, 10);
if (!Number.isInteger(port) || port < 1024 || port > 65535) {
  throw new Error("E2E_PORT must be an integer between 1024 and 65535.");
}
if (soakEnabled && port === 3838) {
  throw new Error("The soak audit must use an isolated E2E_PORT, not 3838.");
}

const baseURL = `http://127.0.0.1:${port}`;
const standardProject = (name, use) => ({
  name,
  testIgnore: /(accessibility|soak)\.spec\.js/,
  use,
});
const projects = [
  standardProject("desktop-chromium", {
    browserName: "chromium",
    viewport: { width: 1280, height: 900 },
  }),
  standardProject("desktop-firefox", {
    browserName: "firefox",
    viewport: { width: 1280, height: 900 },
  }),
  standardProject("desktop-webkit", {
    browserName: "webkit",
    viewport: { width: 1280, height: 900 },
  }),
  standardProject("tablet-768px-chromium", {
    browserName: "chromium",
    viewport: { width: 768, height: 1024 },
  }),
  standardProject("mobile-390px-chromium", {
    browserName: "chromium",
    viewport: { width: 390, height: 844 },
    deviceScaleFactor: 1,
    hasTouch: true,
    isMobile: true,
  }),
];

if (soakEnabled) {
  projects.push({
    name: "audit-soak-chromium",
    testMatch: /soak\.spec\.js/,
    use: {
      browserName: "chromium",
      viewport: { width: 1280, height: 900 },
    },
  });
}

if (accessibilityAuditEnabled) {
  const accessibilityProject = (name, viewport, extraUse = {}) => ({
    name,
    testMatch: /accessibility\.spec\.js/,
    use: {
      browserName: "chromium",
      viewport,
      screenshot: "off",
      trace: "off",
      video: "off",
      ...extraUse,
    },
  });
  projects.push(
    accessibilityProject("audit-a11y-desktop-chromium", {
      width: 1280,
      height: 900,
    }),
    accessibilityProject("audit-a11y-tablet-768px-chromium", {
      width: 768,
      height: 1024,
    }),
    accessibilityProject(
      "audit-a11y-mobile-390px-chromium",
      { width: 390, height: 844 },
      { deviceScaleFactor: 1, hasTouch: true, isMobile: true }
    )
  );
}

module.exports = defineConfig({
  testDir: "./tests/e2e",
  outputDir: "output/playwright/test-results",
  fullyParallel: false,
  workers: 1,
  timeout: 90_000,
  expect: {
    timeout: 15_000,
  },
  retries: process.env.CI ? 1 : 0,
  reporter: process.env.CI
    ? [
        ["line"],
        ["html", { outputFolder: "output/playwright/report", open: "never" }],
      ]
    : [["list"]],
  use: {
    baseURL,
    actionTimeout: 15_000,
    navigationTimeout: 30_000,
    acceptDownloads: true,
    colorScheme: "light",
    locale: "en-US",
    reducedMotion: "reduce",
    screenshot: "only-on-failure",
    trace: "retain-on-failure",
    video: "retain-on-failure",
  },
  webServer: {
    command: "Rscript tests/e2e/start-app.R",
    url: `${baseURL}/ena3d-health/healthz.json`,
    timeout: 120_000,
    reuseExistingServer: !process.env.CI,
    stdout: "pipe",
    stderr: "pipe",
    env: {
      ...process.env,
      E2E_PORT: String(port),
      ENA3D_BUILD_ID: process.env.ENA3D_BUILD_ID || "e2e-local",
      // Browser audits must never make a live provider request, even when the
      // parent shell happens to contain deployment credentials.
      ENA3D_AI_ENABLED: "false",
    },
  },
  projects,
});
