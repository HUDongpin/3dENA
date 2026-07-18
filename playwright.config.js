const { defineConfig } = require("@playwright/test");

const port = Number.parseInt(process.env.E2E_PORT || "3838", 10);
if (!Number.isInteger(port) || port < 1024 || port > 65535) {
  throw new Error("E2E_PORT must be an integer between 1024 and 65535.");
}

const baseURL = `http://127.0.0.1:${port}`;

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
    },
  },
  projects: [
    {
      name: "desktop-chromium",
      use: {
        browserName: "chromium",
        viewport: { width: 1280, height: 900 },
      },
    },
    {
      name: "tablet-768px-chromium",
      use: {
        browserName: "chromium",
        viewport: { width: 768, height: 1024 },
      },
    },
    {
      name: "mobile-390px-chromium",
      use: {
        browserName: "chromium",
        viewport: { width: 390, height: 844 },
        deviceScaleFactor: 1,
        hasTouch: true,
        isMobile: true,
      },
    },
  ],
});
