const AxeBuilder = require("@axe-core/playwright").default;
const { test, expect } = require("@playwright/test");
const fs = require("node:fs/promises");
const path = require("node:path");

const SAMPLE_NAME = "newfrat_enaset.Rdata";
const FINDING_ID = "ENA-BUG-008";
const WCAG_TAGS = ["wcag2a", "wcag2aa", "wcag21a", "wcag21aa"];
const BLOCKING_IMPACTS = new Set(["critical", "serious"]);

test.beforeEach(async ({ page }) => {
  await page.route("**/_vercel/insights/script.js", (route) =>
    route.fulfill({
      status: 200,
      contentType: "application/javascript",
      body: "// Vercel Web Analytics test stub.\n",
    })
  );
});

async function waitForShinyIdle(page) {
  await expect(page.locator("html")).not.toHaveClass(/shiny-busy/, {
    timeout: 30_000,
  });
  await page.waitForTimeout(500);
}

async function selectTrustedSample(page) {
  const sample = page.getByRole("combobox", {
    name: "Trusted sample dataset",
  });
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
  await expect(page.locator(".active-dataset-card strong")).toHaveText(SAMPLE_NAME, {
    timeout: 30_000,
  });
  await waitForShinyIdle(page);
}

function summarizeViolations(violations) {
  return violations.map((violation) => ({
    rule: violation.id,
    impact: violation.impact,
    help: violation.help,
    nodes: violation.nodes.map((node) => ({
      selector: node.target,
      contrast: {
        observed_ratio: Number(
          node.failureSummary?.match(/contrast of ([0-9.]+)/)?.[1] || NaN
        ),
        required_ratio: Number(
          node.failureSummary?.match(/Expected contrast ratio of ([0-9.]+):1/)?.[1] ||
            NaN
        ),
      },
      failure_summary: node.failureSummary,
    })),
  }));
}

async function findBlockingViolations(page, name, selector) {
  const results = await new AxeBuilder({ page })
    .include(selector)
    .withTags(WCAG_TAGS)
    .analyze();
  const violations = results.violations.filter((violation) =>
    BLOCKING_IMPACTS.has(violation.impact)
  );
  const summary = summarizeViolations(violations);
  return summary.map((violation) => ({ surface: name, ...violation }));
}

test("home, data, and Stats surfaces pass keyboard and automated WCAG checks", async ({
  browserName,
  page,
}, testInfo) => {
  test.skip(
    browserName !== "chromium",
    "Axe rules are browser-independent; Chromium viewport projects provide the matrix."
  );

  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  const accessibilityFindings = await findBlockingViolations(
    page,
    "home",
    ".ena3d-home-page"
  );

  const launch = page.getByRole("button", {
    name: "Open the 3D ENA research workspace",
  });
  await launch.focus();
  await expect(launch).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(page.locator('#site_nav a[data-value="tool"]')).toHaveAttribute(
    "aria-selected",
    "true"
  );
  await expect(page.getByRole("heading", { name: "3D ENA", exact: true })).toBeVisible();
  await waitForShinyIdle(page);
  accessibilityFindings.push(
    ...(await findBlockingViolations(
      page,
      "data-workspace",
      ".ena3d-tool-page"
    ))
  );

  await selectTrustedSample(page);
  const statsTab = page.getByRole("tab", { name: "Stats", exact: true });
  await statsTab.focus();
  await expect(statsTab).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(statsTab).toHaveAttribute("aria-selected", "true");
  await expect(statsTab).toBeFocused();
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
  accessibilityFindings.push(
    ...(await findBlockingViolations(page, "stats", ".stats-panel"))
  );
  const report = {
    schema: "urn:3dena:browser-accessibility-audit:1",
    finding_id: FINDING_ID,
    project: testInfo.project.name,
    viewport: page.viewportSize(),
    wcag_tags: WCAG_TAGS,
    blocking_impacts: [...BLOCKING_IMPACTS],
    findings: accessibilityFindings,
  };
  const auditDirectory = path.join(
    process.cwd(),
    "output",
    "playwright",
    "audit"
  );
  await fs.mkdir(auditDirectory, { recursive: true });
  const reportPath = path.join(
    auditDirectory,
    `accessibility-${testInfo.project.name}.json`
  );
  await fs.writeFile(reportPath, `${JSON.stringify(report, null, 2)}\n`, "utf8");
  await testInfo.attach("accessibility-audit.json", {
    path: reportPath,
    contentType: "application/json",
  });

  if (process.env.ENA3D_AUDIT_REPORT_ONLY !== "1") {
    expect(
      accessibilityFindings,
      "Serious or critical automated WCAG findings were detected."
    ).toEqual([]);
  }
});
