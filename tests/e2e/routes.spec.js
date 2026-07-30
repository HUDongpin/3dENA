const { test, expect } = require("@playwright/test");

async function waitForShinyIdle(page) {
  await expect(page.locator("html")).not.toHaveClass(/shiny-busy/, {
    timeout: 30_000,
  });
  await page.waitForTimeout(500);
}

async function expectSitePage(page, route) {
  await expect(page).toHaveURL((url) => url.pathname === route.path);
  await expect(page.locator("html")).toHaveAttribute("data-site-route", route.value);
  const activeTab = page.locator(`#site_nav a[data-value="${route.value}"]`);
  await expect(activeTab).toHaveAttribute("href", route.path);
  await expect(activeTab).toHaveAttribute(
    "aria-selected",
    "true"
  );
  await expect(page.getByRole("heading", route.heading)).toBeVisible();
}

async function clickSiteTab(page, value) {
  const tab = page.locator(`#site_nav a[data-value="${value}"]`);
  if (!(await tab.isVisible())) {
    await page.getByRole("button", { name: "Toggle navigation" }).click();
  }
  await tab.click();
  return tab;
}

const publicRoutes = [
  {
    path: "/",
    value: "home",
    heading: {
      name: "Make epistemic connections visible in three dimensions.",
      exact: true,
    },
  },
  {
    path: "/app",
    value: "tool",
    heading: { name: "3D ENA", exact: true },
  },
  {
    path: "/papers",
    value: "papers",
    heading: { name: "Cite the work behind 3D ENA.", exact: true },
  },
  {
    path: "/team",
    value: "team",
    heading: { name: "Meet the 3D ENA Research Team", exact: true },
  },
  {
    path: "/about",
    value: "about",
    heading: { name: "Dr. Peter Hu Dongpin", exact: true },
  },
];

test("every public page supports direct loading, sharing, and refresh", async ({ page }) => {
  for (const route of publicRoutes) {
    await page.goto(route.path, { waitUntil: "domcontentloaded" });
    await waitForShinyIdle(page);
    await expectSitePage(page, route);
  }

  const team = publicRoutes.find((route) => route.value === "team");
  await page.goto("/team/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await expectSitePage(page, team);
  await page.reload({ waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await expectSitePage(page, team);
});

test("site navigation follows browser back and forward history", async ({ page }) => {
  const home = publicRoutes.find((route) => route.value === "home");
  const team = publicRoutes.find((route) => route.value === "team");
  const about = publicRoutes.find((route) => route.value === "about");

  await page.goto("/", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await expectSitePage(page, home);

  await clickSiteTab(page, "team");
  await expectSitePage(page, team);
  await clickSiteTab(page, "about");
  await expectSitePage(page, about);

  await page.goBack();
  await expectSitePage(page, team);
  await page.goBack();
  await expectSitePage(page, home);
  await page.goForward();
  await expectSitePage(page, team);
});
