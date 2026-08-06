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
    heading: { name: "Meet the team.", exact: true, level: 1 },
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

test("internal route links expose real hrefs and use client-side history", async ({
  page,
}) => {
  const home = publicRoutes.find((route) => route.value === "home");
  const team = publicRoutes.find((route) => route.value === "team");
  const about = publicRoutes.find((route) => route.value === "about");

  await page.goto("/team", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);
  await expectSitePage(page, team);

  const brand = page.getByRole("link", {
    name: "Return to the 3D ENA home page",
  });
  const aboutLink = page.getByRole("link", { name: "More on About" });
  await expect(brand).toHaveAttribute("href", "/");
  await expect(brand).toHaveAttribute("data-site-page", "home");
  await expect(aboutLink).toHaveAttribute("href", "/about");
  await expect(aboutLink).toHaveAttribute("data-site-page", "about");

  await page.evaluate(() => {
    window.__ena3dSpaRouteMarker = "retained";
  });
  await aboutLink.click();
  await expectSitePage(page, about);
  expect(await page.evaluate(() => window.__ena3dSpaRouteMarker)).toBe("retained");

  await page.goBack();
  await expectSitePage(page, team);
  await brand.click();
  await expectSitePage(page, home);
  expect(await page.evaluate(() => window.__ena3dSpaRouteMarker)).toBe("retained");

  await page.goBack();
  await expectSitePage(page, team);
});

test("internal route links preserve native new-tab destinations", async ({
  browserName,
  context,
  page,
}, testInfo) => {
  test.skip(
    browserName !== "chromium" || testInfo.project.name !== "desktop-chromium",
    "Native auxiliary-click behavior is covered once in desktop Chromium."
  );

  await page.goto("/team", { waitUntil: "domcontentloaded" });
  await waitForShinyIdle(page);

  const openWithNativeNewPage = async (link, expectedPath, clickOptions) => {
    const [openedPage] = await Promise.all([
      context.waitForEvent("page"),
      link.click(clickOptions),
    ]);
    await openedPage.waitForLoadState("domcontentloaded");
    await expect(openedPage).toHaveURL((url) => url.pathname === expectedPath);
    await openedPage.close();
  };

  const aboutLink = page.getByRole("link", { name: "More on About" });
  const brand = page.getByRole("link", {
    name: "Return to the 3D ENA home page",
  });

  await openWithNativeNewPage(
    aboutLink,
    "/about",
    { button: "middle" }
  );
  await expect(page).toHaveURL((url) => url.pathname === "/team");
  await openWithNativeNewPage(
    brand,
    "/",
    { modifiers: ["ControlOrMeta"] }
  );
  await expect(page).toHaveURL((url) => url.pathname === "/team");
  await openWithNativeNewPage(
    aboutLink,
    "/about",
    { modifiers: ["Shift"] }
  );
  await expect(page).toHaveURL((url) => url.pathname === "/team");

  await brand.click({ button: "right" });
  await expect(page).toHaveURL((url) => url.pathname === "/team");
});
