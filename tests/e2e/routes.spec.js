const { test, expect } = require("@playwright/test");

async function waitForShinyIdle(page) {
  await expect(page.locator("html")).not.toHaveClass(/shiny-busy/, {
    timeout: 30_000,
  });
  await page.waitForTimeout(500);
}

async function waitForAppReady(page) {
  await page.waitForFunction(
    () => {
      const app = window.Shiny?.shinyapp;
      const guard = document.getElementById("ena3d-connection-guard");
      return Boolean(
        app?.$socket?.readyState === WebSocket.OPEN &&
          app.$allowReconnect === false &&
          guard?.hidden &&
          guard.dataset.sessionProof === "ready"
      );
    },
    null,
    { timeout: 30_000 }
  );
  await waitForShinyIdle(page);
}

async function expectStaticSitePage(page, route) {
  await expect(page).toHaveURL((url) => url.pathname === route.path);
  await expect(page.locator("html")).toHaveAttribute("data-site-mode", "static");
  await expect(page.locator("html")).toHaveAttribute("data-site-route", route.value);
  const activeLink = page.locator(`#site_nav a[data-value="${route.value}"]`);
  await expect(activeLink).toHaveAttribute("href", route.path);
  await expect(activeLink).toHaveAttribute("aria-current", "page");
  await expect(page.getByRole("heading", route.heading)).toBeVisible();
}

async function expectStaticSessionBoundary(page) {
  expect(await page.evaluate(() => typeof window.Shiny)).toBe("undefined");
  await expect(page.locator('script[src*="shiny"]')).toHaveCount(0);
  await expect(page.locator("#ena3d-connection-guard")).toHaveCount(0);
  await expect(page.locator("html")).not.toHaveClass(/ena3d-connection-blocked/);
  expect(
    await page.locator("body > *").evaluateAll((elements) =>
      elements.some((element) => element.inert)
    )
  ).toBe(false);
}

async function expectAppPage(page) {
  await expect(page).toHaveURL((url) => url.pathname === "/app");
  await expect(page.locator("html")).toHaveAttribute("data-site-mode", "stateful");
  await expect(page.locator("html")).toHaveAttribute("data-site-route", "tool");
  const activeTab = page.locator('#site_nav a[data-value="tool"]');
  await expect(activeTab).toHaveAttribute("aria-selected", "true");
  await expect(page.getByRole("heading", { name: "3D ENA", exact: true })).toBeVisible();
  await expect(page.locator("#ena3d-connection-guard")).toHaveCount(1);
}

async function clickStaticTab(page, value) {
  const tab = page.locator(`#site_nav a[data-value="${value}"]`);
  if (!(await tab.isVisible())) {
    await page.getByRole("button", { name: "Toggle navigation" }).click();
  }
  await tab.click();
}

const staticRoutes = [
  {
    path: "/",
    value: "home",
    heading: {
      name: "Make epistemic connections visible in three dimensions.",
      exact: true,
    },
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

test("static pages direct-load and refresh without a Shiny session", async ({ page }) => {
  const shinySockets = [];
  page.on("websocket", (socket) => {
    if (/(__sockjs__|\/websocket\/?$)/.test(socket.url())) {
      shinySockets.push(socket.url());
    }
  });

  for (const route of staticRoutes) {
    await page.goto(route.path, { waitUntil: "domcontentloaded" });
    await expectStaticSitePage(page, route);
    await expectStaticSessionBoundary(page);
    expect(shinySockets).toEqual([]);

    await page.evaluate(() => {
      document.dispatchEvent(new Event("shiny:disconnected"));
    });
    await page.waitForTimeout(400);
    await expectStaticSessionBoundary(page);
  }

  const team = staticRoutes.find((route) => route.value === "team");
  await page.goto("/team/", { waitUntil: "domcontentloaded" });
  await expectStaticSitePage(page, team);
  await page.reload({ waitUntil: "domcontentloaded" });
  await expectStaticSitePage(page, team);
  await expectStaticSessionBoundary(page);
  expect(shinySockets).toEqual([]);
});

test("/app direct-loads the stateful workspace and keeps its guard", async ({ page }) => {
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForAppReady(page);
  await expectAppPage(page);
});

test("static navigation follows browser back and forward history", async ({ page }) => {
  const home = staticRoutes.find((route) => route.value === "home");
  const team = staticRoutes.find((route) => route.value === "team");
  const about = staticRoutes.find((route) => route.value === "about");

  await page.goto("/", { waitUntil: "domcontentloaded" });
  await expectStaticSitePage(page, home);

  await clickStaticTab(page, "team");
  await expectStaticSitePage(page, team);
  await clickStaticTab(page, "about");
  await expectStaticSitePage(page, about);

  await page.goBack();
  await expectStaticSitePage(page, team);
  await page.goBack();
  await expectStaticSitePage(page, home);
  await page.goForward();
  await expectStaticSitePage(page, team);
  await expectStaticSessionBoundary(page);
});

test("static internal links expose real hrefs and use client-side history", async ({
  page,
}) => {
  const home = staticRoutes.find((route) => route.value === "home");
  const team = staticRoutes.find((route) => route.value === "team");
  const about = staticRoutes.find((route) => route.value === "about");

  await page.goto("/team", { waitUntil: "domcontentloaded" });
  await expectStaticSitePage(page, team);

  const brand = page.getByRole("link", {
    name: "Return to the 3D ENA home page",
  });
  const aboutLink = page.getByRole("link", { name: "More on About" });
  await expect(brand).toHaveAttribute("href", "/");
  await expect(brand).toHaveAttribute("data-site-page", "home");
  await expect(aboutLink).toHaveAttribute("href", "/about");
  await expect(aboutLink).toHaveAttribute("data-site-page", "about");

  await page.evaluate(() => {
    window.__ena3dStaticRouteMarker = "retained";
  });
  await aboutLink.click();
  await expectStaticSitePage(page, about);
  expect(await page.evaluate(() => window.__ena3dStaticRouteMarker)).toBe("retained");

  await page.goBack();
  await expectStaticSitePage(page, team);
  await brand.click();
  await expectStaticSitePage(page, home);
  expect(await page.evaluate(() => window.__ena3dStaticRouteMarker)).toBe("retained");
  await expectStaticSessionBoundary(page);
});

test("workspace links cross the static-to-stateful document boundary", async ({ page }) => {
  await page.goto("/", { waitUntil: "domcontentloaded" });
  await expectStaticSitePage(page, staticRoutes[0]);
  await page.evaluate(() => {
    window.__ena3dStaticDocumentMarker = "static";
  });

  const workspaceLink = page.getByRole("link", {
    name: "Open the 3D ENA research workspace",
  }).first();
  await expect(workspaceLink).toHaveAttribute("href", "/app");
  await workspaceLink.click();
  await waitForAppReady(page);
  await expectAppPage(page);
  expect(await page.evaluate(() => window.__ena3dStaticDocumentMarker)).toBeUndefined();
});

test("trajectory workspace links remain shareable and refreshable", async ({ page }) => {
  await page.goto("/app?workspace=trajectory", { waitUntil: "domcontentloaded" });
  await waitForAppReady(page);
  await expect(page.locator('#workspace_sections a[data-value="Model"]')).toHaveAttribute(
    "aria-selected",
    "true"
  );
  await expect(page.locator('#main_app-mytabs a[data-value="trajectory"]')).toHaveAttribute(
    "aria-selected",
    "true"
  );

  await page.reload({ waitUntil: "domcontentloaded" });
  await waitForAppReady(page);
  await expect(page.locator('#main_app-mytabs a[data-value="trajectory"]')).toHaveAttribute(
    "aria-selected",
    "true"
  );
});

test("/app remains fail-closed after a session disconnect", async ({ page }) => {
  await page.goto("/app", { waitUntil: "domcontentloaded" });
  await waitForAppReady(page);

  await page.evaluate(() => {
    window.jQuery(document).trigger("shiny:disconnected");
  });

  const guard = page.locator("#ena3d-connection-guard");
  await expect(guard).toBeVisible();
  await expect(guard).toHaveAttribute("data-state", "failed");
  await expect(page.getByRole("alertdialog")).toBeVisible();
  await expect(page.getByRole("button", { name: "Reload page" })).toBeFocused();
  await expect(page.locator("html")).toHaveClass(/ena3d-connection-blocked/);
  expect(
    await page.locator("body > *").evaluateAll((elements) =>
      elements.some(
        (element) =>
          element.id !== "ena3d-connection-guard" &&
          !element.querySelector?.("#ena3d-connection-guard") &&
          element.inert
      )
    )
  ).toBe(true);
});

test("static internal links preserve native new-tab destinations", async ({
  browserName,
  context,
  page,
}, testInfo) => {
  test.skip(
    browserName !== "chromium" || testInfo.project.name !== "desktop-chromium",
    "Native auxiliary-click behavior is covered once in desktop Chromium."
  );

  await page.goto("/team", { waitUntil: "domcontentloaded" });
  await expectStaticSessionBoundary(page);

  const openWithNativeNewPage = async (link, expectedPath, clickOptions) => {
    const [openedPage] = await Promise.all([
      context.waitForEvent("page"),
      link.click(clickOptions),
    ]);
    await openedPage.waitForLoadState("domcontentloaded");
    await expect(openedPage).toHaveURL((url) => url.pathname === expectedPath);
    await expectStaticSessionBoundary(openedPage);
    await openedPage.close();
  };

  const aboutLink = page.getByRole("link", { name: "More on About" });
  const brand = page.getByRole("link", {
    name: "Return to the 3D ENA home page",
  });

  await openWithNativeNewPage(aboutLink, "/about", { button: "middle" });
  await expect(page).toHaveURL((url) => url.pathname === "/team");
  await openWithNativeNewPage(brand, "/", { modifiers: ["ControlOrMeta"] });
  await expect(page).toHaveURL((url) => url.pathname === "/team");
  await openWithNativeNewPage(aboutLink, "/about", { modifiers: ["Shift"] });
  await expect(page).toHaveURL((url) => url.pathname === "/team");

  await brand.click({ button: "right" });
  await expect(page).toHaveURL((url) => url.pathname === "/team");
});
