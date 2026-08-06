(function () {
  "use strict";

  const routeForPage = Object.freeze({
    home: "/",
    tool: "/app",
    papers: "/papers",
    team: "/team",
    about: "/about",
  });
  const titleForPage = Object.freeze({
    home: "3D ENA | Epistemic Network Analysis",
    tool: "3D ENA Workspace | Epistemic Network Analysis",
    papers: "Papers | 3D ENA",
    team: "Team | 3D ENA",
    about: "About | 3D ENA",
  });
  const pageForRoute = Object.freeze(
    Object.fromEntries(Object.entries(routeForPage).map(([page, route]) => [route, page]))
  );

  function normalizePath(pathname) {
    if (!pathname || pathname === "/") return "/";
    return pathname.replace(/\/+$/, "") || "/";
  }

  function siteTab(page) {
    const nav = document.getElementById("site_nav");
    if (!nav || !Object.prototype.hasOwnProperty.call(routeForPage, page)) return null;
    return nav.querySelector(`a[data-value="${page}"]`);
  }

  function prepareRouteLinks(nav) {
    nav.querySelectorAll("a[data-value]").forEach((link) => {
      const route = routeForPage[link.dataset.value];
      if (!route) return;
      const tabTarget =
        link.getAttribute("data-bs-target") ||
        link.getAttribute("data-target") ||
        link.getAttribute("href");
      if (tabTarget && tabTarget.startsWith("#")) {
        link.setAttribute("data-bs-target", tabTarget);
        link.setAttribute("data-target", tabTarget);
      }
      link.setAttribute("href", route);
    });
  }

  function setPageMetadata(page) {
    document.documentElement.dataset.siteRoute = page;
    document.title = titleForPage[page];
    const nav = document.getElementById("site_nav");
    if (!nav) return;
    nav.querySelectorAll("a[data-value]").forEach((link) => {
      if (link.dataset.value === page) {
        link.setAttribute("aria-current", "page");
      } else {
        link.removeAttribute("aria-current");
      }
    });
  }

  function selectPage(page) {
    const link = siteTab(page);
    if (!link) return;
    setPageMetadata(page);
    if (link.getAttribute("aria-selected") !== "true" && !link.classList.contains("active")) {
      link.click();
    }
  }

  function isUnmodifiedPrimaryClick(event) {
    return (
      event.button === 0 &&
      !event.defaultPrevented &&
      !event.metaKey &&
      !event.ctrlKey &&
      !event.shiftKey &&
      !event.altKey
    );
  }

  function writeRoute(page) {
    const target = routeForPage[page];
    if (!target) return;
    setPageMetadata(page);
    const targetUrl = `${target}${window.location.search}`;
    if (normalizePath(window.location.pathname) === target) {
      if (window.location.pathname !== target || window.location.hash) {
        window.history.replaceState({ ena3dPage: page }, "", targetUrl);
      }
      return;
    }
    window.history.pushState({ ena3dPage: page }, "", targetUrl);
  }

  function navigateToPage(page) {
    if (!Object.prototype.hasOwnProperty.call(routeForPage, page)) return;
    selectPage(page);
    writeRoute(page);
  }

  function handleSitePageLink(event) {
    if (!isUnmodifiedPrimaryClick(event) || !(event.target instanceof Element)) {
      return;
    }

    const link = event.target.closest("a[data-site-page]");
    if (!link || (link.target && link.target !== "_self") || link.hasAttribute("download")) {
      return;
    }

    const page = link.dataset.sitePage;
    const target = routeForPage[page];
    if (!target) return;

    const linkUrl = new URL(link.href, window.location.href);
    if (
      linkUrl.origin !== window.location.origin ||
      normalizePath(linkUrl.pathname) !== target
    ) {
      return;
    }

    event.preventDefault();
    navigateToPage(page);
  }

  function syncFromLocation() {
    const path = normalizePath(window.location.pathname);
    const page = pageForRoute[path] || "home";
    const canonicalPath = routeForPage[page];
    if (window.location.pathname !== canonicalPath || window.location.hash) {
      window.history.replaceState(
        { ena3dPage: page },
        "",
        `${canonicalPath}${window.location.search}`
      );
    }
    selectPage(page);
  }

  function initializeSiteRoutes() {
    const nav = document.getElementById("site_nav");
    if (!nav) return;

    prepareRouteLinks(nav);
    document.addEventListener("click", handleSitePageLink);
    nav.addEventListener("click", (event) => {
      const link = event.target.closest('a[data-value]');
      if (!link || !nav.contains(link)) return;
      window.setTimeout(() => writeRoute(link.dataset.value), 0);
    });
    nav.addEventListener("shown.bs.tab", (event) => {
      const link = event.target.closest('a[data-value]');
      if (link && nav.contains(link)) writeRoute(link.dataset.value);
    });
    window.addEventListener("popstate", syncFromLocation);
    document.addEventListener("shiny:connected", syncFromLocation);
    syncFromLocation();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeSiteRoutes, { once: true });
  } else {
    initializeSiteRoutes();
  }
})();
