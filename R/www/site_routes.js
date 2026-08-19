(function () {
  "use strict";

  const routeForPage = Object.freeze({
    home: "/",
    papers: "/papers",
    team: "/team",
    about: "/about",
  });
  const titleForPage = Object.freeze({
    home: "3D ENA | Epistemic Network Analysis",
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

  function pageFromLocation() {
    return pageForRoute[normalizePath(window.location.pathname)] || "home";
  }

  // Static pages publish their route without waiting for a Shiny lifecycle.
  document.documentElement.dataset.siteRoute = pageFromLocation();

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
    if (!Object.prototype.hasOwnProperty.call(routeForPage, page)) return;
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

  function writeCitationToClipboard(text) {
    const writeWithSelection = function () {
      return new Promise(function (resolve, reject) {
        const textarea = document.createElement("textarea");
        textarea.value = text;
        textarea.setAttribute("readonly", "");
        textarea.style.position = "fixed";
        textarea.style.opacity = "0";
        document.body.appendChild(textarea);
        textarea.select();
        const copied = document.execCommand("copy");
        textarea.remove();
        if (copied) resolve();
        else reject(new Error("Clipboard copy was not available."));
      });
    };
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text).catch(writeWithSelection);
    }
    return writeWithSelection();
  }

  function handleCitationCopy(event) {
    if (!(event.target instanceof Element)) return;
    const button = event.target.closest(".ena3d-copy-citation");
    if (!button) return;
    const citation = document.getElementById(button.getAttribute("data-citation-target"));
    if (!citation) return;
    const citationText =
      citation.getAttribute("data-citation-text") || citation.textContent.trim();
    const defaultAriaLabel = button.getAttribute("aria-label");
    writeCitationToClipboard(citationText)
      .then(function () {
        window.clearTimeout(button.ena3dCopyResetTimer);
        button.textContent = "Copied";
        button.setAttribute("aria-label", "APA citation copied");
        button.classList.add("is-copied");
        button.ena3dCopyResetTimer = window.setTimeout(function () {
          button.textContent = button.getAttribute("data-default-label") || "Copy APA";
          button.setAttribute("aria-label", defaultAriaLabel);
          button.classList.remove("is-copied");
        }, 2200);
      })
      .catch(function () {
        button.textContent = "Select citation to copy";
        button.setAttribute(
          "aria-label",
          "Clipboard copy unavailable; select the citation to copy"
        );
        citation.focus();
      });
  }

  function initializeSiteRoutes() {
    const nav = document.getElementById("site_nav");
    if (!nav) return;

    prepareRouteLinks(nav);
    document.addEventListener("click", handleSitePageLink);
    document.addEventListener("click", handleCitationCopy);
    nav.addEventListener("click", (event) => {
      if (!(event.target instanceof Element)) return;
      const link = event.target.closest("a[data-value]");
      if (!link || !nav.contains(link) || !routeForPage[link.dataset.value]) return;
      window.setTimeout(() => writeRoute(link.dataset.value), 0);
    });
    nav.addEventListener("shown.bs.tab", (event) => {
      if (!(event.target instanceof Element)) return;
      const link = event.target.closest("a[data-value]");
      if (link && nav.contains(link) && routeForPage[link.dataset.value]) {
        writeRoute(link.dataset.value);
      }
    });
    window.addEventListener("popstate", syncFromLocation);
    syncFromLocation();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeSiteRoutes, { once: true });
  } else {
    initializeSiteRoutes();
  }
})();
