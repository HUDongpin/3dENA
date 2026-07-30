/* global document */

async (page) => {
  const state = page.__ena3dConnectionAudit;
  if (!state || state.closed !== state.stableClosed) {
    throw new Error("The transport reset during the six-minute hold.");
  }

  const target = state.next;
  const selector = target === "tool" ? "#launch_ena_note" : "#home_brand";
  await page.locator(selector).click({ timeout: 10000 });
  await page.waitForFunction(
    (value) => {
      const tab = document.querySelector(
        `#site_nav a[data-value="${value}"]`
      );
      return tab && tab.getAttribute("aria-selected") === "true";
    },
    target,
    { timeout: 10000 }
  );

  state.next = target === "tool" ? "home" : "tool";
  const roundTrips = await state.proveServerRoundTrip();
  return { serverRoundTrip: true, roundTrips };
}
