/* global document */

async (page) => {
  const state = page.__ena3dConnectionAudit;
  if (!state) throw new Error("The transport monitor is unavailable.");
  if (!state.pendingInterruption) {
    throw new Error("The external transport interruption was not armed.");
  }

  const openedBefore = state.pendingInterruption.opened;
  const closedBefore = state.pendingInterruption.closed;
  const closeDeadline = Date.now() + 10000;
  while (state.closed <= closedBefore && Date.now() < closeDeadline) {
    await page.waitForTimeout(50);
  }
  if (state.closed <= closedBefore) {
    throw new Error("The external outage did not close the transport.");
  }

  const reconnectDeadline = Date.now() + 20000;
  while (
    (state.opened <= openedBefore || state.liveSockets.size !== 1) &&
    Date.now() < reconnectDeadline
  ) {
    await page.waitForTimeout(50);
  }
  if (state.opened <= openedBefore || state.liveSockets.size !== 1) {
    throw new Error("A single transport did not reopen in the recovery window.");
  }

  await page.waitForFunction(
    () => {
      const guard = document.getElementById("ena3d-connection-guard");
      const live = document.getElementById("ena3d-connection-live");
      return (
        guard &&
        guard.hidden &&
        guard.dataset.state === "connected" &&
        live &&
        live.textContent.trim() ===
          "Connection restored. Your existing analysis session is unchanged."
      );
    },
    null,
    { timeout: 20000 }
  );

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
  const roundTrips = await state.proveServerRoundTrip();
  state.stableClosed = state.closed;
  state.pendingInterruption = null;

  return {
    transportInterrupted: true,
    sameSessionRecovered: true,
    postRecoveryRoundTrip: true,
    roundTrips,
  };
}
