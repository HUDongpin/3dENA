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

  const proof = await state.proveServerRoundTrip();
  const sameSessionRecovered = Boolean(
    state.baselineSessionId && proof.sessionId === state.baselineSessionId
  );
  if (!sameSessionRecovered) {
    throw new Error("The recovered transport belongs to a different R session.");
  }
  state.stableClosed = state.closed;
  state.pendingInterruption = null;

  return {
    transportInterrupted: true,
    sameSessionRecovered,
    postRecoveryRoundTrip: true,
    roundTrips: proof.roundTrips,
  };
}
