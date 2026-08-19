/* global document, window */

async (page) => {
  await page.waitForFunction(
    () => {
      const app = window.Shiny && window.Shiny.shinyapp;
      const guard = document.getElementById("ena3d-connection-guard");
      return (
        app &&
        app.$socket &&
        app.$socket.readyState === 1 &&
        app.$allowReconnect === false &&
        guard &&
        guard.hidden &&
        guard.dataset.state === "connected" &&
        guard.dataset.sessionProof === "ready"
      );
    },
    null,
    { timeout: 60000 }
  );

  const state = page.__ena3dConnectionAudit;
  if (!state || state.opened < 1) {
    throw new Error("The Shiny transport was not observed.");
  }
  const baselineProof = await state.proveServerRoundTrip();
  state.baselineSessionId = baselineProof.sessionId;
  state.stableClosed = state.closed;
  return {
    appReady: true,
    baselineSessionProof: true,
    roundTrips: baselineProof.roundTrips,
  };
}
