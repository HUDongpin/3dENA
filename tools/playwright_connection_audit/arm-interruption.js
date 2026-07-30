/* global document, window */

async (page) => {
  const state = page.__ena3dConnectionAudit;
  if (!state) throw new Error("The transport monitor is unavailable.");
  if (state.pendingInterruption) {
    throw new Error("A transport interruption is already pending.");
  }
  if (
    state.liveSockets.size !== 1 ||
    state.opened - state.closed !== 1
  ) {
    throw new Error("The audit requires exactly one live WebSocket.");
  }

  const connection = await page.evaluate(() => {
    const app = window.Shiny && window.Shiny.shinyapp;
    const guard = document.getElementById("ena3d-connection-guard");
    return {
      connected: Boolean(app && app.$socket && app.$socket.readyState === 1),
      guardConnected: Boolean(
        guard && guard.hidden && guard.dataset.state === "connected"
      ),
    };
  });
  if (!connection.connected || !connection.guardConnected) {
    throw new Error("The connection audit was not armed from a ready session.");
  }

  state.interruptionSerial += 1;
  state.pendingInterruption = {
    serial: state.interruptionSerial,
    opened: state.opened,
    closed: state.closed,
    roundTrips: state.roundTrips,
  };
  return {
    interruptionArmed: true,
    serial: state.pendingInterruption.serial,
    openedBefore: state.pendingInterruption.opened,
    closedBefore: state.pendingInterruption.closed,
    roundTripsBefore: state.pendingInterruption.roundTrips,
  };
}
