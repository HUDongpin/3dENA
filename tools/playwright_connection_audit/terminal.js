/* global document, window */

async (page) => {
  const state = page.__ena3dConnectionAudit;
  if (!state) throw new Error("The transport monitor is unavailable.");
  if (!state.pendingInterruption) {
    throw new Error("The external transport interruption was not armed.");
  }

  const closedBefore = state.pendingInterruption.closed;
  const closeDeadline = Date.now() + 10000;
  while (state.closed <= closedBefore && Date.now() < closeDeadline) {
    await page.waitForTimeout(50);
  }
  if (state.closed <= closedBefore) {
    throw new Error("The external outage did not close the transport.");
  }

  await page.waitForFunction(
    () => {
      const guard = document.getElementById("ena3d-connection-guard");
      const reload = document.getElementById("ena3d-connection-reload");
      const nativeReload = document.getElementById("ss-reload-link");
      const nativeReloadVisible = Boolean(
        nativeReload &&
        !nativeReload.hidden &&
        window.getComputedStyle(nativeReload).display !== "none" &&
        window.getComputedStyle(nativeReload).visibility !== "hidden"
      );
      return (
        guard &&
        !guard.hidden &&
        guard.dataset.state === "failed" &&
        reload &&
        nativeReloadVisible
      );
    },
    null,
    { timeout: 20000 }
  );

  const terminalState = await page.evaluate(() => {
    const app = window.Shiny && window.Shiny.shinyapp;
    const guard = document.getElementById("ena3d-connection-guard");
    const nativeReload = document.getElementById("ss-reload-link");
    const live = document.getElementById("ena3d-connection-live");
    const nativeReloadVisible = Boolean(
      nativeReload &&
      !nativeReload.hidden &&
      window.getComputedStyle(nativeReload).display !== "none" &&
      window.getComputedStyle(nativeReload).visibility !== "hidden"
    );
    return {
      allowReconnectDisabled: Boolean(app && app.$allowReconnect === false),
      guardFailed: Boolean(
        guard && !guard.hidden && guard.dataset.state === "failed"
      ),
      nativeReloadVisible,
      restoredAnnouncement:
        live &&
        live.textContent.trim() ===
          "Connection restored. Your existing analysis session is unchanged.",
    };
  });
  const noLiveTransport = state.liveSockets.size === 0;
  state.pendingInterruption = null;
  return {
    longInterruptionBlocked: terminalState.guardFailed,
    nativeDisconnectPreserved: terminalState.nativeReloadVisible,
    newSessionReplayPrevented: Boolean(
      terminalState.allowReconnectDisabled &&
      terminalState.guardFailed &&
      !terminalState.restoredAnnouncement &&
      noLiveTransport
    ),
    outageSeconds: 22,
  };
}
