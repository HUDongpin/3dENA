(function () {
  "use strict";

  document.documentElement.dataset.siteMode = "stateful";
  document.documentElement.dataset.siteRoute = "tool";

  const workspace = new URLSearchParams(window.location.search).get("workspace");
  if (workspace !== "trajectory") return;

  let sent = false;
  let retryTimer = 0;

  function stop() {
    window.clearInterval(retryTimer);
    if (window.jQuery) {
      window.jQuery(document).off("shiny:connected.ena3dWorkspaceEntry");
    }
    document.removeEventListener("shiny:connected", sendWorkspaceEntry);
    window.removeEventListener("pagehide", stop);
  }

  function sendWorkspaceEntry() {
    if (sent || typeof window.Shiny?.setInputValue !== "function") return;
    const socket = window.Shiny.shinyapp?.$socket;
    if (!socket || socket.readyState !== WebSocket.OPEN) return;
    sent = true;
    window.Shiny.setInputValue(
      "ena3d_workspace_entry",
      workspace,
      { priority: "event" }
    );
    stop();
  }

  if (window.jQuery) {
    window
      .jQuery(document)
      .on("shiny:connected.ena3dWorkspaceEntry", sendWorkspaceEntry);
  }
  document.addEventListener("shiny:connected", sendWorkspaceEntry);
  retryTimer = window.setInterval(sendWorkspaceEntry, 250);
  window.addEventListener("pagehide", stop, { once: true });
  sendWorkspaceEntry();
})();
