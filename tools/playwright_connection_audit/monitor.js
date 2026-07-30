/* global document, window */

async (page) => {
  const state = {
    opened: 0,
    closed: 0,
    liveSockets: new Set(),
    stableClosed: null,
    next: "tool",
    roundTrips: 0,
    interruptionSerial: 0,
    pendingInterruption: null,
  };
  state.proveServerRoundTrip = async () => {
    const proofAccepted = await page.evaluate(async () => {
      if (
        !window.Shiny ||
        typeof window.Shiny.setInputValue !== "function" ||
        !window.jQuery
      ) {
        throw new Error("Shiny is unavailable for the server proof.");
      }

      const nonce = `connection-audit-${window.crypto.randomUUID()}`;
      return new Promise((resolve, reject) => {
        const $document = window.jQuery(document);
        const eventName = "shiny:message.ena3dConnectionAudit";
        const cleanup = () => {
          window.clearTimeout(timeout);
          $document.off(eventName);
        };
        const timeout = window.setTimeout(() => {
          cleanup();
          reject(new Error("The server proof did not return in time."));
        }, 10000);

        $document.off(eventName).on(eventName, (event) => {
          const proof =
            event.message &&
            event.message.custom &&
            event.message.custom["ena3d-session-proof"];
          if (!proof || proof.nonce !== nonce) return;
          const validSessionProof =
            typeof proof.session_id === "string" &&
            /^[a-f0-9]{64}$/.test(proof.session_id);
          cleanup();
          resolve(validSessionProof);
        });
        window.Shiny.setInputValue(
          "ena3d_connection_probe",
          { nonce },
          { priority: "event" }
        );
      });
    });
    if (!proofAccepted) {
      throw new Error("The server returned an invalid session proof.");
    }
    state.roundTrips += 1;
    return state.roundTrips;
  };
  page.__ena3dConnectionAudit = state;
  page.on("websocket", (socket) => {
    state.opened += 1;
    state.liveSockets.add(socket);
    socket.on("close", () => {
      state.closed += 1;
      state.liveSockets.delete(socket);
    });
  });
  return { monitorReady: true };
}
