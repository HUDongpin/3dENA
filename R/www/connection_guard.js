(function () {
  "use strict";

  if (window.__ena3dConnectionGuardInitialized) return;
  window.__ena3dConnectionGuardInitialized = true;

  const initialize = function () {
    const guard = document.getElementById("ena3d-connection-guard");
    const dialog = guard && guard.querySelector(".ena3d-connection-dialog");
    const title = document.getElementById("ena3d-connection-title");
    const message = document.getElementById("ena3d-connection-message");
    const reload = document.getElementById("ena3d-connection-reload");
    const live = document.getElementById("ena3d-connection-live");
    if (!guard || !dialog || !title || !message || !reload || !live) return;

    let showTimer = null;
    let proofTimer = null;
    let proofRetryTimer = null;
    let announcementTimer = null;
    let restoreFocusTo = null;
    let disconnected = false;
    let hostReconnectActive = false;
    let hasConnected = false;
    let verifiedSessionId = null;
    let expectedSessionId = null;
    let pendingProof = null;
    let proofSerial = 0;
    let connectionEpoch = 0;
    let terminalState = null;
    let proofHandlerRegistered = false;
    const inertState = new Map();

    const delayFromAttribute = function (name, fallback) {
      const value = Number(guard.getAttribute(name));
      return Number.isFinite(value) && value >= 0 ? value : fallback;
    };

    const clearShowTimer = function () {
      window.clearTimeout(showTimer);
      showTimer = null;
    };

    const clearAnnouncement = function () {
      window.clearTimeout(announcementTimer);
      announcementTimer = null;
      live.textContent = "";
    };

    const clearProof = function () {
      window.clearTimeout(proofTimer);
      window.clearTimeout(proofRetryTimer);
      proofTimer = null;
      proofRetryTimer = null;
      pendingProof = null;
      proofSerial += 1;
    };

    const announce = function (copy, epoch) {
      clearAnnouncement();
      announcementTimer = window.setTimeout(function () {
        if (
          !disconnected &&
          !terminalState &&
          epoch === connectionEpoch
        ) {
          live.textContent = copy;
        }
      }, 0);
    };

    const elementContainsGuard = function (element) {
      return (
        element === guard ||
        element === live ||
        element.contains(guard) ||
        element.contains(live)
      );
    };

    const setBackgroundInert = function (blocked) {
      if (blocked) {
        Array.from(document.body.children).forEach(function (element) {
          if (
            !(element instanceof HTMLElement) ||
            elementContainsGuard(element)
          ) {
            return;
          }
          if (!inertState.has(element)) {
            inertState.set(element, element.hasAttribute("inert"));
          }
          element.inert = true;
        });
        return;
      }

      Array.from(inertState.entries()).forEach(function (entry) {
        const element = entry[0];
        const wasInert = entry[1];
        if (!wasInert) {
          element.inert = false;
          element.removeAttribute("inert");
        }
        inertState.delete(element);
      });
    };

    const reveal = function () {
      if (!disconnected) return;
      const wasHidden = guard.hidden;
      guard.hidden = false;
      guard.setAttribute("aria-hidden", "false");
      document.documentElement.classList.add("ena3d-connection-blocked");
      setBackgroundInert(true);
      if (wasHidden) {
        dialog.focus({ preventScroll: true });
      }
    };

    const hide = function () {
      clearShowTimer();
      clearProof();
      guard.hidden = true;
      guard.setAttribute("aria-hidden", "true");
      guard.setAttribute("data-state", "connected");
      document.documentElement.classList.remove("ena3d-connection-blocked");
      setBackgroundInert(false);
      if (
        restoreFocusTo &&
        document.contains(restoreFocusTo) &&
        typeof restoreFocusTo.focus === "function"
      ) {
        restoreFocusTo.focus({ preventScroll: true });
      }
      restoreFocusTo = null;
    };

    const setReconnectingCopy = function (copy) {
      if (terminalState) return;
      guard.setAttribute("data-state", "reconnecting");
      title.textContent = "Connection interrupted";
      message.textContent = copy || (
        navigator.onLine === false
          ? "This device appears to be offline. The server is holding your current analysis session while the network returns."
          : "Trying to reconnect to your existing analysis session. Keep this page open."
      );
    };

    const setVerifyingCopy = function (attempt) {
      if (terminalState) return;
      guard.setAttribute("data-state", "verifying");
      title.textContent = "Verifying your session";
      message.textContent = attempt > 1
        ? "The connection is open, but the current analysis is still busy. Waiting for the original session to confirm its identity."
        : "The connection reopened. Checking that your original uploads and analysis state are still attached.";
      reveal();
    };

    const setFailedCopy = function (state, copy) {
      clearShowTimer();
      clearProof();
      clearAnnouncement();
      disconnected = true;
      terminalState = state;
      guard.setAttribute("data-state", state);
      title.textContent = state === "session-replaced"
        ? "Original session was not restored"
        : "Session connection ended";
      message.textContent = copy;
      reveal();
      reload.focus({ preventScroll: true });
    };

    const beginInterruption = function () {
      if (terminalState) return connectionEpoch;
      if (!disconnected) {
        restoreFocusTo = document.activeElement;
      }
      disconnected = true;
      connectionEpoch += 1;
      expectedSessionId = verifiedSessionId;
      clearShowTimer();
      clearProof();
      clearAnnouncement();
      setReconnectingCopy();
      showTimer = window.setTimeout(
        reveal,
        delayFromAttribute("data-show-delay-ms", 300)
      );
      return connectionEpoch;
    };

    const newNonce = function () {
      if (window.crypto && typeof window.crypto.randomUUID === "function") {
        return window.crypto.randomUUID();
      }
      return [
        Date.now().toString(36),
        Math.random().toString(36).slice(2),
        Math.random().toString(36).slice(2),
      ].join("-");
    };

    const requestProof = function (purpose, epoch, attempt) {
      if (terminalState || epoch !== connectionEpoch) return;
      if (purpose === "initial" && (disconnected || verifiedSessionId)) return;
      if (purpose !== "initial" && !disconnected) return;

      clearProof();
      const serial = proofSerial + 1;
      proofSerial = serial;
      const nonce = newNonce();
      pendingProof = {
        nonce: nonce,
        purpose: purpose,
        epoch: epoch,
        attempt: attempt,
        serial: serial,
      };

      proofTimer = window.setTimeout(function () {
        if (!pendingProof || pendingProof.serial !== serial) return;
        const timedOut = pendingProof;
        clearProof();
        if (
          terminalState ||
          timedOut.epoch !== connectionEpoch ||
          (timedOut.purpose === "initial" && disconnected)
        ) {
          return;
        }

        if (timedOut.purpose !== "initial") {
          setVerifyingCopy(timedOut.attempt + 1);
        }
        proofRetryTimer = window.setTimeout(function () {
          requestProof(
            timedOut.purpose,
            timedOut.epoch,
            timedOut.attempt + 1
          );
        }, delayFromAttribute("data-proof-retry-ms", 1000));
      }, delayFromAttribute("data-proof-timeout-ms", 8000));

      // Shiny fires shiny:connected before it sends the init message. Deferring
      // this input prevents the proof update from overtaking that init message.
      window.setTimeout(function () {
        if (
          !pendingProof ||
          pendingProof.serial !== serial ||
          !window.Shiny ||
          typeof window.Shiny.setInputValue !== "function"
        ) {
          return;
        }
        window.Shiny.setInputValue(
          "ena3d_connection_probe",
          { nonce: nonce },
          { priority: "event" }
        );
      }, 0);
    };

    const onSessionProof = function (proof) {
      if (
        !pendingProof ||
        !proof ||
        typeof proof.nonce !== "string" ||
        typeof proof.session_id !== "string" ||
        !/^[a-f0-9]{64}$/.test(proof.session_id) ||
        proof.nonce !== pendingProof.nonce
      ) {
        return;
      }

      const accepted = pendingProof;
      const sessionId = proof.session_id;
      clearProof();
      if (
        terminalState ||
        accepted.epoch !== connectionEpoch
      ) {
        return;
      }

      if (accepted.purpose === "initial") {
        if (disconnected) return;
        verifiedSessionId = sessionId;
        expectedSessionId = sessionId;
        guard.setAttribute("data-session-proof", "ready");
        return;
      }

      if (!expectedSessionId) {
        setFailedCopy(
          "failed",
          "The original session identity was not established before the interruption, so this page cannot safely claim that uploads or results were restored. Reload before continuing."
        );
        return;
      }

      if (sessionId === expectedSessionId) {
        verifiedSessionId = sessionId;
        terminalState = null;
        disconnected = false;
        hide();
        announce(
          "Connection restored. Your existing analysis session is unchanged.",
          accepted.epoch
        );
        return;
      }

      setFailedCopy(
        "session-replaced",
        "The server opened a new session. Previous uploads, results, and running calculations were not restored. Reload and upload the data again."
      );
    };

    const registerProofHandler = function () {
      if (
        proofHandlerRegistered ||
        !window.Shiny ||
        typeof window.Shiny.addCustomMessageHandler !== "function"
      ) {
        return proofHandlerRegistered;
      }
      window.Shiny.addCustomMessageHandler(
        "ena3d-session-proof",
        onSessionProof
      );
      proofHandlerRegistered = true;
      return true;
    };

    const requestInitialProof = function () {
      if (
        verifiedSessionId ||
        disconnected ||
        terminalState ||
        pendingProof ||
        proofRetryTimer
      ) {
        return;
      }
      if (!registerProofHandler()) {
        window.setTimeout(requestInitialProof, 100);
        return;
      }
      requestProof("initial", connectionEpoch, 1);
    };

    const beginRecoveryVerification = function (purpose) {
      if (terminalState) return;
      if (!disconnected) {
        beginInterruption();
      }
      setVerifyingCopy(1);
      requestProof(purpose, connectionEpoch, 1);
    };

    const onConnected = function () {
      registerProofHandler();
      if (!hasConnected) {
        hasConnected = true;
        requestInitialProof();
        return;
      }
      if (terminalState) return;
      beginRecoveryVerification("shiny-reconnect");
    };

    const onDisconnected = function () {
      if (terminalState) return;
      if (!disconnected) {
        beginInterruption();
      }
      setFailedCopy(
        "failed",
        "The original server session is no longer reachable. Reload the page to start a new session before continuing."
      );
    };

    const elementIsVisible = function (element) {
      return Boolean(
        element &&
        !element.hidden &&
        window.getComputedStyle(element).display !== "none" &&
        window.getComputedStyle(element).visibility !== "hidden"
      );
    };

    const readHostReconnectState = function () {
      const hostDialog = document.getElementById("ss-connect-dialog");
      if (!elementIsVisible(hostDialog)) return "hidden";
      const reloadLink = document.getElementById("ss-reload-link");
      if (
        elementIsVisible(reloadLink) ||
        /disconnected from the server/i.test(hostDialog.textContent || "")
      ) {
        return "failed";
      }
      return "reconnecting";
    };

    let lastHostState = "hidden";
    const syncHostReconnectState = function () {
      if (!guard.hidden) {
        // Shiny may append its native overlay after this guard is shown.
        // Keep every newly-added background sibling non-interactive too.
        setBackgroundInert(true);
      }

      const state = readHostReconnectState();
      if (state === lastHostState) return;
      lastHostState = state;
      if (terminalState) return;

      if (state === "reconnecting") {
        hostReconnectActive = true;
        beginInterruption();
        return;
      }
      if (state === "failed") {
        hostReconnectActive = false;
        if (!disconnected) beginInterruption();
        setFailedCopy(
          "failed",
          "The server could not reconnect to your existing analysis session. Reload before continuing."
        );
        return;
      }
      if (hostReconnectActive) {
        hostReconnectActive = false;
        beginRecoveryVerification("host-reconnect");
      }
    };

    reload.addEventListener("click", function () {
      window.location.reload();
    });

    dialog.addEventListener("keydown", function (event) {
      if (event.key === "Tab") {
        event.preventDefault();
        reload.focus({ preventScroll: true });
      }
    });
    document.addEventListener("focusin", function (event) {
      if (!guard.hidden && !dialog.contains(event.target)) {
        reload.focus({ preventScroll: true });
      }
    });

    window.addEventListener("offline", function () {
      if (disconnected && !terminalState) {
        setReconnectingCopy();
        reveal();
      }
    });
    window.addEventListener("online", function () {
      if (
        disconnected &&
        !terminalState &&
        guard.getAttribute("data-state") === "reconnecting"
      ) {
        message.textContent =
          "Network access returned. Reconnecting to your existing analysis session.";
      }
    });

    registerProofHandler();
    if (window.jQuery) {
      window.jQuery(document)
        .off(".ena3dConnectionGuard")
        .on("shiny:disconnected.ena3dConnectionGuard", onDisconnected)
        .on("shiny:connected.ena3dConnectionGuard", onConnected);
    }

    const hostObserver = new MutationObserver(syncHostReconnectState);
    hostObserver.observe(document.body, {
      attributes: true,
      attributeFilter: ["class", "style", "hidden"],
      childList: true,
      subtree: true,
    });
    syncHostReconnectState();

    if (
      window.Shiny &&
      window.Shiny.initializedPromise &&
      typeof window.Shiny.initializedPromise.then === "function"
    ) {
      window.Shiny.initializedPromise.then(function () {
        hasConnected = true;
        requestInitialProof();
      });
    }
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, { once: true });
  } else {
    initialize();
  }
})();
