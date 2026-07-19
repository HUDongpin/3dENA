# Page-scoped UI for evidence-grounded Qwen interpretation of ENA results.
#
# Public integration contract
# ---------------------------
# Add `ai_interpretation_ui("main_app_ai")` inside the 3D ENA tool tab only.
# The server module uses the following namespaced inputs:
#   toggle, close, mode, language, research_context, consent,
#   preview_toggle, interpret, cancel
# and the following namespaced outputs:
#   scope, status_summary, stale_notice, preview, disabled_message,
#   error_message, result_meta, result_headline, result_claims,
#   result_evidence, result_caveats, result_alternatives, result_next_checks.
#
# Model-derived prose deliberately uses textOutput()/verbatimTextOutput(). This
# keeps Qwen text escaped; do not replace these bindings with uiOutput() or
# htmlOutput(). The server may change the visible state with:
#
#   session$sendCustomMessage(
#     "ena3d-ai-interpretation-state",
#     list(id = session$ns("root"), state = "loading", stale = FALSE)
#   )
#
# Allowed states are idle, loading, ready, error, and disabled. `open = TRUE`
# or `open = FALSE` may be included in the same message.

.ena3d_ai_text_output <- function(output_id, class = NULL, inline = FALSE) {
  output <- shiny::textOutput(output_id, inline = inline)
  if (!is.null(class)) {
    output <- htmltools::tagAppendAttributes(output, class = class)
  }
  output
}

.ena3d_ai_safe_context_limit <- function(value) {
  if (length(value) != 1L || is.na(value) || !is.numeric(value) ||
      !is.finite(value) || value != as.integer(value)) {
    stop("`context_max_chars` must be one whole number.", call. = FALSE)
  }

  value <- as.integer(value)
  if (value < 100L || value > 5000L) {
    stop("`context_max_chars` must be between 100 and 5000.", call. = FALSE)
  }
  value
}

ai_interpretation_ui <- function(
    id,
    context_max_chars = 1500L,
    stylesheet_version = NULL) {
  if (length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("`id` must be one non-empty string.", call. = FALSE)
  }

  context_max_chars <- .ena3d_ai_safe_context_limit(context_max_chars)
  ns <- shiny::NS(id)

  stylesheet_href <- "ai_interpretation.css"
  if (!is.null(stylesheet_version)) {
    if (length(stylesheet_version) != 1L || is.na(stylesheet_version)) {
      stop("`stylesheet_version` must be NULL or one value.", call. = FALSE)
    }
    stylesheet_href <- paste0(
      stylesheet_href,
      "?v=",
      utils::URLencode(as.character(stylesheet_version), reserved = TRUE)
    )
  }

  interpret_button <- htmltools::tagAppendAttributes(
    shiny::actionButton(
      ns("interpret"),
      "Interpret ENA results",
      icon = shiny::tags$i("💬", `aria-hidden` = "true"),
      class = "ena-ai-interpret-button"
    ),
    disabled = "disabled",
    `aria-disabled` = "true",
    `aria-describedby` = paste(
      ns("preview_requirement"), ns("privacy_notice")
    )
  )

  context_input <- shiny::textAreaInput(
      ns("research_context"),
      "Research context (optional)",
      value = "",
      rows = 4L,
      width = "100%",
      resize = "vertical",
      placeholder = paste(
        "For example: study question, group definitions, or analytical",
        "constraints. Do not enter names or other identifying information."
      )
    )
  context_input <- htmltools::tagQuery(context_input)$
    find("textarea")$
    addAttrs(
    maxlength = context_max_chars,
    autocomplete = "off",
    spellcheck = "true",
    `aria-describedby` = ns("research_context_help"),
    `data-max-chars` = context_max_chars
    )$
    allTags()
  context_input <- htmltools::tagAppendAttributes(
    context_input,
    `data-ena-ai-context` = "true"
  )

  consent_input <- shiny::checkboxInput(
    ns("consent"),
    paste(
      "I reviewed this exact provider data envelope and consent to send it",
      "for this one interpretation request."
    ),
    value = FALSE
  )
  consent_input <- htmltools::tagQuery(consent_input)$
    find("input")$
    addAttrs(
      disabled = "disabled",
      `aria-disabled` = "true",
      `aria-describedby` = paste(
        ns("preview_requirement"), ns("privacy_notice")
      )
    )$
    allTags()
  consent_input <- htmltools::tagAppendAttributes(
    consent_input,
    `data-ena-ai-consent` = "true"
  )

  mode_input <- htmltools::tagAppendAttributes(
    shiny::radioButtons(
      ns("mode"),
      "Interpretation mode",
      choices = c(
        "Quick" = "quick",
        "Deep" = "deep",
        "Challenge" = "challenge"
      ),
      selected = "quick",
      inline = TRUE
    ),
    class = "ena-ai-mode",
    `aria-describedby` = ns("mode_help")
  )

  language_input <- htmltools::tagAppendAttributes(
    shiny::radioButtons(
      ns("language"),
      "Output language",
      choices = c("English" = "en", "中文" = "zh"),
      selected = "en",
      inline = TRUE
    ),
    class = "ena-ai-language"
  )

  close_button <- shiny::actionButton(
    ns("close"),
    label = shiny::tagList(
      shiny::tags$span(`aria-hidden` = "true", "\u00d7"),
      shiny::tags$span(class = "ena-ai-sr-only", "Close interpretation panel")
    ),
    class = "ena-ai-close",
    `aria-label` = "Close AI interpretation panel"
  )

  preview_button <- shiny::actionButton(
    ns("preview_toggle"),
    "Review exact provider data envelope",
    class = "ena-ai-preview-toggle",
    `aria-expanded` = "false",
    `aria-controls` = ns("preview_panel")
  )

  safe_output <- function(name, class = "ena-ai-output-text", inline = FALSE) {
    .ena3d_ai_text_output(ns(name), class = class, inline = inline)
  }

  client_controller <- shiny::tags$script(
    type = "text/javascript",
    htmltools::HTML("(function () {
      'use strict';

      const script = document.currentScript;
      const root = script && script.closest('[data-ena-ai-root]');
      if (!root || root.dataset.enaAiReady === 'true') return;
      root.dataset.enaAiReady = 'true';

      const toggle = root.querySelector('[data-ena-ai-toggle]');
      const drawer = root.querySelector('[data-ena-ai-drawer]');
      const backdrop = root.querySelector('[data-ena-ai-backdrop]');
      const closeButton = root.querySelector('[data-ena-ai-close]');
      const consentContainer = root.querySelector('[data-ena-ai-consent]');
      const consent = consentContainer && (
        consentContainer.matches('input[type=\"checkbox\"]') ? consentContainer :
          consentContainer.querySelector('input[type=\"checkbox\"]')
      );
      const interpret = root.querySelector('[data-ena-ai-interpret]');
      const contextContainer = root.querySelector('[data-ena-ai-context]');
      const context = contextContainer && (
        contextContainer.matches('textarea') ? contextContainer :
          contextContainer.querySelector('textarea')
      );
      const contextCount = root.querySelector('[data-ena-ai-context-count]');
      const previewToggle = root.querySelector('[data-ena-ai-preview-toggle]');
      const previewPanel = root.querySelector('[data-ena-ai-preview-panel]');
      const heading = root.querySelector('[data-ena-ai-heading]');
      let restoreFocus = null;

      const focusableSelector = [
        'a[href]', 'button:not([disabled])', 'input:not([disabled])',
        'select:not([disabled])', 'textarea:not([disabled])',
        '[tabindex]:not([tabindex=\"-1\"])'
      ].join(',');

      const syncInterpretAvailability = function () {
        if (!consent || !interpret) return;
        const unavailable = root.dataset.state === 'loading' ||
          root.dataset.state === 'disabled';
        const previewReady = root.dataset.previewReady === 'true';
        const consentReady = root.dataset.consentReady === 'true';
        consent.disabled = unavailable || !previewReady;
        consent.setAttribute(
          'aria-disabled', consent.disabled ? 'true' : 'false'
        );
        const enabled = consent.checked && previewReady && consentReady &&
          !unavailable;
        interpret.disabled = !enabled;
        interpret.setAttribute('aria-disabled', enabled ? 'false' : 'true');
      };

      const updateContextCount = function () {
        if (!context || !contextCount) return;
        const maximum = Number(context.getAttribute('maxlength')) || 0;
        contextCount.textContent = context.value.length + ' / ' + maximum;
      };

      const setOpen = function (open) {
        if (!drawer || !toggle || !backdrop) return;
        drawer.classList.toggle('is-open', open);
        backdrop.classList.toggle('is-open', open);
        drawer.setAttribute('aria-hidden', open ? 'false' : 'true');
        toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
        document.body.classList.toggle('ena-ai-drawer-open', open);

        if (open) {
          restoreFocus = document.activeElement;
          window.requestAnimationFrame(function () {
            if (heading) heading.focus();
          });
        } else if (restoreFocus && restoreFocus.isConnected) {
          restoreFocus.focus();
          restoreFocus = null;
        }
      };
      root._enaAiSetOpen = setOpen;

      if (toggle) toggle.addEventListener('click', function () { setOpen(true); });
      if (closeButton) closeButton.addEventListener('click', function () { setOpen(false); });
      if (backdrop) backdrop.addEventListener('click', function () { setOpen(false); });
      if (consent) consent.addEventListener('change', function () {
        root.dataset.consentReady = 'false';
        syncInterpretAvailability();
      });
      if (context) context.addEventListener('input', updateContextCount);

      if (previewToggle && previewPanel) {
        previewToggle.addEventListener('click', function () {
          const willOpen = previewPanel.hidden;
          previewPanel.hidden = !willOpen;
          previewToggle.setAttribute('aria-expanded', willOpen ? 'true' : 'false');
        });
      }

      if (drawer) {
        drawer.addEventListener('keydown', function (event) {
          if (event.key === 'Escape') {
            event.preventDefault();
            setOpen(false);
            return;
          }
          if (event.key !== 'Tab') return;

          const focusable = Array.from(drawer.querySelectorAll(focusableSelector))
            .filter(function (element) {
              return element.getClientRects().length > 0 &&
                element.getAttribute('aria-hidden') !== 'true';
            });
          if (!focusable.length) return;
          const first = focusable[0];
          const last = focusable[focusable.length - 1];
          if (event.shiftKey && document.activeElement === first) {
            event.preventDefault();
            last.focus();
          } else if (!event.shiftKey && document.activeElement === last) {
            event.preventDefault();
            first.focus();
          }
        });
      }

      const installStateHandler = function () {
        if (!window.Shiny || window.ena3dAiStateHandlerInstalled) return;
        window.ena3dAiStateHandlerInstalled = true;
        window.Shiny.addCustomMessageHandler(
          'ena3d-ai-interpretation-state',
          function (message) {
            if (!message || typeof message.id !== 'string') return;
            const target = document.getElementById(message.id);
            if (!target || !target.matches('[data-ena-ai-root]')) return;

            const allowed = ['idle', 'loading', 'ready', 'error', 'disabled'];
            if (allowed.indexOf(message.state) !== -1) {
              target.dataset.state = message.state;
            }
            if (typeof message.stale === 'boolean') {
              target.dataset.stale = message.stale ? 'true' : 'false';
            }
            if (typeof message.preview_ready === 'boolean') {
              target.dataset.previewReady = message.preview_ready ?
                'true' : 'false';
            }
            if (typeof message.consent_ready === 'boolean') {
              target.dataset.consentReady = message.consent_ready ?
                'true' : 'false';
            }
            if (typeof message.open === 'boolean' && target._enaAiSetOpen) {
              target._enaAiSetOpen(message.open);
            }
            const targetConsentContainer = target.querySelector(
              '[data-ena-ai-consent]'
            );
            const targetConsent = targetConsentContainer && (
              targetConsentContainer.matches('input[type=\"checkbox\"]') ?
                targetConsentContainer :
                targetConsentContainer.querySelector('input[type=\"checkbox\"]')
            );
            const targetInterpret = target.querySelector('[data-ena-ai-interpret]');
            const targetPreviewToggle = target.querySelector(
              '[data-ena-ai-preview-toggle]'
            );
            const targetPreviewPanel = target.querySelector(
              '[data-ena-ai-preview-panel]'
            );
            if (targetConsent && targetInterpret) {
              const unavailable = target.dataset.state === 'loading' ||
                target.dataset.state === 'disabled';
              const previewReady = target.dataset.previewReady === 'true';
              const consentReady = target.dataset.consentReady === 'true';
              if (!previewReady && targetConsent.checked) {
                targetConsent.checked = false;
                targetConsent.dispatchEvent(new Event('change', {
                  bubbles: true
                }));
              }
              targetConsent.disabled = unavailable || !previewReady;
              targetConsent.setAttribute(
                'aria-disabled', targetConsent.disabled ? 'true' : 'false'
              );
              const enabled = targetConsent.checked && previewReady &&
                consentReady && !unavailable;
              targetInterpret.disabled = !enabled;
              targetInterpret.setAttribute(
                'aria-disabled', enabled ? 'false' : 'true'
              );
            }
            if (target.dataset.previewReady !== 'true' && targetPreviewPanel) {
              targetPreviewPanel.hidden = true;
              if (targetPreviewToggle) {
                targetPreviewToggle.setAttribute('aria-expanded', 'false');
              }
            }
          }
        );
      };

      installStateHandler();
      document.addEventListener('shiny:connected', installStateHandler, { once: true });
      document.addEventListener('shown.bs.tab', function () {
        if (root.getClientRects().length === 0) setOpen(false);
      });
      updateContextCount();
      syncInterpretAvailability();
    })();")
  )

  shiny::tags$div(
    id = ns("root"),
    class = "ena-ai-root",
    `data-ena-ai-root` = "true",
    `data-state` = "idle",
    `data-stale` = "false",
    `data-preview-ready` = "false",
    `data-consent-ready` = "false",
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = stylesheet_href
      )
    ),
    htmltools::tagAppendAttributes(
      shiny::actionButton(
        ns("toggle"),
        "AI interpretation",
        icon = shiny::tags$i("💡", `aria-hidden` = "true"),
        class = "ena-ai-toggle"
      ),
      `data-ena-ai-toggle` = "true",
      `aria-expanded` = "false",
      `aria-controls` = ns("drawer"),
      `aria-haspopup` = "dialog"
    ),
    shiny::tags$div(
      id = ns("backdrop"),
      class = "ena-ai-backdrop",
      `data-ena-ai-backdrop` = "true",
      `aria-hidden` = "true"
    ),
    shiny::tags$aside(
      id = ns("drawer"),
      class = "ena-ai-drawer",
      role = "dialog",
      `aria-modal` = "true",
      `aria-hidden` = "true",
      `aria-labelledby` = ns("title"),
      `aria-describedby` = ns("privacy_notice"),
      `data-ena-ai-drawer` = "true",
      tabindex = "-1",
      shiny::tags$header(
        class = "ena-ai-header",
        shiny::tags$div(
          class = "ena-ai-header-copy",
          shiny::tags$p(class = "ena-ai-kicker", "Evidence-grounded · Qwen Max"),
          shiny::tags$h2(
            id = ns("title"),
            class = "ena-ai-title",
            `data-ena-ai-heading` = "true",
            tabindex = "-1",
            "Interpret ENA results"
          ),
          shiny::tags$div(
            class = "ena-ai-scope-row",
            shiny::tags$span(class = "ena-ai-scope-label", "Scope"),
            safe_output("scope", class = "ena-ai-scope", inline = TRUE),
            safe_output(
              "status_summary",
              class = "ena-ai-status-summary",
              inline = TRUE
            )
          )
        ),
        htmltools::tagAppendAttributes(
          close_button,
          `data-ena-ai-close` = "true"
        )
      ),
      shiny::tags$div(
        class = "ena-ai-scroll-region",
        shiny::tags$div(
          id = ns("stale_state"),
          class = "ena-ai-stale-notice",
          role = "status",
          `aria-live` = "polite",
          shiny::tags$strong("Results changed"),
          safe_output("stale_notice", class = "ena-ai-stale-text")
        ),
        shiny::tags$section(
          class = "ena-ai-controls",
          `aria-labelledby` = ns("controls_title"),
          shiny::tags$h3(
            id = ns("controls_title"),
            class = "ena-ai-section-title",
            "Interpretation settings"
          ),
          mode_input,
          shiny::tags$div(
            id = ns("mode_help"),
            class = "ena-ai-mode-help",
            shiny::tags$span(
              shiny::tags$strong("Quick"),
              " summarizes the clearest evidence."
            ),
            shiny::tags$span(
              shiny::tags$strong("Deep"),
              " develops a fuller reading and may take longer."
            ),
            shiny::tags$span(
              shiny::tags$strong("Challenge"),
              " tests claims against caveats and rival explanations."
            )
          ),
          language_input,
          context_input,
          shiny::tags$div(
            id = ns("research_context_help"),
            class = "ena-ai-field-help",
            shiny::tags$span(
              "Keep this non-identifying. It is included verbatim in the request."
            ),
            shiny::tags$span(
              id = ns("research_context_count"),
              class = "ena-ai-context-count",
              role = "status",
              `aria-live` = "polite",
              `data-ena-ai-context-count` = "true",
              paste0("0 / ", context_max_chars)
            )
          ),
          htmltools::tagAppendAttributes(
            preview_button,
            `data-ena-ai-preview-toggle` = "true"
          ),
          shiny::tags$p(
            id = ns("preview_requirement"),
            class = "ena-ai-field-help ena-ai-preview-requirement",
            paste(
              "Review must succeed for the current analysis and options before",
              "consent is enabled. Any change requires a new review."
            )
          ),
          shiny::tags$section(
            id = ns("preview_panel"),
            class = "ena-ai-preview-panel",
            `aria-labelledby` = ns("preview_title"),
            `data-ena-ai-preview-panel` = "true",
            hidden = "hidden",
            shiny::tags$h4(
              id = ns("preview_title"),
              "Exact provider data envelope"
            ),
            shiny::tags$p(
              class = "ena-ai-field-help",
              paste(
                "This is the exact JSON data envelope supplied to Qwen.",
                "Transport headers, the API key, and the fixed system prompt",
                "are intentionally not part of this preview."
              )
            ),
            htmltools::tagAppendAttributes(
              shiny::verbatimTextOutput(ns("preview")),
              class = "ena-ai-request-preview",
              tabindex = "0"
            )
          ),
          shiny::tags$div(
            class = "ena-ai-consent-card",
            shiny::tags$p(
              id = ns("privacy_notice"),
              class = "ena-ai-privacy-notice",
              shiny::tags$strong("Before sending"),
              " The reviewed envelope contains only aggregate ENA evidence,",
              " interpretation options, and the optional context above. It is",
              " sent to Alibaba Cloud Qwen. Raw participant rows and unit",
              " identifiers remain in 3D ENA."
            ),
            consent_input
          )
        ),
        shiny::tags$div(
          class = "ena-ai-state-region",
          `aria-live` = "polite",
          `aria-atomic` = "false",
          shiny::tags$section(
            id = ns("empty_state"),
            class = "ena-ai-state ena-ai-empty-state",
            shiny::tags$h3("Ready when you are"),
            shiny::tags$p(
              "Choose a mode, review the exact current provider data envelope,",
              " then provide consent and interpret the ENA results."
            )
          ),
          shiny::tags$section(
            id = ns("disabled_state"),
            class = "ena-ai-state ena-ai-disabled-state",
            role = "status",
            shiny::tags$h3("AI interpretation is unavailable"),
            safe_output("disabled_message")
          ),
          shiny::tags$section(
            id = ns("loading_state"),
            class = "ena-ai-state ena-ai-loading-state",
            role = "status",
            shiny::tags$div(class = "ena-ai-spinner", `aria-hidden` = "true"),
            shiny::tags$div(
              shiny::tags$h3("Interpreting the evidence"),
              shiny::tags$p(
                "Qwen is working from the single-use data envelope you reviewed."
              )
            )
          ),
          shiny::tags$section(
            id = ns("error_state"),
            class = "ena-ai-state ena-ai-error-state",
            role = "alert",
            shiny::tags$h3("Interpretation could not be completed"),
            safe_output("error_message")
          ),
          shiny::tags$article(
            id = ns("result"),
            class = "ena-ai-state ena-ai-result",
            `aria-labelledby` = ns("result_title"),
            shiny::tags$div(
              class = "ena-ai-result-header",
              shiny::tags$p(class = "ena-ai-kicker", "AI-assisted interpretation"),
              shiny::tags$h3(id = ns("result_title"), "Current ENA reading"),
              safe_output("result_meta", class = "ena-ai-result-meta")
            ),
            shiny::tags$section(
              class = "ena-ai-result-section ena-ai-result-headline",
              shiny::tags$h4("What the results show"),
              safe_output("result_headline")
            ),
            shiny::tags$section(
              class = "ena-ai-result-section",
              shiny::tags$h4("Evidence-linked claims"),
              safe_output("result_claims")
            ),
            shiny::tags$section(
              class = "ena-ai-result-section",
              shiny::tags$h4("Evidence referenced"),
              safe_output("result_evidence")
            ),
            shiny::tags$section(
              class = "ena-ai-result-section",
              shiny::tags$h4("Caveats"),
              safe_output("result_caveats")
            ),
            shiny::tags$section(
              class = "ena-ai-result-section",
              shiny::tags$h4("Alternative explanations"),
              safe_output("result_alternatives")
            ),
            shiny::tags$section(
              class = "ena-ai-result-section",
              shiny::tags$h4("Recommended next checks"),
              safe_output("result_next_checks")
            ),
            shiny::tags$p(
              class = "ena-ai-disclaimer",
              "AI-generated interpretation can be incomplete or mistaken. Verify",
              " every claim against the displayed ENA evidence before reporting it."
            )
          )
        )
      ),
      shiny::tags$footer(
        class = "ena-ai-footer",
        htmltools::tagAppendAttributes(
          interpret_button,
          `data-ena-ai-interpret` = "true"
        ),
        shiny::actionButton(
          ns("cancel"),
          "Cancel request",
          class = "ena-ai-cancel-button"
        )
      )
    ),
    client_controller
  )
}
