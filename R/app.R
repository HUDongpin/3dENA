.ena3d_source_started_at <- unname(proc.time()[["elapsed"]])

.ena3d_candidate_files <- sub(
  "^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)
)
.ena3d_frame_files <- unlist(lapply(sys.frames(), function(frame) {
  if (!is.null(frame$ofile)) as.character(frame$ofile) else character()
}), use.names = FALSE)
.ena3d_candidate_dirs <- unique(c(
  dirname(c(.ena3d_candidate_files, .ena3d_frame_files)),
  getwd(),
  file.path(getwd(), "R")
))
.ena3d_app_dir <- .ena3d_candidate_dirs[vapply(
  .ena3d_candidate_dirs,
  function(path) {
    file.exists(file.path(path, "app.R")) &&
      file.exists(file.path(path, "install_dependencies.R"))
  },
  logical(1)
)][1L]
if (is.na(.ena3d_app_dir)) {
  stop("Could not locate the 3D ENA application directory.", call. = FALSE)
}
.ena3d_app_dir <- normalizePath(.ena3d_app_dir, mustWork = TRUE)
.ena3d_project_root <- normalizePath(
  file.path(.ena3d_app_dir, ".."), mustWork = TRUE
)
.ena3d_source <- function(file) {
  source(file.path(.ena3d_app_dir, file), chdir = TRUE, local = FALSE)
}

if (!identical(Sys.getenv("VERCEL", unset = ""), "1")) {
  .ena3d_source('install_dependencies.R')
}

library(shiny)
.ena3d_source('inline_ui.R')
.ena3d_source('security_utils.R')
.ena3d_source('app_connection.R')
.ena3d_source('site_routes.R')
.ena3d_source('qwen_client.R')
.ena3d_source('ai_evidence.R')
.ena3d_source('color_list.R')
.ena3d_source('app_ui_plot_settings.R')
.ena3d_source('app_ui_trajectory.R')
.ena3d_source('app_ui_main_plot.R')
.ena3d_source('app_ui_model_tab.R')
.ena3d_source('app_ui_data_upload_tab.R')
.ena3d_source('app_ui_camera_position_panel.R')
.ena3d_source('app_ui_stats.R')
.ena3d_source('app_ui_ai_interpretation.R')
.ena3d_source('app_ui_site.R')
.ena3d_source('static_site.R')

# Vercel must see the container port before its cold-start deadline. The
# prebuilt public shell needs Shiny only, so defer the analysis namespaces and
# server modules until the first Shiny session is established.
.ena3d_analysis_runtime_loaded <- FALSE
ena3d_load_analysis_runtime <- function() {
  if (isTRUE(.ena3d_analysis_runtime_loaded)) return(invisible(FALSE))

  for (package in c("plotly", "data.table", "rENA")) {
    suppressPackageStartupMessages(
      library(package, character.only = TRUE)
    )
  }
  ena3d_register_plotly_resources()
  .ena3d_source('app_server.R')
  .ena3d_analysis_runtime_loaded <<- TRUE
  invisible(TRUE)
}


config = list()
config$sample_data_path = normalizePath(
  file.path(.ena3d_project_root, "sample_data"),
  mustWork = TRUE
)
config$data_limits = ena3d_data_limits()
config$build_id = ena3d_resolve_build_id()
# Keep module exports and structured logs aligned with the authoritative
# deployment provenance selected above.
Sys.setenv(ENA3D_BUILD_ID = config$build_id)
if (identical(Sys.getenv("VERCEL", unset = ""), "1")) {
  Sys.setenv(ENA3D_GIT_COMMIT = config$build_id)
}
config$app_version = Sys.getenv(
  "ENA3D_APP_VERSION",
  unset = trimws(readLines(
    file.path(.ena3d_project_root, "VERSION"),
    n = 1L,
    warn = FALSE
  ))
)
config$runtime_profile = ena3d_runtime_profile()
config$connection_policy = ena3d_connection_policy(config$runtime_profile)
config$shiny_server_version = ena3d_validate_runtime_host(
  config$runtime_profile
)
config$sample_files = ena3d_list_trusted_samples(config$sample_data_path)
config$sample_count = length(config$sample_files)
.ena3d_ai_disabled_config <- function() {
  list(
    enabled = FALSE,
    secret_configured = FALSE,
    available = FALSE,
    qwen_client_file = normalizePath(
      file.path(.ena3d_app_dir, "qwen_client.R"), mustWork = TRUE
    ),
    min_cell_n = 5L,
    top_n = 10L,
    context_max_chars = 1500L,
    timeout_seconds = 60,
    max_processes = 4L,
    max_requests_per_hour = 10L,
    max_evidence_bytes = 65536L
  )
}

# AI is an optional boundary. Validate its provider and local resource settings
# as one unit so a typo can only disable AI, never take down the ENA app.
config$ai = tryCatch({
  ai_config <- ena3d_qwen_config_from_env(load_secret = FALSE)
  ai_config$qwen_client_file <- normalizePath(
    file.path(.ena3d_app_dir, "qwen_client.R"), mustWork = TRUE
  )
  ai_config$min_cell_n <- as.integer(ena3d_env_number(
    "ENA3D_AI_MIN_CELL_N", 5, minimum = 2, maximum = 100
  ))
  ai_config$top_n <- as.integer(ena3d_env_number(
    "ENA3D_AI_TOP_N", 10, minimum = 1, maximum = 25
  ))
  ai_config$context_max_chars <- as.integer(ena3d_env_number(
    "ENA3D_AI_CONTEXT_MAX_CHARS", 1500, minimum = 100, maximum = 5000
  ))
  ai_config$max_processes <- as.integer(ena3d_env_number(
    "ENA3D_AI_MAX_CONCURRENT_JOBS", 4, minimum = 1, maximum = 16
  ))
  ai_config$max_requests_per_hour <- as.integer(ena3d_env_number(
    "ENA3D_AI_MAX_REQUESTS_PER_HOUR", 10, minimum = 1, maximum = 100
  ))
  ai_config$max_evidence_bytes <- as.integer(ena3d_env_number(
    "ENA3D_AI_MAX_EVIDENCE_BYTES", 65536, minimum = 4096, maximum = 262144
  ))
  ai_config$available <- isTRUE(ai_config$enabled) &&
    isTRUE(ai_config$secret_configured)
  ai_config
}, error = function(error) {
  ena3d_security_log(
    "ai_configuration_invalid",
    level = "WARN",
    fields = list(error_class = class(error)[[1L]])
  )
  .ena3d_ai_disabled_config()
})

config$health_path = file.path(
  tempdir(), paste0("ena3d-health-", Sys.getpid())
)
dir.create(config$health_path, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  list(
    status = "ok",
    app = "3D ENA",
    version = config$app_version,
    build = config$build_id,
    trusted_samples = config$sample_count,
    ai_enabled = isTRUE(config$ai$available),
    runtime_profile = config$runtime_profile,
    connection_policy = config$connection_policy,
    shiny_server_version = if (nzchar(config$shiny_server_version)) {
      config$shiny_server_version
    } else {
      NULL
    }
  ),
  path = file.path(config$health_path, "healthz.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)
suppressWarnings(try(shiny::removeResourcePath("ena3d-health"), silent = TRUE))
shiny::addResourcePath("ena3d-health", config$health_path)
suppressWarnings(try(shiny::removeResourcePath("ena3d-assets"), silent = TRUE))
shiny::addResourcePath(
  "ena3d-assets",
  file.path(.ena3d_project_root, "images")
)
ena3d_security_log(
  "app_start",
  fields = list(
    app_version = config$app_version,
    sample_count = config$sample_count,
    ai_enabled = isTRUE(config$ai$available),
    runtime_profile = config$runtime_profile,
    connection_policy = config$connection_policy,
    shiny_server_version = config$shiny_server_version,
    r_version = paste(R.version$major, R.version$minor, sep = ".")
  )
)
"
R6 class.
It is an object used to communicate data between modules.
"
ENA_3D_Server <- R6::R6Class("ENA_3D_Server",
                         public = list(
                           active_tab = NULL,
                           render_comparison = FALSE,
                           render_overall = FALSE,
                           render_unit_group_change_plot=FALSE,
                           render_network_plot=FALSE,
                           render_trajectory_plot=FALSE,
                           ena_obj=NULL,
                           color_list = color_list,
                           is_app_initialized = FALSE,
                           initialize = function() {}
                         )
)

app_ui <- function(){

  tagList(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$meta(
        name = "description",
        content = paste(
          "3D ENA is an interactive research environment for exploring",
          "Epistemic Network Analysis in three dimensions."
        )
      ),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:title", content = "3D ENA | See connections in three dimensions"),
      tags$meta(
        property = "og:description",
        content = "Explore knowledge structures, group comparisons, and longitudinal network trajectories."
      ),
      tags$meta(property = "og:url", content = "https://3dena.com/"),
      tags$meta(property = "og:image", content = "https://3dena.com/og.png"),
      tags$meta(name = "twitter:card", content = "summary_large_image"),
      ena3d_analytics_tags(),
      tags$link(
        rel = "icon",
        type = "image/svg+xml",
        href = "ena3d-assets/favicon.svg"
      ),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0(
          "app_shell.css?v=",
          utils::URLencode(
            paste(config$app_version, config$build_id, sep = "-"),
            reserved = TRUE
          )
        )
      ),
      ena3d_connection_assets(
        paste(config$app_version, config$build_id, sep = "-")
      ),
      tags$script(
        defer = NA,
        src = paste0(
          "/app_entry.js?v=",
          utils::URLencode(
            paste(config$app_version, config$build_id, sep = "-"),
            reserved = TRUE
          )
        )
      ),
      tags$script(
        "Shiny.addCustomMessageHandler('ena3d-plot-visibility', function(message) {
          const element = document.getElementById(message.id);
          if (!element) return;
          element.style.display = message.visible ? '' : 'none';
          $(element).trigger(message.visible ? 'shown' : 'hidden');
        });"
      )
    ),
    tags$style(type="text/css",
               ".recalculating {opacity: 1.0;}
                  .mysidebar .left-side .nav {--bs-nav-link-padding-x:0.2rem;font-size:var(--ena-type-ui)}
                  .mysidebar .left-side {padding:3px}
                  .mysidebar .left-side .nav a {text-align:center}
                  .mysidebar {
                        height:calc(100vh - 14rem);
                        min-height:36rem;
                  }
                  .mysidebar .left-side {height:100%}
                  .mysidebar .left-side .nav {     
                        align-items: center;
                        justify-content: space-around;
                        display: flex;
                        height: 100%;
                        max-height:60vh;
                   }
                   .mysidebar .right-side {
                        overflow: scroll;
                        height: 100%;
                        opacity:1;
                        transform:translateX(0);
                        transition:opacity 220ms ease-out, transform 220ms ease-out;
                   }
                   .hide {
                        display:none !important;
                   }
                   .ena3d-main-layout.is-collapsed .mysidebar .right-side {
                        opacity:0;
                        transform:translateX(-0.5rem);
                        pointer-events:none;
                   }
                   .toggle-sidebar-btn{
                        position:absolute;
                        transform:translate(5px,-100px);
                        width:auto;
                        min-width:5.75rem;
                        max-width:100%;
                        display:inline-flex;
                        align-items:center;
                        justify-content:center;
                        gap:0.35rem;
                        white-space:nowrap;
                        --bs-btn-padding-x:0.1rem;
                   }
                   .toggle-sidebar-btn::before{
                        content:'\\2039';
                        display:inline-block;
                        font-size:1.35em;
                        line-height:0.75;
                        transform:rotate(0deg);
                        transition:transform 220ms ease-out;
                   }
                   .ena3d-main-layout.is-collapsed .toggle-sidebar-btn::before{
                        transform:rotate(180deg);
                   }
                   @media (min-width: 992px) {
                     .ena3d-main-layout .ena3d-sidebar-column,
                     .ena3d-main-layout .plot-container{
                       min-width:0;
                       transition:width 220ms ease-out;
                     }
                     .ena3d-main-layout.is-transitioning .ena3d-sidebar-column,
                     .ena3d-main-layout.is-transitioning .plot-container{
                       will-change:width;
                     }
                     .ena3d-main-layout.is-collapsed .ena3d-sidebar-column{
                       width:8.33333333%;
                     }
                     .ena3d-main-layout.is-collapsed .plot-container{
                       width:91.66666667%;
                     }
                     .ena3d-main-layout.is-transitioning .mysidebar{
                       position:relative;
                       overflow-x:hidden;
                     }
                     .ena3d-main-layout.is-transitioning .mysidebar .left-side{
                       transition:width 220ms ease-out;
                       will-change:width;
                     }
                     .ena3d-main-layout.is-transitioning .mysidebar .right-side{
                       position:absolute;
                       top:0;
                       right:auto;
                       bottom:0;
                       width:75%;
                       max-width:none;
                       will-change:opacity, transform;
                     }
                   }
                   @media (max-width: 991.98px), (prefers-reduced-motion: reduce) {
                     .ena3d-main-layout .ena3d-sidebar-column,
                     .ena3d-main-layout .plot-container,
                     .ena3d-main-layout .mysidebar .left-side,
                     .ena3d-main-layout .mysidebar .right-side,
                     .ena3d-main-layout .toggle-sidebar-btn::before{
                       transition:none !important;
                     }
                   }
                   .camera-position-panel .form-group{
                        display:flex;
                        flex-direction:row;
                        justify-content:center;
                        margin-bottom:0px;
                        align-items: center;
                   }
                  .camera-position-panel{
                        display:flex;
                        justify-content:center;
                  }
                  .plot-tool-bar{
                        display:flex;
                        flex-wrap:wrap;
                        align-items:flex-start;
                        gap:0.5rem;
                        padding: 10px 10px 5px 10px;
                  }
                  .plot-tool-bar .camera-position-panel{
                        flex:1 1 27rem;
                        width:auto;
                  }
                  .plot-tool-bar .col-sm-2{
                        flex:1 1 10rem;
                        width:auto;
                        min-width:0;
                  }
                  .plot-tool-bar #main_app-fullscreen_btn{
                        width:100%;
                        max-width:100%;
                        overflow-wrap:anywhere;
                        white-space:normal;
                  }
                  .fullscreen-status{
                        display:block;
                        min-height:1.2em;
                        margin-top:0.25rem;
                        font-size:var(--ena-type-meta);
                  }
                  .active-dataset-card{
                        margin-top:0.75rem;
                  }
                  .active-dataset-details{
                        margin-bottom:0;
                  }
                  .dataset-hash{
                        display:block;
                        overflow-wrap:anywhere;
                        font-size:var(--ena-type-meta);
                  }
                  .trajectory-plot-layout{
                        display:grid;
                        grid-template-columns:minmax(0, 1fr) auto;
                        align-items:start;
                        gap:0.75rem;
                        min-width:0;
                  }
                  .trajectory-plot-canvas{
                        min-width:0;
                  }
                  .trajectory-node-legend-slot:empty{
                        display:none;
                  }
                  .trajectory-node-legend-slot:not(:empty){
                        width:13rem;
                        max-height:90vh;
                        overflow-y:auto;
                        overscroll-behavior:contain;
                        background:#fff;
                        color:#25282d;
                        border:1px solid #d9dde3;
                        border-radius:0.35rem;
                        box-shadow:0 1px 3px rgba(0,0,0,0.12);
                  }
                  .trajectory-node-legend-slot:focus-visible{
                        outline:3px solid #3b82f6;
                        outline-offset:2px;
                  }
                  .trajectory-node-legend{
                        padding:0.75rem;
                  }
                  .trajectory-node-legend h3{
                        margin:0;
                        color:#25282d;
                        font-size:1rem;
                        font-weight:700;
                  }
                  .trajectory-node-legend-subtitle{
                        margin:0.15rem 0 0.6rem;
                        color:#626975;
                        font-size:var(--ena-type-meta);
                        line-height:1.25;
                  }
                  .trajectory-node-legend-list{
                        display:grid;
                        gap:0.35rem;
                        margin:0;
                        padding:0;
                        list-style:none;
                  }
                  .trajectory-node-legend-item{
                        display:flex;
                        align-items:center;
                        min-width:0;
                        gap:0.45rem;
                        font-size:var(--ena-type-small);
                        line-height:1.25;
                  }
                  .trajectory-node-legend-swatch{
                        flex:0 0 auto;
                        width:0.8rem;
                        height:0.8rem;
                        border:1px solid rgba(37,40,45,0.65);
                        border-radius:50%;
                  }
                  .trajectory-node-legend-label{
                        min-width:0;
                        overflow-wrap:anywhere;
                  }
                  @media (max-width: 991.98px) {
                    .mysidebar .left-side .nav{
                      display:grid !important;
                      height:auto;
                      max-height:none;
                      grid-template-columns:repeat(4, minmax(0, 1fr));
                      align-items:stretch;
                      justify-content:stretch;
                    }
                    .mysidebar .left-side .nav > li{
                      width:auto;
                      margin:0;
                    }
                    .trajectory-plot-layout{
                      grid-template-columns:minmax(0, 1fr);
                      gap:0.5rem;
                    }
                    .trajectory-node-legend-slot:not(:empty){
                      width:100%;
                      max-height:12rem;
                    }
                    .trajectory-node-legend-list{
                      grid-template-columns:repeat(3, minmax(0, 1fr));
                    }
                  }
                  @media (max-width: 575.98px) {
                    .ena3d-main-layout{
                      margin-left:0;
                      margin-right:0;
                    }
                    .ena3d-sidebar-column,
                    .mysidebar,
                    .mysidebar .left-side,
                    .mysidebar .right-side{
                      height:auto !important;
                      max-height:none !important;
                    }
                    .mysidebar .right-side{
                      height:auto;
                      max-height:none;
                      overflow:visible;
                    }
                    .toggle-sidebar-btn{
                      position:static;
                      transform:none !important;
                      width:100%;
                      margin:0.5rem 0;
                    }
                    .plot-container{
                      width:100%;
                      max-width:100%;
                      padding-left:0.5rem;
                      padding-right:0.5rem;
                    }
                    .plot-container .plotly.html-widget{
                      max-width:100%;
                      height:70vh !important;
                      min-height:420px;
                    }
                    .plot-tool-bar,
                    .camera-position-panel{
                      flex-wrap:wrap;
                    }
                    .plot-tool-bar{
                      margin-left:0;
                      margin-right:0;
                    }
                    .trajectory-plot-layout{
                      grid-template-columns:minmax(0, 1fr);
                      gap:0.5rem;
                    }
                    .trajectory-node-legend-slot:not(:empty){
                      width:100%;
                      max-height:9rem;
                    }
                    .trajectory-node-legend-list{
                      grid-template-columns:repeat(2, minmax(0, 1fr));
                    }
                  }
                 "
    ),
    ena3d_connection_guard_ui(),
    navbarPage(
      title = ena3d_brand_ui(),
      id = "site_nav",
      selected = "tool",
      windowTitle = "3D ENA Workspace | Epistemic Network Analysis",
      collapsible = TRUE,
      fluid = TRUE,
      theme = ena3d_site_theme(),
      bslib::nav_item(
        tags$a(
          class = "nav-link",
          href = "/",
          `data-value` = "home",
          "Home"
        )
      ),
      tabPanel(
        title = "3D ENA",
        value = "tool",
        tags$div(
          class = "ena3d-tool-page",
          fluidRow(
      
      column(5,
        fluidRow(
          h2('3D ENA',id='ena_3d_h2')
        ),
        
        navlistPanel(
          id = "workspace_sections",
          widths = c(3, 9),
          tabPanel("Data",
                   data_upload_ui(
                     id = "main_app",
                     sample_data_files = config$sample_files
                   )
          ),
          tabPanel("Model",
                   model_ui(id = "main_app"),
          ),
          tabPanel("Plot Tools",
                   plot_settings_ui(id = "main_app")
          ),
          tabPanel("Stats",
                   stats_ui(id = "main_app")
          ),

        )%>%
          tagAppendAttributes(class = 'mysidebar', id = 'ena3d-sidebar'),
        fluidRow(
          actionButton(
            'toggle_sidebar_btn',
            'Hide',
            class = 'toggle-sidebar-btn',
            `aria-expanded` = 'true',
            `aria-controls` = 'ena3d-sidebar-details',
            `aria-label` = 'Hide ENA controls'
          ),
          
        )
        
        ) %>% tagAppendAttributes(class = 'ena3d-sidebar-column'),
      column(7,
        
        fluidRow(
          column(8,camera_position_panel_ui(id = "main_app"))%>%
            tagAppendAttributes(class= 'camera-position-panel'),
          column(
            2,
            actionButton(
              NS("main_app",'fullscreen_btn'),
              'Full Screen',
              `aria-label` = 'Enter fullscreen for the visible ENA plot',
              `aria-controls` = 'ena3d-plot-container'
            ),
            tags$small(
              id = 'ena3d-fullscreen-status',
              class = 'fullscreen-status text-muted',
              role = 'status',
              `aria-live` = 'polite'
            )
          ),
          column(
            2,
            ai_interpretation_ui(
              NS("main_app", "ai_interpretation"),
              context_max_chars = config$ai$context_max_chars,
              stylesheet_version = paste(
                config$app_version, config$build_id, sep = "-"
              )
            )
          )
          
        )%>%tagAppendAttributes(class= 'plot-tool-bar'),
        
        plot_ui(id = "main_app"),
        
      )%>%tagAppendAttributes(
        class = 'plot-container',
        id = 'ena3d-plot-container'
      )
          ) %>% tagAppendAttributes(class = 'ena3d-main-layout')
        )
      ),
      bslib::nav_item(
        tags$a(
          class = "nav-link",
          href = "/papers",
          `data-value` = "papers",
          "PAPERS"
        )
      ),
      bslib::nav_item(
        tags$a(
          class = "nav-link",
          href = "/team",
          `data-value` = "team",
          "TEAM"
        )
      ),
      bslib::nav_item(
        tags$a(
          class = "nav-link",
          href = "/about",
          `data-value` = "about",
          "ABOUT"
        )
      ),
      footer = ena3d_footer_ui()
    ),
    
    tags$script(
      type = "text/javascript",
      shiny::HTML("(function () {
        const sidebar = document.querySelector('.mysidebar');
        const toggleButton = document.querySelector('.toggle-sidebar-btn');
        const plotContainer = document.querySelector('.plot-container');
        const heading = document.getElementById('ena_3d_h2');
        const mainLayout = document.querySelector('.ena3d-main-layout');

        if (
          sidebar && toggleButton && plotContainer && mainLayout &&
          sidebar.children.length >= 2
        ) {
          const leftSide = sidebar.children[0];
          const rightSide = sidebar.children[1];
          const sidebarColumn = sidebar.closest('.ena3d-sidebar-column');
          const motionPreference = window.matchMedia(
            '(prefers-reduced-motion: reduce)'
          );
          const desktopLayout = window.matchMedia('(min-width: 992px)');
          const SIDEBAR_MOTION_MS = 220;
          const SIDEBAR_MOTION_FALLBACK_MS = SIDEBAR_MOTION_MS + 80;
          let transitionGeneration = 0;
          let transitionFrame = 0;
          let transitionTimer = 0;
          let resizeFrame = 0;
          let activeTransition = null;

          leftSide.classList.add('left-side');
          rightSide.classList.add('right-side', 'well');
          rightSide.id = 'ena3d-sidebar-details';
          if (sidebarColumn) sidebarColumn.classList.add('big-sidebar');

          const positionToggle = function () {
            if (window.matchMedia('(max-width: 575.98px)').matches) {
              toggleButton.style.transform = 'none';
              return;
            }
            const translateX = leftSide.getBoundingClientRect().width / 2 - 20;
            toggleButton.style.transform = `translate(${translateX}px,-100px)`;
          };

          const clearSidebarTransitionCallbacks = function () {
            if (transitionFrame) {
              window.cancelAnimationFrame(transitionFrame);
              transitionFrame = 0;
            }
            if (transitionTimer) {
              window.clearTimeout(transitionTimer);
              transitionTimer = 0;
            }
          };

          const dispatchFinalPlotResize = function () {
            if (resizeFrame) window.cancelAnimationFrame(resizeFrame);
            resizeFrame = window.requestAnimationFrame(function () {
              resizeFrame = 0;
              window.dispatchEvent(new Event('resize'));
            });
          };

          const setSidebarControlState = function (collapsed) {
            toggleButton.textContent = collapsed ? 'Show' : 'Hide';
            toggleButton.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
            toggleButton.setAttribute(
              'aria-label', collapsed ? 'Show ENA controls' : 'Hide ENA controls'
            );
            if (heading) heading.textContent = collapsed ? 'ENA' : '3D ENA';
          };

          const setSidebarDetailsInteractive = function (interactive) {
            rightSide.toggleAttribute('inert', !interactive);
            if (interactive) {
              rightSide.removeAttribute('aria-hidden');
            } else {
              rightSide.setAttribute('aria-hidden', 'true');
            }
          };

          const finalizeSidebarTransition = function (collapsed, generation) {
            if (
              !activeTransition ||
              generation !== activeTransition.generation ||
              collapsed !== activeTransition.collapsed
            ) return;

            clearSidebarTransitionCallbacks();
            activeTransition = null;
            mainLayout.classList.toggle('is-collapsed', collapsed);
            mainLayout.classList.remove('is-transitioning');
            rightSide.classList.toggle('hide', collapsed);
            setSidebarDetailsInteractive(!collapsed);
            leftSide.classList.toggle('col-sm-12', collapsed);
            leftSide.classList.toggle('col-sm-3', !collapsed);
            leftSide.style.removeProperty('width');
            leftSide.style.removeProperty('transition');
            rightSide.style.removeProperty('left');
            rightSide.style.removeProperty('width');
            positionToggle();
            dispatchFinalPlotResize();
          };

          const beginSidebarTransition = function (collapsed) {
            transitionGeneration += 1;
            const generation = transitionGeneration;
            clearSidebarTransitionCallbacks();
            if (resizeFrame) {
              window.cancelAnimationFrame(resizeFrame);
              resizeFrame = 0;
            }

            activeTransition = {generation: generation, collapsed: collapsed};
            setSidebarControlState(collapsed);

            if (collapsed && rightSide.contains(document.activeElement)) {
              toggleButton.focus({preventScroll: true});
            }
            setSidebarDetailsInteractive(!collapsed);
            if (!collapsed) rightSide.classList.remove('hide');

            const currentRailWidth = leftSide.getBoundingClientRect().width;
            const inlineDetailsWidth = Number.parseFloat(rightSide.style.width);
            const currentDetailsWidth = Number.isFinite(inlineDetailsWidth)
              ? inlineDetailsWidth
              : rightSide.getBoundingClientRect().width;
            const currentSidebarWidth = sidebarColumn
              ? sidebarColumn.getBoundingClientRect().width
              : 0;
            const currentPlotWidth = plotContainer.getBoundingClientRect().width;
            const availableWidth = currentSidebarWidth + currentPlotWidth;
            const targetSidebarWidth = availableWidth * (collapsed ? 1 : 5) / 12;
            const targetRailWidth = collapsed
              ? targetSidebarWidth
              : targetSidebarWidth / 4;
            const targetDetailsWidth = targetSidebarWidth * 3 / 4;
            const stagedDetailsWidth = collapsed && currentDetailsWidth > 0
              ? currentDetailsWidth
              : targetDetailsWidth;
            const stagedDetailsLeft = collapsed
              ? currentRailWidth
              : targetRailWidth;
            const hasWidthMotion = Math.abs(
              currentSidebarWidth - targetSidebarWidth
            ) > 0.5;

            leftSide.style.setProperty('transition', 'none', 'important');
            leftSide.style.width = `${currentRailWidth}px`;
            leftSide.classList.toggle('col-sm-12', collapsed);
            leftSide.classList.toggle('col-sm-3', !collapsed);
            rightSide.style.left = `${stagedDetailsLeft}px`;
            rightSide.style.width = `${stagedDetailsWidth}px`;
            mainLayout.classList.add('is-transitioning');
            void leftSide.offsetWidth;
            leftSide.style.removeProperty('transition');

            const shouldAnimate = Boolean(
              sidebarColumn && desktopLayout.matches &&
              !motionPreference.matches && hasWidthMotion
            );

            if (!shouldAnimate) {
              mainLayout.classList.toggle('is-collapsed', collapsed);
              finalizeSidebarTransition(collapsed, generation);
              return;
            }

            void mainLayout.offsetWidth;
            transitionFrame = window.requestAnimationFrame(function () {
              transitionFrame = 0;
              if (
                !activeTransition ||
                generation !== activeTransition.generation
              ) return;

              mainLayout.classList.toggle('is-collapsed', collapsed);
              leftSide.style.width = `${targetRailWidth}px`;
              transitionTimer = window.setTimeout(function () {
                finalizeSidebarTransition(collapsed, generation);
              }, SIDEBAR_MOTION_FALLBACK_MS);
            });
          };

          if (sidebarColumn) {
            sidebarColumn.addEventListener('transitionend', function (event) {
              if (
                event.target !== sidebarColumn ||
                event.propertyName !== 'width' ||
                !activeTransition
              ) return;
              finalizeSidebarTransition(
                activeTransition.collapsed,
                activeTransition.generation
              );
            });
          }

          toggleButton.addEventListener('click', function () {
            const collapsed = toggleButton.getAttribute('aria-expanded') === 'true';
            beginSidebarTransition(collapsed);
          });

          positionToggle();
          window.addEventListener('resize', positionToggle);
          window.addEventListener('resize', function () {
            if (
              activeTransition &&
              (!desktopLayout.matches || motionPreference.matches)
            ) {
              finalizeSidebarTransition(
                activeTransition.collapsed,
                activeTransition.generation
              );
            }
          });
          if (typeof ResizeObserver === 'function') {
            const railObserver = new ResizeObserver(positionToggle);
            railObserver.observe(leftSide);
          }
          if (typeof motionPreference.addEventListener === 'function') {
            motionPreference.addEventListener('change', function () {
              if (activeTransition && motionPreference.matches) {
                finalizeSidebarTransition(
                  activeTransition.collapsed,
                  activeTransition.generation
                );
              }
            });
          }
        }

        const fullscreenButton = document.getElementById('main_app-fullscreen_btn');
        const fullscreenStatus = document.getElementById('ena3d-fullscreen-status');
        let fallbackFullscreenPlot = null;
        let fullscreenExitButton = null;
        const setFullscreenStatus = function (message, isError) {
          if (!fullscreenStatus) return;
          fullscreenStatus.textContent = message;
          fullscreenStatus.classList.toggle('text-danger', Boolean(isError));
          fullscreenStatus.classList.toggle('text-muted', !isError);
        };
        const resizeVisiblePlot = function () {
          window.setTimeout(function () {
            window.dispatchEvent(new Event('resize'));
          }, 50);
        };
        const removeFullscreenExitButton = function () {
          if (fullscreenExitButton) fullscreenExitButton.remove();
          fullscreenExitButton = null;
        };
        const exitFallbackFullscreen = function () {
          if (!fallbackFullscreenPlot) return;
          fallbackFullscreenPlot.classList.remove('ena3d-fullscreen-fallback');
          fallbackFullscreenPlot = null;
          removeFullscreenExitButton();
          setFullscreenStatus('Fullscreen closed.', false);
          resizeVisiblePlot();
        };
        const exitFullscreen = function () {
          if (fallbackFullscreenPlot) {
            exitFallbackFullscreen();
            return;
          }
          const exit = document.exitFullscreen || document.webkitExitFullscreen;
          if (!exit) return;
          const result = exit.call(document);
          if (result && typeof result.catch === 'function') {
            result.catch(function () {
              setFullscreenStatus('Could not close fullscreen mode.', true);
            });
          }
        };
        const installFullscreenExitButton = function (visiblePlot) {
          removeFullscreenExitButton();
          fullscreenExitButton = document.createElement('button');
          fullscreenExitButton.type = 'button';
          fullscreenExitButton.className = 'btn btn-default ena3d-fullscreen-exit';
          fullscreenExitButton.textContent = 'Exit full screen';
          fullscreenExitButton.setAttribute('aria-label', 'Exit fullscreen plot');
          fullscreenExitButton.addEventListener('click', exitFullscreen);
          visiblePlot.appendChild(fullscreenExitButton);
        };
        const enterFallbackFullscreen = function (visiblePlot) {
          fallbackFullscreenPlot = visiblePlot;
          visiblePlot.classList.add('ena3d-fullscreen-fallback');
          installFullscreenExitButton(visiblePlot);
          setFullscreenStatus('Fullscreen fallback active.', false);
          resizeVisiblePlot();
        };
        if (fullscreenButton) {
          fullscreenButton.addEventListener('click', function () {
            if (fallbackFullscreenPlot || document.fullscreenElement ||
                document.webkitFullscreenElement) {
              exitFullscreen();
              return;
            }
            const plots = Array.from(
              document.querySelectorAll('.plot-container .plotly.html-widget')
            );
            const visiblePlot = plots.find(function (element) {
              const style = window.getComputedStyle(element);
              const bounds = element.getBoundingClientRect();
              return style.display !== 'none' &&
                style.visibility !== 'hidden' &&
                bounds.width > 0 && bounds.height > 0;
            });
            if (!visiblePlot) {
              setFullscreenStatus('No visible plot is available for fullscreen.', true);
              return;
            }

            const requestFullscreen = visiblePlot.requestFullscreen ||
              visiblePlot.webkitRequestFullscreen;
            if (!requestFullscreen) {
              enterFallbackFullscreen(visiblePlot);
              return;
            }
            if (visiblePlot.id) {
              fullscreenButton.setAttribute('aria-controls', visiblePlot.id);
            }
            setFullscreenStatus('Requesting fullscreen ...', false);
            installFullscreenExitButton(visiblePlot);
            try {
              const result = requestFullscreen.call(visiblePlot);
              if (result && typeof result.catch === 'function') {
                result.catch(function (error) {
                  console.warn(
                    'Native fullscreen was unavailable; using the page fallback.',
                    error
                  );
                  enterFallbackFullscreen(visiblePlot);
                });
              }
            } catch (error) {
              console.warn(
                'Native fullscreen was unavailable; using the page fallback.',
                error
              );
              enterFallbackFullscreen(visiblePlot);
            }
          });
          document.addEventListener('fullscreenchange', function () {
            if (document.fullscreenElement) {
              setFullscreenStatus('Fullscreen active.', false);
            } else if (!fallbackFullscreenPlot) {
              removeFullscreenExitButton();
              setFullscreenStatus('Fullscreen closed.', false);
            }
            // Plotly widgets retain the width measured in the split workspace
            // until their htmlwidget resize handler runs. Re-measure after the
            // fullscreen element has acquired its viewport-sized box so the
            // WebGL canvas fills the screen instead of leaving a black gutter.
            resizeVisiblePlot();
          });
          document.addEventListener('webkitfullscreenchange', function () {
            if (!document.webkitFullscreenElement && !fallbackFullscreenPlot) {
              removeFullscreenExitButton();
              setFullscreenStatus('Fullscreen closed.', false);
            }
            resizeVisiblePlot();
          });
          document.addEventListener('keydown', function (event) {
            if (event.key === 'Escape' && fallbackFullscreenPlot) {
              event.preventDefault();
              exitFallbackFullscreen();
            }
          });
        }

        document.addEventListener('shown.bs.tab', function () {
          window.setTimeout(function () {
            window.dispatchEvent(new Event('resize'));
          }, 50);
        });

        const writeCitationToClipboard = function (text) {
          const writeWithSelection = function () {
            return new Promise(function (resolve, reject) {
              const textarea = document.createElement('textarea');
              textarea.value = text;
              textarea.setAttribute('readonly', '');
              textarea.style.position = 'fixed';
              textarea.style.opacity = '0';
              document.body.appendChild(textarea);
              textarea.select();
              const copied = document.execCommand('copy');
              textarea.remove();
              if (copied) resolve();
              else reject(new Error('Clipboard copy was not available.'));
            });
          };
          if (navigator.clipboard && window.isSecureContext) {
            return navigator.clipboard.writeText(text).catch(writeWithSelection);
          }
          return writeWithSelection();
        };

        document.addEventListener('click', function (event) {
          const button = event.target.closest('.ena3d-copy-citation');
          if (!button) return;
          const citation = document.getElementById(
            button.getAttribute('data-citation-target')
          );
          if (!citation) return;
          const citationText = citation.getAttribute('data-citation-text') ||
            citation.textContent.trim();
          const defaultAriaLabel = button.getAttribute('aria-label');
          writeCitationToClipboard(citationText).then(function () {
            window.clearTimeout(button.ena3dCopyResetTimer);
            button.textContent = 'Copied';
            button.setAttribute('aria-label', 'APA citation copied');
            button.classList.add('is-copied');
            button.ena3dCopyResetTimer = window.setTimeout(function () {
              button.textContent = button.getAttribute('data-default-label') || 'Copy APA';
              button.setAttribute('aria-label', defaultAriaLabel);
              button.classList.remove('is-copied');
            }, 2200);
          }).catch(function () {
            button.textContent = 'Select citation to copy';
            button.setAttribute(
              'aria-label',
              'Clipboard copy unavailable; select the citation to copy'
            );
            citation.focus();
          });
        });
      })();")
    )
    
  )
  
}

"
 Server wrapper, used to passing variables (state) between UI and the
"
app_server <- function(input, output, session) {
  ena3d_load_analysis_runtime()
  ena3d_disable_new_session_reconnect(session)
  ena3d_register_connection_proof(input, session)

  # Use ena_server_state to communicate between the UI and ena_app_server module
  ena_server_state <- ENA_3D_Server$new()
  ena_server_state$active_tab <- reactive({
    input$'main_app-mytabs'
  })
  
  # The server needs to know which tab is currently active in order to show the corresponding data
  ena_server_state$render_comparison <- reactive({
    ena_server_state$active_tab() == 'comparison_plot'
  })
  ena_server_state$render_overall <- reactive({
    ena_server_state$active_tab() == 'overall_model'
  })
  ena_server_state$render_unit_group_change_plot <-reactive({
    ena_server_state$active_tab() == 'group_change'
  })
  ena_server_state$render_network_plot <-reactive({
    ena_server_state$active_tab() == 'network'
  })
  ena_server_state$render_trajectory_plot <- reactive({
    ena_server_state$active_tab() == 'trajectory'
  })

  observeEvent(input$ena3d_workspace_entry, {
    req(identical(input$ena3d_workspace_entry, "trajectory"))
    updateNavlistPanel(
      session,
      "workspace_sections",
      selected = "Model"
    )
    updateTabsetPanel(
      session,
      "main_app-mytabs",
      selected = "trajectory"
    )
  })
  
  ena_app_server(
    id = "main_app",
    state = ena_server_state,
    config = config,
    page_active = reactive(TRUE),
    workspace_section = reactive(input$workspace_sections)
  )
  # ena_comparison_plot_server( "main_app")
}
options(
  shiny.maxRequestSize = ena3d_env_number(
    "ENA3D_MAX_REQUEST_BYTES",
    5 * 1024^2,
    maximum = 25 * 1024^2
  )
)

inline_assets <- tolower(Sys.getenv("ENA3D_INLINE_ASSETS", unset = "false")) %in%
  c("1", "true", "yes", "on")
.ena3d_static_plotly_registered <- FALSE
.ena3d_www_dir <- file.path(.ena3d_app_dir, "www")

prebuilt_static_site_path <- if (inline_assets) {
  Sys.getenv("ENA3D_PREBUILT_STATIC_SITE_PATH", unset = "")
} else {
  ""
}
if (nzchar(prebuilt_static_site_path) &&
    !file.exists(prebuilt_static_site_path)) {
  stop(
    "ENA3D_PREBUILT_STATIC_SITE_PATH does not exist: ",
    prebuilt_static_site_path,
    call. = FALSE
  )
}
if (nzchar(prebuilt_static_site_path)) {
  static_site_html <- readChar(
    prebuilt_static_site_path,
    nchars = file.info(prebuilt_static_site_path)$size,
    useBytes = TRUE
  )
  static_site_html <- gsub(
    "__ENA3D_BUILD_ID__",
    config$build_id,
    static_site_html,
    fixed = TRUE
  )
} else {
  static_site_html <- ena3d_render_static_site(
    config,
    .ena3d_www_dir,
    # Keep the static document independent from Shiny's session-scoped
    # dependency registry in every runtime profile, including local runApp.
    inline_assets = TRUE
  )
}
static_site_http_handler <- ena3d_static_site_handler(static_site_html)

ena3d_app <- if (inline_assets) {
  prebuilt_ui_path <- Sys.getenv("ENA3D_PREBUILT_UI_PATH", unset = "")
  if (nzchar(prebuilt_ui_path) && !file.exists(prebuilt_ui_path)) {
    stop(
      "ENA3D_PREBUILT_UI_PATH does not exist: ",
      prebuilt_ui_path,
      call. = FALSE
    )
  }
  if (nzchar(prebuilt_ui_path)) {
    app_ui_html <- readChar(
      prebuilt_ui_path,
      nchars = file.info(prebuilt_ui_path)$size,
      useBytes = TRUE
    )
    app_ui_html <- gsub(
      "__ENA3D_BUILD_ID__",
      config$build_id,
      app_ui_html,
      fixed = TRUE
    )
    .ena3d_static_plotly_registered <-
      ena3d_register_prebuilt_plotly_resources(app_ui_html)
  } else {
    ena3d_load_analysis_runtime()
    app_ui_html <- ena3d_render_inline_ui(
      app_ui(),
      .ena3d_www_dir
    )
  }

  app_ui_handler <- local({
    content <- app_ui_html
    function(request) {
      shiny:::httpResponse(
        status = 200L,
        content = content,
        headers = list("Cache-Control" = "no-store")
      )
    }
  })
  attr(app_ui_handler, "http_methods_supported") <- "GET"
  shinyApp(app_ui_handler, app_server)
} else {
  ena3d_load_analysis_runtime()
  shinyApp(app_ui(), app_server)
}

ena3d_app <- ena3d_enable_site_routes(
  ena3d_app,
  static_handler = static_site_http_handler
)
ena3d_security_log(
  "app_ready",
  fields = list(
    startup_seconds = sprintf(
      "%.3f",
      unname(proc.time()[["elapsed"]]) - .ena3d_source_started_at
    ),
    analysis_runtime_loaded = .ena3d_analysis_runtime_loaded,
    static_plotly_registered = .ena3d_static_plotly_registered
  )
)
ena3d_app
