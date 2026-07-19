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

.ena3d_source('install_dependencies.R')

library(plotly)
library(data.table)
library(shiny)
library(R6)
library(rENA)
.ena3d_source('security_utils.R')
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
.ena3d_source('app_server.R')


config = list()
config$sample_data_path = normalizePath(
  file.path(.ena3d_project_root, "sample_data"),
  mustWork = TRUE
)
config$data_limits = ena3d_data_limits()
config$build_id = Sys.getenv("ENA3D_BUILD_ID", unset = "development")
config$app_version = Sys.getenv(
  "ENA3D_APP_VERSION",
  unset = trimws(readLines(
    file.path(.ena3d_project_root, "VERSION"),
    n = 1L,
    warn = FALSE
  ))
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
      ai_enabled = isTRUE(config$ai$available)
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
    r_version = paste(R.version$major, R.version$minor, sep = ".")
  )
)
"
R6 class.
It is an object used to communicate data between modules.
"
ENA_3D_Server <- R6Class("ENA_3D_Server",
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
                   }
                   .hide {
                        display:none !important;
                   }
                   .toggle-sidebar-btn{
                        position:absolute;
                        transform:translate(5px,-100px);
                        width:auto;
                        min-width:45px;
                        max-width:100%;
                        white-space:normal;
                        --bs-btn-padding-x:0.1rem;
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
    navbarPage(
      title = ena3d_brand_ui(),
      id = "site_nav",
      selected = "home",
      windowTitle = "3D ENA | Epistemic Network Analysis",
      collapsible = TRUE,
      fluid = TRUE,
      theme = bslib::bs_theme(
        version = 5,
        bg = "#f4f1e9",
        fg = "#102a43",
        primary = "#087f85",
        secondary = "#d36f52"
      ),
      tabPanel(
        title = "Home",
        value = "home",
        ena3d_home_ui()
      ),
      tabPanel(
        title = "3D ENA",
        value = "tool",
        tags$div(
          class = "ena3d-tool-page",
          fluidRow(
      
      column(5,
        fluidRow(
          h2('3D ENA',id='ena_3d_h2'),
          tags$small(
            class = "text-muted ena3d-build-id",
            paste0(
              "Version ", config$app_version,
              " · Build ", config$build_id
            )
          ),
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
      tabPanel(
        title = "ABOUT",
        value = "about",
        ena3d_about_ui()
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

        if (sidebar && toggleButton && plotContainer && sidebar.children.length >= 2) {
          const leftSide = sidebar.children[0];
          const rightSide = sidebar.children[1];
          const sidebarColumn = sidebar.closest('.col-sm-5, .col-sm-1');

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

          toggleButton.addEventListener('click', function () {
            const collapsed = !rightSide.classList.contains('hide');
            rightSide.classList.toggle('hide', collapsed);
            leftSide.classList.toggle('col-sm-12', collapsed);
            leftSide.classList.toggle('col-sm-3', !collapsed);

            if (sidebarColumn) {
              sidebarColumn.classList.toggle('col-sm-1', collapsed);
              sidebarColumn.classList.toggle('col-sm-5', !collapsed);
            }
            plotContainer.classList.toggle('col-sm-11', collapsed);
            plotContainer.classList.toggle('col-sm-7', !collapsed);

            toggleButton.textContent = collapsed ? 'Show' : 'Hide';
            toggleButton.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
            toggleButton.setAttribute(
              'aria-label', collapsed ? 'Show ENA controls' : 'Hide ENA controls'
            );
            if (heading) heading.textContent = collapsed ? 'ENA' : '3D ENA';
            positionToggle();
            window.dispatchEvent(new Event('resize'));
          });

          positionToggle();
          window.addEventListener('resize', positionToggle);
        }

        const fullscreenButton = document.getElementById('main_app-fullscreen_btn');
        const fullscreenStatus = document.getElementById('ena3d-fullscreen-status');
        const setFullscreenStatus = function (message, isError) {
          if (!fullscreenStatus) return;
          fullscreenStatus.textContent = message;
          fullscreenStatus.classList.toggle('text-danger', Boolean(isError));
          fullscreenStatus.classList.toggle('text-muted', !isError);
        };
        if (fullscreenButton) {
          fullscreenButton.addEventListener('click', function () {
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
              setFullscreenStatus('Fullscreen is not supported by this browser.', true);
              return;
            }
            if (visiblePlot.id) {
              fullscreenButton.setAttribute('aria-controls', visiblePlot.id);
            }
            setFullscreenStatus('Requesting fullscreen ...', false);
            const result = requestFullscreen.call(visiblePlot);
            if (result && typeof result.catch === 'function') {
              result.catch(function (error) {
                console.warn('Could not enter fullscreen mode.', error);
                setFullscreenStatus('Could not enter fullscreen mode.', true);
              });
            }
          });
          document.addEventListener('fullscreenchange', function () {
            setFullscreenStatus(
              document.fullscreenElement ? 'Fullscreen active.' : 'Fullscreen closed.',
              false
            );
          });
        }

        document.addEventListener('shown.bs.tab', function () {
          window.setTimeout(function () {
            window.dispatchEvent(new Event('resize'));
          }, 50);
        });
      })();")
    )
    
  )
  
}

"
 Server wrapper, used to passing variables (state) between UI and the
"
app_server <- function(input, output, session) {
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

  open_site_page <- function(page) {
    updateNavbarPage(session, "site_nav", selected = page)
  }
  observeEvent(input$home_brand, open_site_page("home"))
  observeEvent(input$launch_ena, open_site_page("tool"))
  observeEvent(input$launch_ena_note, open_site_page("tool"))
  observeEvent(input$launch_ena_about, open_site_page("tool"))
  observeEvent(input$explore_trajectory, {
    open_site_page("tool")
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
  observeEvent(input$meet_developer, open_site_page("about"))
  
  ena_app_server(
    id = "main_app",
    state = ena_server_state,
    config = config,
    page_active = reactive(identical(input$site_nav, "tool")),
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

# Build and pre-render the document before the HTTP server accepts traffic.
# Container platforms can route dependency requests to a newly started
# instance before that instance has served its first page request. Shiny
# registers generated htmlDependency resource paths while rendering, so the
# eager render ensures every instance can serve those requests immediately.
app_ui_document <- app_ui()
invisible(shiny:::renderPage(app_ui_document))
shinyApp(app_ui_document, app_server)
