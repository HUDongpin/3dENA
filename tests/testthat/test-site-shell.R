library(testthat)

.site_shell_roots <- c(".", "..", "../..")
.site_shell_root <- .site_shell_roots[file.exists(
  file.path(.site_shell_roots, "R", "app_ui_site.R")
)][1L]
if (is.na(.site_shell_root)) stop("Could not locate the 3D ENA project root.")
.site_shell_root <- normalizePath(.site_shell_root)

if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("htmltools", quietly = TRUE)) {
  skip("Site-shell tests require shiny and htmltools.")
}

.site_shell_env <- new.env(parent = globalenv())
.site_shell_env$tags <- shiny::tags
.site_shell_env$tagList <- htmltools::tagList
.site_shell_env$actionLink <- shiny::actionLink
.site_shell_env$actionButton <- shiny::actionButton
sys.source(
  file.path(.site_shell_root, "R", "app_ui_site.R"),
  envir = .site_shell_env
)


test_that("Brand is an accessible action that returns to Home", {
  brand <- htmltools::renderTags(.site_shell_env$ena3d_brand_ui())$html

  expect_match(brand, 'id="home_brand"', fixed = TRUE)
  expect_match(brand, 'class="action-button ena3d-brand"', fixed = TRUE)
  expect_match(
    brand,
    'aria-label="Return to the 3D ENA home page"',
    fixed = TRUE
  )
  expect_match(brand, 'href="#"', fixed = TRUE)
})


test_that("Home gives researchers a direct path into 3D ENA", {
  home <- htmltools::renderTags(.site_shell_env$ena3d_home_ui())$html

  expect_false(grepl("3D TRAJECTORY ANALYSIS", home, fixed = TRUE))
  expect_match(home, "Open 3D ENA", fixed = TRUE)
  expect_match(home, "id=\"launch_ena\"", fixed = TRUE)
  expect_match(home, "id=\"explore_trajectory\"", fixed = TRUE)
  expect_match(home, "Explore trajectory", fixed = TRUE)
  expect_match(home, "ena3d-assets/trajectory-home-preview-3d.png", fixed = TRUE)
  expect_false(grepl("ena3d-assets/trajectory-home-preview.svg", home, fixed = TRUE))
  expect_false(grepl("FIG. 01", home, fixed = TRUE))
  expect_false(grepl("Ordered centroid paths reveal", home, fixed = TRUE))
  expect_match(home, "jENA", fixed = TRUE)
  expect_match(home, "PARITY for rENA", fixed = TRUE)
  expect_false(grepl("Bootstrap uncertainty", home, fixed = TRUE))
  expect_match(home, "Follow change through time", fixed = TRUE)
  expect_match(home, "Group comparison", fixed = TRUE)
  expect_false(grepl("uncertainty intervals", home, fixed = TRUE))
  preview_path <- file.path(
    .site_shell_root,
    "images",
    "trajectory-home-preview-3d.png"
  )
  expect_true(file.exists(preview_path))
  expect_gt(file.info(preview_path)$size, 0)
  expect_match(home, "Load", fixed = TRUE)
  expect_match(home, "Configure", fixed = TRUE)
  expect_match(home, "Interpret", fixed = TRUE)
})


test_that("Home hero uses the compact colorful trajectory layout", {
  css <- paste(
    readLines(
      file.path(.site_shell_root, "R", "www", "app_shell.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(css, "min-height: clamp(38rem, 52vw, 42.5rem);", fixed = TRUE)
  expect_match(
    css,
    "grid-template-columns: minmax(0, 0.92fr) minmax(520px, 1.08fr);",
    fixed = TRUE
  )
  expect_match(css, "max-width: 19ch;", fixed = TRUE)
  expect_match(
    css,
    "font-size: clamp(2.9rem, 4.15vw, 4.5rem);",
    fixed = TRUE
  )
  expect_match(css, ".ena3d-trajectory-showcase", fixed = TRUE)
  expect_match(css, "max-width: 860px;", fixed = TRUE)
  expect_match(css, "height: clamp(22rem, 30vw, 27rem);", fixed = TRUE)
  expect_match(css, ".ena3d-brand:focus-visible", fixed = TRUE)
  expect_match(css, "linear-gradient(145deg", fixed = TRUE)
  expect_match(
    css,
    'body.bslib-page-navbar > nav.navbar + div.container-fluid',
    fixed = TRUE
  )
  expect_match(css, "border-top: 0;", fixed = TRUE)
  expect_false(grepl("min-height: 620px;", css, fixed = TRUE))
  expect_false(grepl(".ena3d-hero::before", css, fixed = TRUE))
  expect_false(grepl("background-size: 44px 44px;", css, fixed = TRUE))
})


test_that("About presents the verified public developer profile", {
  about <- htmltools::renderTags(.site_shell_env$ena3d_about_ui())$html

  expect_match(about, "Dr. Peter Hu Dongpin", fixed = TRUE)
  expect_match(about, "Developer of 3D ENA Version 0.2.0", fixed = TRUE)
  expect_false(grepl("Co-developer of 3D ENA", about, fixed = TRUE))
  expect_false(grepl(">DEVELOPER<", about, fixed = TRUE))
  expect_match(
    about,
    'src="ena3d-assets/peter-hu-portrait.png"',
    fixed = TRUE
  )
  expect_match(about, 'alt="Portrait of Dr. Peter Hu Dongpin"', fixed = TRUE)
  expect_match(about, 'class="ena3d-about-portrait"', fixed = TRUE)
  expect_match(about, "Educational Technology", fixed = TRUE)
  expect_false(grepl(
    "MEd in Educational Studies, The Education University of Hong Kong",
    about,
    fixed = TRUE
  ))
  expect_match(about, "Learning analytics and network analysis", fixed = TRUE)
  expect_match(about, "https://www.hudongpin.com/", fixed = TRUE)
  expect_match(about, "rel=\"noopener noreferrer\"", fixed = TRUE)
  expect_match(
    about,
    "The 3D ENA Version 0.2.0 project is inspired by the previous 3D ENA Version 0.1.0.",
    fixed = TRUE
  )
  expect_match(
    about,
    "Dr. Peter Hu is charge of revolutionizing the 3D ENA tool since 2026 July 17.",
    fixed = TRUE
  )
  expect_match(about, "Welcome research collaboration worldwide.", fixed = TRUE)
  expect_false(grepl("Biographical details are based", about, fixed = TRUE))
})


test_that("Papers provides three verified, copy-ready APA references", {
  papers <- htmltools::renderTags(.site_shell_env$ena3d_papers_ui())$html

  expect_match(papers, "Cite the work behind 3D ENA.", fixed = TRUE)
  expect_match(papers, "Start with the method paper.", fixed = TRUE)
  expect_match(
    papers,
    "educational research and political research.",
    fixed = TRUE
  )
  expect_false(grepl("political discourse and learning research.", papers, fixed = TRUE))
  expect_false(grepl("APA 7TH EDITION", papers, fixed = TRUE))
  expect_match(papers, "Three verified references", fixed = TRUE)
  expect_match(
    papers,
    "Development of ENA 3D: A Tool for Epistemic Network Analysis in Three-Dimensional Space",
    fixed = TRUE
  )
  expect_match(
    papers,
    "The Application of ENA to Political Discourse in Taiwan: A Case Study",
    fixed = TRUE
  )
  expect_match(
    papers,
    "Effects on the Learning Achievement, Approaches to Learning, and Multi-Stage Reflection Quality",
    fixed = TRUE
  )
  expect_match(papers, "10.1007/978-3-031-76335-9_11", fixed = TRUE)
  expect_match(papers, "10.1007/978-3-031-76332-8_22", fixed = TRUE)
  expect_match(papers, "10.1016/j.compedu.2025.105397", fixed = TRUE)
  expect_equal(
    lengths(regmatches(papers, gregexpr("ena3d-copy-citation", papers, fixed = TRUE))),
    3L
  )
  expect_match(papers, "data-citation-text=", fixed = TRUE)
  expect_match(papers, "https://www.ena3d.org/papers.html", fixed = TRUE)
})


test_that("The application shell declares exactly the requested site tabs", {
  app_source <- paste(
    readLines(file.path(.site_shell_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(app_source, 'title = "Home"', fixed = TRUE)
  expect_match(app_source, 'title = "3D ENA"', fixed = TRUE)
  expect_match(app_source, 'title = "PAPERS"', fixed = TRUE)
  expect_match(app_source, 'title = "ABOUT"', fixed = TRUE)
  expect_match(app_source, 'value = "home"', fixed = TRUE)
  expect_match(app_source, 'value = "tool"', fixed = TRUE)
  expect_match(app_source, 'value = "papers"', fixed = TRUE)
  expect_match(app_source, 'value = "about"', fixed = TRUE)
  expect_match(
    app_source,
    '(?s)title = "PAPERS".*title = "ABOUT"',
    perl = TRUE
  )
  expect_match(app_source, 'id = "workspace_sections"', fixed = TRUE)
  expect_match(app_source, "input$home_brand", fixed = TRUE)
  expect_match(app_source, 'open_site_page("home")', fixed = TRUE)
  expect_match(app_source, "input$explore_trajectory", fixed = TRUE)
  expect_match(app_source, 'selected = "trajectory"', fixed = TRUE)
})
