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
  file.path(.site_shell_root, "R", "trajectory_plot.R"),
  envir = .site_shell_env
)
sys.source(
  file.path(.site_shell_root, "R", "app_ui_site.R"),
  envir = .site_shell_env
)


test_that("Brand is an accessible real route link that returns to Home", {
  brand <- htmltools::renderTags(.site_shell_env$ena3d_brand_ui())$html

  expect_match(brand, 'id="home_brand"', fixed = TRUE)
  expect_match(brand, 'class="ena3d-brand"', fixed = TRUE)
  expect_match(
    brand,
    'aria-label="Return to the 3D ENA home page"',
    fixed = TRUE
  )
  expect_match(brand, 'href="/"', fixed = TRUE)
  expect_match(brand, 'data-site-page="home"', fixed = TRUE)
  expect_false(grepl('href="#"', brand, fixed = TRUE))
})


test_that("Home gives researchers a direct path into 3D ENA", {
  home <- htmltools::renderTags(.site_shell_env$ena3d_home_ui())$html

  expect_false(grepl("3D TRAJECTORY ANALYSIS", home, fixed = TRUE))
  expect_match(home, "Open 3D ENA", fixed = TRUE)
  expect_match(home, "id=\"launch_ena\"", fixed = TRUE)
  expect_match(home, "id=\"explore_trajectory\"", fixed = TRUE)
  expect_match(
    home,
    '<a id="launch_ena" href="/app"',
    fixed = TRUE
  )
  expect_match(
    home,
    '<a id="explore_trajectory" href="/app?workspace=trajectory"',
    fixed = TRUE
  )
  expect_match(
    home,
    '<a id="launch_ena_note" href="/app"',
    fixed = TRUE
  )
  expect_false(grepl('<button id="launch_ena"', home, fixed = TRUE))
  expect_false(grepl('<button id="explore_trajectory"', home, fixed = TRUE))
  expect_match(home, "Explore trajectory", fixed = TRUE)
  expect_match(
    home,
    'src="ena3d-assets/trajectory-home-preview-3d.png"',
    fixed = TRUE
  )
  expect_match(home, 'width="1446"', fixed = TRUE)
  expect_match(home, 'height="1310"', fixed = TRUE)
  expect_false(grepl("home_trajectory_plot", home, fixed = TRUE))
  expect_false(grepl("Drag to rotate", home, fixed = TRUE))
  expect_false(grepl("ena3d-assets/trajectory-home-preview.svg", home, fixed = TRUE))
  expect_false(grepl("FIG. 01", home, fixed = TRUE))
  expect_false(grepl("Ordered centroid paths reveal", home, fixed = TRUE))
  expect_match(home, "jENA", fixed = TRUE)
  expect_match(home, "PARITY for rENA", fixed = TRUE)
  expect_false(grepl("Bootstrap uncertainty", home, fixed = TRUE))
  expect_match(home, "Follow change through time", fixed = TRUE)
  expect_false(grepl("Ordered nodes", home, fixed = TRUE))
  expect_false(grepl("Direction", home, fixed = TRUE))
  expect_false(grepl("Group comparison", home, fixed = TRUE))
  expect_false(grepl("ena3d-trajectory-key", home, fixed = TRUE))
  expect_false(grepl("uncertainty intervals", home, fixed = TRUE))
  expect_match(home, "Load", fixed = TRUE)
  expect_match(home, "Configure", fixed = TRUE)
  expect_match(home, "Interpret", fixed = TRUE)
  expect_match(home, "A focused workflow for ENA research", fixed = TRUE)
  expect_false(grepl(
    "A focused workflow for exploratory ENA research",
    home,
    fixed = TRUE
  ))
  preview_path <- file.path(
    .site_shell_root,
    "images",
    "trajectory-home-preview-3d.png"
  )
  expect_true(file.exists(preview_path))
  expect_gt(file.info(preview_path)$size, 0)
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
    "font-size: var(--ena-type-display);",
    fixed = TRUE
  )
  expect_match(css, ".ena3d-trajectory-showcase", fixed = TRUE)
  expect_match(css, ".ena3d-figure-frame img", fixed = TRUE)
  expect_false(grepl(".ena3d-plot-instructions", css, fixed = TRUE))
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


test_that("Primary site titles share one responsive display scale", {
  css <- paste(
    readLines(
      file.path(.site_shell_root, "R", "www", "app_shell.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    css,
    "--ena-type-display: clamp(2.9rem, 4.15vw, 4.5rem);",
    fixed = TRUE
  )
  expect_match(
    css,
    "--ena-type-display: clamp(2.35rem, 10.8vw, 3.2rem);",
    fixed = TRUE
  )
  expect_match(
    css,
    "(?s)\\.ena3d-hero h1\\s*\\{[^}]*font-size: var\\(--ena-type-display\\);",
    perl = TRUE
  )
  expect_match(
    css,
    "(?s)\\.ena3d-papers-hero h1\\s*\\{[^}]*font-size: var\\(--ena-type-display\\);",
    perl = TRUE
  )
  expect_match(
    css,
    "(?s)\\.ena3d-team-roster-heading h1\\s*\\{[^}]*font-size: var\\(--ena-type-display\\);",
    perl = TRUE
  )
  expect_match(
    css,
    "(?s)\\.ena3d-about-heading h1\\s*\\{[^}]*font-size: var\\(--ena-type-display\\);",
    perl = TRUE
  )
  expect_equal(
    lengths(regmatches(
      css,
      gregexpr("font-size: var(--ena-type-display);", css, fixed = TRUE)
    )),
    4L
  )
})


test_that("About presents the verified public developer profile", {
  about <- htmltools::renderTags(.site_shell_env$ena3d_about_ui())$html
  about_text <- trimws(gsub("\\s+", " ", about))

  expect_match(about, "Dr. Peter Hu Dongpin", fixed = TRUE)
  expect_match(about, "Developer of 3D ENA Version 2.0", fixed = TRUE)
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
    about_text,
    paste(
      "Dr. Peter Hu develops theory-informed, evidence-based digital learning",
      "environments and learning analytical tools that connect educational",
      "research with artificial intelligence technology."
    ),
    fixed = TRUE
  )
  expect_match(
    about,
    "The 3D ENA Version 2.0 project is built on the previous 3D ENA Version 1.0.",
    fixed = TRUE
  )
  expect_match(
    about,
    "Dr. Peter Hu is charge of revolutionizing the 3D ENA tool since 2026 July 17.",
    fixed = TRUE
  )
  expect_match(about, "Welcome research collaboration worldwide.", fixed = TRUE)
  expect_match(
    about,
    '<a id="launch_ena_about" href="/app"',
    fixed = TRUE
  )
  expect_false(grepl('<button id="launch_ena_about"', about, fixed = TRUE))
  expect_false(grepl("Biographical details are based", about, fixed = TRUE))
})


test_that("Papers provides four copy-ready references in the requested order", {
  papers <- htmltools::renderTags(.site_shell_env$ena3d_papers_ui())$html

  expect_false(grepl("PAPERS &amp; CITATION", papers, fixed = TRUE))
  expect_match(papers, "Cite the work behind 3D ENA.", fixed = TRUE)
  expect_match(papers, "Start with the method paper.", fixed = TRUE)
  expect_match(
    papers,
    "use in educational research and political research.",
    fixed = TRUE
  )
  expect_false(grepl("political discourse and learning research.", papers, fixed = TRUE))
  expect_false(grepl("APA 7TH EDITION", papers, fixed = TRUE))
  expect_match(papers, "Four references", fixed = TRUE)
  expect_match(papers, "Publication links are included when available.", fixed = TRUE)

  expected_titles <- c(
    paste(
      "Design and development from rENA to jENA:",
      "Accelerating the creation of web-based Open ENA tools"
    ),
    paste(
      "Development of ENA 3D: A Tool for Epistemic Network Analysis",
      "in Three-Dimensional Space"
    ),
    paste(
      "Effects on the Learning Achievement, Approaches to Learning, and",
      "Multi-Stage Reflection Quality of Students with Different Levels",
      "of Digital Self-Efficacy in a Data Literacy Course: An ARCS-Based",
      "Self-Reflective Online Learning Model"
    ),
    paste(
      "The Application of ENA to Political Discourse in Taiwan:",
      "A Case Study"
    )
  )
  title_positions <- vapply(
    expected_titles,
    function(title) regexpr(title, papers, fixed = TRUE)[[1L]],
    integer(1L)
  )
  expect_true(all(title_positions > 0L))
  expect_true(all(diff(title_positions) > 0L))

  expect_match(papers, "Hu, D., Hamilton, E., Tu, Y. F., &amp; Xu, Q. (2026, November).", fixed = TRUE)
  expect_match(papers, "[Conference paper].", fixed = TRUE)
  expect_match(papers, "International Conference on Quantitative Ethnography", fixed = TRUE)
  expect_match(papers, "10.1007/978-3-031-76335-9_11", fixed = TRUE)
  expect_match(papers, "10.1007/978-3-031-76332-8_22", fixed = TRUE)
  expect_match(papers, "10.1016/j.compedu.2025.105397", fixed = TRUE)
  expect_equal(
    lengths(regmatches(papers, gregexpr("ena3d-copy-citation", papers, fixed = TRUE))),
    4L
  )
  expect_equal(
    lengths(regmatches(papers, gregexpr("ena3d-doi-link", papers, fixed = TRUE))),
    3L
  )
  expect_match(papers, "data-citation-text=", fixed = TRUE)
  expect_match(papers, "https://www.ena3d.org/papers.html", fixed = TRUE)
})

test_that("Team presents ten research profiles in the requested order", {
  team <- htmltools::renderTags(.site_shell_env$ena3d_team_ui())$html
  expected_names <- c(
    "Prof. Gwo-Jen Hwang",
    "Dr. Yun-Fang Tu",
    "Dr. Peter Hu Dongpin",
    "Mr. YU Jianxing",
    "Dr. Huang Lingyun",
    "Dr. Phoebe, KANG Xia",
    "Dr. WU Yajun",
    "Dr. Cao Yuan",
    "Dr. HU Xiao",
    "Dr. LI Jun"
  )
  name_positions <- vapply(
    expected_names,
    function(name) regexpr(name, team, fixed = TRUE)[[1L]],
    integer(1)
  )

  expect_false(grepl("ena3d-team-hero", team, fixed = TRUE))
  expect_false(grepl("ena3d-team-lede", team, fixed = TRUE))
  expect_false(grepl("ena3d-team-spectrum", team, fixed = TRUE))
  expect_false(grepl("RESEARCH SPECTRUM", team, fixed = TRUE))
  expect_false(grepl("Meet the 3D ENA Research Team", team, fixed = TRUE))
  expect_match(team, ">SCHOLARS<", fixed = TRUE)
  expect_match(
    team,
    '<h1 id="ena3d-team-roster-title">Meet the team.</h1>',
    fixed = TRUE
  )
  expect_false(grepl("Nine scholars connect", team, fixed = TRUE))
  expect_true(all(name_positions > 0L))
  expect_true(all(diff(name_positions) > 0L))
  expect_equal(
    lengths(regmatches(team, gregexpr('role="listitem"', team, fixed = TRUE))),
    10L
  )
  expect_match(
    team,
    paste0(
      '<p class="ena3d-team-role">',
      "Chair Professor \u00b7 Educational Technology</p>"
    ),
    fixed = TRUE
  )
  expect_match(team, "National Taichung University of Education", fixed = TRUE)
  expect_match(
    team,
    paste0(
      '<p class="ena3d-team-role">',
      "Assistant Professor \u00b7 Educational Technology</p>"
    ),
    fixed = TRUE
  )
  expect_match(
    team,
    paste0(
      '<p class="ena3d-team-affiliation">',
      "National Taiwan University of Science and Technology</p>"
    ),
    fixed = TRUE
  )
  expect_lt(
    regexpr(
      "Assistant Professor \u00b7 Educational Technology",
      team,
      fixed = TRUE
    )[[1L]],
    regexpr("<h2>Dr. Yun-Fang Tu</h2>", team, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("<h2>Dr. Yun-Fang Tu</h2>", team, fixed = TRUE)[[1L]],
    regexpr(
      "National Taiwan University of Science and Technology",
      team,
      fixed = TRUE
    )[[1L]]
  )
  expect_false(grepl("Soochow University", team, fixed = TRUE))
  expect_match(team, "10.1016/j.compedu.2025.105397", fixed = TRUE)
  expect_match(team, "Developer of 3D ENA Version 2.0", fixed = TRUE)
  expect_match(
    team,
    "Assistant Professor \u00b7 Curriculum and Instruction",
    fixed = TRUE
  )
  expect_match(team, "PhD \u00b7 McGill University", fixed = TRUE)
  expect_match(team, "School of Humanities \u00b7 Foshan University", fixed = TRUE)
  expect_match(
    team,
    "Postdoctoral Fellow \u00b7 Curriculum and Instruction",
    fixed = TRUE
  )
  expect_match(team, "HKU Shadow Education SIG", fixed = TRUE)
  expect_match(team, "3D ENA Research Group \u00b7 Hong Kong", fixed = TRUE)
  expect_false(grepl("ENA 3D Research Group", team, fixed = TRUE))
  expect_match(team, "BSc Computer Science \u00b7 MA Psychology", fixed = TRUE)
  expect_match(team, "He is the key developer of 3D ENA 1.0.", fixed = TRUE)
  hu_profile <- substring(
    team,
    regexpr("Dr. HU Xiao", team, fixed = TRUE)[[1L]]
  )
  expect_match(
    hu_profile,
    "Assistant Professor · Applied Linguistics",
    fixed = TRUE
  )
  expect_match(
    hu_profile,
    "City University of Macau",
    fixed = TRUE
  )
  expect_false(grepl(
    "Faculty",
    hu_profile,
    fixed = TRUE
  ))
  expect_match(
    hu_profile,
    "PhD in Education · The University of Hong Kong",
    fixed = TRUE
  )
  expect_match(
    hu_profile,
    "top-tier SSCI journals such as System, Assessing Writing, and the International Journal of Applied Linguistics",
    fixed = TRUE
  )
  expect_match(
    hu_profile,
    "Spanish Ministry of Science",
    fixed = TRUE
  )
  expect_match(
    hu_profile,
    "Guangdong Provincial Philosophy and Social Sciences Planning",
    fixed = TRUE
  )
  expect_match(
    hu_profile,
    "https://fhss.cityu.edu.mo/en/applied-linguistics/458",
    fixed = TRUE
  )
  expect_lt(
    regexpr("Dr. HU Xiao", team, fixed = TRUE)[[1L]],
    regexpr("Dr. LI Jun", team, fixed = TRUE)[[1L]]
  )
  li_profile <- substring(
    team,
    regexpr("Dr. LI Jun", team, fixed = TRUE)[[1L]]
  )
  expect_true(
    regexpr(
      "Member \u00b7 HKU Shadow Education SIG",
      li_profile,
      fixed = TRUE
    )[[1L]] <
      regexpr(
        "PhD \u00b7 The University of Hong Kong",
        li_profile,
        fixed = TRUE
      )[[1L]]
  )
  expect_match(team, 'src="ena3d-assets/team-gwo-jen-hwang.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-yun-fang-tu.jpg"', fixed = TRUE)
  expect_match(
    team,
    paste0(
      'class="ena3d-team-member ena3d-team-member--lead ',
      'ena3d-team-member--hwang"'
    ),
    fixed = TRUE
  )
  expect_false(grepl("ena3d-team-member--featured", team, fixed = TRUE))
  expect_match(team, 'src="ena3d-assets/peter-hu-portrait-web.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-huang-lingyun.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-phoebe-kang-xia.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-wu-yajun.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-cao-yuan.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-hu-xiao.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-li-jun.jpg"', fixed = TRUE)
  expect_match(team, 'src="ena3d-assets/team-yu-jianxing.jpg"', fixed = TRUE)
  expect_equal(
    lengths(regmatches(
      team,
      gregexpr("ena3d-team-portrait--balanced", team, fixed = TRUE)
    )),
    10L
  )
  expect_false(grepl("<figcaption", team, fixed = TRUE))
  developer_link <- regmatches(
    team,
    regexpr('<a[^>]*id="meet_developer"[^>]*>', team, perl = TRUE)
  )
  expect_length(developer_link, 1L)
  expect_match(developer_link, 'href="/about"', fixed = TRUE)
  expect_match(developer_link, 'data-site-page="about"', fixed = TRUE)
  expect_false(grepl('href="#"', developer_link, fixed = TRUE))
  expect_match(team, "More on About", fixed = TRUE)
  expect_match(team, 'rel="noopener noreferrer"', fixed = TRUE)
  expect_false(grepl("PedaNova", team, fixed = TRUE))
  expect_false(grepl(">CEO<", team, fixed = TRUE))
  expect_false(grepl(">Founder<", team, fixed = TRUE))
})


test_that("Team layout has dedicated responsive and accessible styling", {
  css <- paste(
    readLines(
      file.path(.site_shell_root, "R", "www", "app_shell.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(css, ".ena3d-team-page", fixed = TRUE)
  expect_match(css, ".ena3d-team-list", fixed = TRUE)
  expect_false(grepl(".ena3d-team-hero", css, fixed = TRUE))
  expect_false(grepl(".ena3d-team-spectrum", css, fixed = TRUE))
  expect_match(css, "grid-template-columns: repeat(12, minmax(0, 1fr));", fixed = TRUE)
  expect_match(css, ".ena3d-team-member--featured", fixed = TRUE)
  expect_match(css, ".ena3d-team-member--closing", fixed = TRUE)
  expect_match(css, ".ena3d-team-portrait--balanced", fixed = TRUE)
  expect_match(
    css,
    "aspect-ratio: 4 / 3;",
    fixed = TRUE
  )
  expect_match(
    css,
    "--ena3d-team-portrait-y:",
    fixed = TRUE
  )
  expect_match(
    css,
    paste(
      ".ena3d-team-member--hwang .ena3d-team-portrait img {",
      "  object-position: 50% 10%;",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_false(grepl("--ena3d-team-portrait-scale", css, fixed = TRUE))
  expect_match(css, ".ena3d-team-member--yu", fixed = TRUE)
  expect_match(css, ".ena3d-team-member--hu-xiao", fixed = TRUE)
  expect_match(css, ".ena3d-team-profile-link:focus-visible", fixed = TRUE)
  expect_match(
    css,
    "@media (min-width: 768px) and (max-width: 991.98px)",
    fixed = TRUE
  )
  expect_match(css, ".ena3d-team-member {\n    grid-column: 1 / -1;", fixed = TRUE)
})


test_that("static content and the stateful workspace have separate shells", {
  app_source <- paste(
    readLines(file.path(.site_shell_root, "R", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  static_source <- paste(
    readLines(
      file.path(.site_shell_root, "R", "static_site.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(app_source, 'title = "3D ENA"', fixed = TRUE)
  expect_match(app_source, 'value = "tool"', fixed = TRUE)
  expect_match(app_source, 'href = "/papers"', fixed = TRUE)
  expect_match(app_source, 'href = "/team"', fixed = TRUE)
  expect_match(app_source, 'href = "/about"', fixed = TRUE)
  expect_match(app_source, 'id = "workspace_sections"', fixed = TRUE)
  expect_match(app_source, "ena3d_connection_guard_ui()", fixed = TRUE)
  expect_false(grepl("ena3d_home_ui()", app_source, fixed = TRUE))
  expect_false(grepl("ena3d_papers_ui()", app_source, fixed = TRUE))
  expect_false(grepl("ena3d_team_ui()", app_source, fixed = TRUE))
  expect_false(grepl("ena3d_about_ui()", app_source, fixed = TRUE))

  expect_match(static_source, 'title = "Home"', fixed = TRUE)
  expect_match(static_source, 'title = "PAPERS"', fixed = TRUE)
  expect_match(static_source, 'title = "TEAM"', fixed = TRUE)
  expect_match(static_source, 'title = "ABOUT"', fixed = TRUE)
  expect_match(static_source, 'value = "home"', fixed = TRUE)
  expect_match(static_source, 'value = "papers"', fixed = TRUE)
  expect_match(static_source, 'value = "team"', fixed = TRUE)
  expect_match(static_source, 'value = "about"', fixed = TRUE)
  expect_false(grepl("ena3d_connection_guard_ui", static_source, fixed = TRUE))

  expect_false(grepl("input$home_brand", app_source, fixed = TRUE))
  expect_false(grepl("input$meet_developer", app_source, fixed = TRUE))
  expect_match(app_source, "input$ena3d_workspace_entry", fixed = TRUE)
  expect_match(app_source, 'selected = "trajectory"', fixed = TRUE)
})


test_that("The site footer preserves the requested capitalization", {
  footer <- htmltools::renderTags(.site_shell_env$ena3d_footer_ui())$html

  expect_match(
    footer,
    "Research Visualization for Epistemic Network Analysis",
    fixed = TRUE
  )
  expect_false(grepl(
    "Research visualization for Epistemic Network Analysis",
    footer,
    fixed = TRUE
  ))
})
