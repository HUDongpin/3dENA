ena3d_brand_ui <- function() {
  actionLink(
    "home_brand",
    label = tagList(
      tags$span(class = "ena3d-brand-mark", `aria-hidden` = "true", "3D"),
      tags$span(
        class = "ena3d-brand-copy",
        tags$strong("ENA"),
        tags$small("Epistemic Network Analysis")
      )
    ),
    class = "ena3d-brand",
    `aria-label` = "Return to the 3D ENA home page"
  )
}

ena3d_home_ui <- function() {
  tags$main(
    class = "site-page ena3d-home-page",
    tags$section(
      class = "ena3d-hero",
      tags$div(
        class = "ena3d-hero-copy",
        tags$h1(
          tags$span("Make epistemic connections "),
          tags$em("visible in three dimensions.")
        ),
        tags$p(
          class = "ena3d-hero-lede",
          "Explore knowledge structures in a shared 3D rotation, compare groups, ",
          "and trace ordered centroid paths to see how networks move over time."
        ),
        tags$div(
          class = "ena3d-hero-actions",
          actionButton(
            "launch_ena",
            label = tags$span("Open 3D ENA", tags$span(`aria-hidden` = "true", "\u2192")),
            class = "ena3d-primary-action",
            `aria-label` = "Open the 3D ENA research workspace"
          ),
          actionButton(
            "explore_trajectory",
            label = tags$span(
              "Explore trajectory",
              tags$span(`aria-hidden` = "true", "\u2192")
            ),
            class = "ena3d-secondary-action",
            `aria-label` = "Open the centroid trajectory analysis workspace"
          )
        ),
        tags$dl(
          class = "ena3d-capability-list",
          tags$div(tags$dt("TIME"), tags$dd("Ordered centroids")),
          tags$div(tags$dt("2\u20133D"), tags$dd("Linked views")),
          tags$div(
            tags$dt("jENA"),
            tags$dd(class = "ena3d-capability-case-sensitive", "PARITY for rENA")
          )
        )
      ),
      tags$figure(
        class = "ena3d-hero-visual",
        tags$div(
          class = "ena3d-trajectory-showcase",
          tags$div(
            class = "ena3d-visual-heading",
            tags$div(
              tags$p(class = "ena3d-visual-kicker", "TRAJECTORY ANALYSIS"),
              tags$h2("Follow change through time")
            ),
            tags$span(class = "ena3d-feature-badge", "CORE FEATURE")
          ),
          tags$div(
            class = "ena3d-figure-frame",
            tags$img(
              src = "ena3d-assets/trajectory-home-preview-3d.png",
              alt = paste(
                "A three-dimensional centroid trajectory with colored time",
                "points connected by blue path segments across the SVD1,",
                "SVD2, and SVD3 axes."
              ),
              width = "556",
              height = "564",
              loading = "eager",
              decoding = "async",
              fetchpriority = "high"
            )
          ),
          tags$ul(
            class = "ena3d-trajectory-key",
            `aria-label` = "Trajectory visualization features",
            tags$li(class = "ena3d-key-order", "Ordered nodes"),
            tags$li(class = "ena3d-key-direction", "Direction"),
            tags$li(class = "ena3d-key-comparison", "Group comparison")
          )
        )
      )
    ),
    tags$section(
      class = "ena3d-method-section",
      tags$div(
        class = "ena3d-section-heading",
        tags$p(class = "ena3d-kicker", "FROM DATA TO INTERPRETATION"),
        tags$h2("A focused workflow for exploratory ENA research"),
        tags$p(
          "Move from a validated ENA dataset to interpretable spatial, network, ",
          "statistical, and longitudinal views without leaving the workspace."
        )
      ),
      tags$ol(
        class = "ena3d-method-grid",
        tags$li(
          tags$span(class = "ena3d-step-number", "01"),
          tags$h3("Load"),
          tags$p(
            "Build ENA from raw Excel or CSV data, or start with a reviewed ",
            "sample or versioned .ena3d.json exchange file."
          )
        ),
        tags$li(
          tags$span(class = "ena3d-step-number", "02"),
          tags$h3("Configure"),
          tags$p("Choose ENA dimensions, groups, comparison settings, and plot controls.")
        ),
        tags$li(
          tags$span(class = "ena3d-step-number", "03"),
          tags$h3("Interpret"),
          tags$p("Inspect networks, differences, statistics, and ordered centroid trajectories.")
        )
      )
    ),
    tags$section(
      class = "ena3d-research-note",
      tags$p(class = "ena3d-kicker", "DESIGNED FOR RESEARCH"),
      tags$blockquote(
        "A visual analytics workspace should make complex relationships easier to examine ",
        "while keeping analytical choices visible."
      ),
      actionButton(
        "launch_ena_note",
        label = tags$span("Begin an analysis", tags$span(`aria-hidden` = "true", "\u2192")),
        class = "ena3d-text-action",
        `aria-label` = "Begin an analysis in the 3D ENA workspace"
      )
    )
  )
}

ena3d_about_ui <- function() {
  tags$main(
    class = "site-page ena3d-about-page",
    tags$section(
      class = "ena3d-about-hero",
      tags$div(
        class = "ena3d-about-hero-copy",
        tags$div(
          class = "ena3d-about-heading",
          tags$h1("Dr. Peter Hu Dongpin"),
          tags$p(
            class = "ena3d-about-role",
            paste(
              "Educational researcher · Application developer ·",
              "Developer of 3D ENA Version 0.2.0"
            )
          )
        ),
        tags$div(
          class = "ena3d-about-summary",
          tags$p(
            "Dr. Peter Hu develops theory-informed, evidence-based learning environments ",
            "and analytical tools that connect educational research with practical technology."
          ),
          tags$p(
            "His work asks how learning technology can improve outcomes and how evidence ",
            "from learning processes can explain and predict that progress."
          ),
          tags$a(
            class = "ena3d-profile-link",
            href = "https://www.hudongpin.com/",
            target = "_blank",
            rel = "noopener noreferrer",
            "Visit academic profile",
            tags$span(`aria-hidden` = "true", "\u2197")
          )
        )
      ),
      tags$figure(
        class = "ena3d-about-portrait",
        tags$img(
          src = "ena3d-assets/peter-hu-portrait.png",
          alt = "Portrait of Dr. Peter Hu Dongpin",
          width = "1536",
          height = "1024",
          loading = "eager",
          decoding = "async",
          fetchpriority = "high"
        ),
        tags$figcaption(
          tags$strong("Dr. Peter Hu Dongpin"),
          tags$span("Educational researcher and application developer")
        )
      )
    ),
    tags$section(
      class = "ena3d-profile-grid",
      tags$article(
        class = "ena3d-profile-card ena3d-profile-card-featured",
        tags$p(class = "ena3d-card-label", "RESEARCH AGENDA"),
        tags$h2("Technology that supports learning — and evidence that explains why."),
        tags$p(
          "His research integrates experiment-based inquiry, learning analytics, ",
          "and application development to study meaningful educational change."
        )
      ),
      tags$article(
        class = "ena3d-profile-card",
        tags$p(class = "ena3d-card-label", "EDUCATION"),
        tags$h3("Interdisciplinary by design"),
        tags$ul(
          tags$li("PhD in Educational Technology, The University of Hong Kong"),
          tags$li("BSc in Computer Science (Machine Learning & AI), University of London")
        )
      ),
      tags$article(
        class = "ena3d-profile-card",
        tags$p(class = "ena3d-card-label", "RESEARCH AREAS"),
        tags$h3("Learning, technology, and evidence"),
        tags$ul(
          tags$li("Technology-enhanced learning"),
          tags$li("Learning analytics and network analysis"),
          tags$li("Artificial intelligence in education"),
          tags$li("Content and language integrated learning")
        )
      )
    ),
    tags$section(
      class = "ena3d-about-cta",
      tags$div(
        tags$p(class = "ena3d-kicker", "3D ENA"),
        tags$h2("Explore the research tool."),
        tags$p("Move directly into the complete interactive 3D ENA workspace.")
      ),
      actionButton(
        "launch_ena_about",
        label = tags$span("Open workspace", tags$span(`aria-hidden` = "true", "\u2192")),
        class = "ena3d-primary-action",
        `aria-label` = "Open the 3D ENA research workspace"
      )
    ),
    tags$p(
      class = "ena3d-profile-source",
      paste(
        "The 3D ENA Version 0.2.0 project is inspired by the previous ENA 3D",
        "Version 0.1.0. Dr. Peter Hu is charge of revolutionizing the 3D ENA",
        "tool since 2026 July 17. Welcome research collaboration worldwide."
      )
    )
  )
}

ena3d_footer_ui <- function() {
  tags$footer(
    class = "ena3d-site-footer",
    tags$div(
      tags$strong("3D ENA"),
      tags$span("Research visualization for Epistemic Network Analysis")
    ),
    tags$span(paste0("\u00a9 ", format(Sys.Date(), "%Y"), " Dr. Peter Hu Dongpin"))
  )
}
