ena3d_brand_ui <- function() {
  tags$a(
    id = "home_brand",
    class = "ena3d-brand",
    href = "/",
    `data-site-page` = "home",
    tags$span(class = "ena3d-brand-mark", `aria-hidden` = "true", "3D"),
    tags$span(
      class = "ena3d-brand-copy",
      tags$strong("ENA"),
      tags$small("Epistemic Network Analysis")
    ),
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
          tags$a(
            id = "launch_ena",
            href = "/app",
            tags$span("Open 3D ENA", tags$span(`aria-hidden` = "true", "\u2192")),
            class = "ena3d-primary-action",
            `aria-label` = "Open the 3D ENA research workspace"
          ),
          tags$a(
            id = "explore_trajectory",
            href = "/app?workspace=trajectory",
            tags$span(
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
                "A three-dimensional ENA visualization with labeled points,",
                "group trajectories, and red, blue, and green SVD axes."
              ),
              width = "1446",
              height = "1310",
              loading = "eager",
              decoding = "async",
              fetchpriority = "high"
            )
          )
        )
      )
    ),
    tags$section(
      class = "ena3d-method-section",
      tags$div(
        class = "ena3d-section-heading",
        tags$p(class = "ena3d-kicker", "FROM DATA TO INTERPRETATION"),
        tags$h2("A focused workflow for ENA research"),
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
      tags$a(
        id = "launch_ena_note",
        href = "/app",
        tags$span("Begin an analysis", tags$span(`aria-hidden` = "true", "\u2192")),
        class = "ena3d-text-action",
        `aria-label` = "Begin an analysis in the 3D ENA workspace"
      )
    )
  )
}

ena3d_paper_citation_ui <- function(
    number,
    type,
    title,
    citation_id,
    citation_text,
    citation_html,
    doi,
    featured = FALSE) {
  tags$article(
    class = paste(
      "ena3d-paper-card",
      if (isTRUE(featured)) "ena3d-paper-card-featured" else NULL
    ),
    tags$header(
      class = "ena3d-paper-card-header",
      tags$span(class = "ena3d-paper-number", sprintf("%02d", number)),
      tags$span(class = "ena3d-paper-type", type)
    ),
    tags$h3(title),
    tags$div(
      class = "ena3d-citation-block",
      tags$p(
        id = citation_id,
        class = "ena3d-citation",
        tabindex = "-1",
        `data-citation-text` = citation_text,
        citation_html
      )
    ),
    tags$div(
      class = "ena3d-paper-actions",
      tags$button(
        type = "button",
        class = "ena3d-copy-citation",
        `data-citation-target` = citation_id,
        `data-default-label` = "Copy APA",
        `aria-label` = paste("Copy APA citation for", title),
        "Copy APA"
      ),
      tags$a(
        class = "ena3d-doi-link",
        href = doi,
        target = "_blank",
        rel = "noopener noreferrer",
        "View publication",
        tags$span(`aria-hidden` = "true", "\u2197")
      )
    )
  )
}

ena3d_papers_ui <- function() {
  method_citation <- paste0(
    "Yu, J., Hu, D., & Wang, C.-H. (2024). Development of ENA 3D: A tool for ",
    "epistemic network analysis in three-dimensional space. In Y. J. Kim & Z. ",
    "Swiecki (Eds.), Advances in quantitative ethnography (pp. 152\u2013165). ",
    "Springer. https://doi.org/10.1007/978-3-031-76335-9_11"
  )
  political_citation <- paste0(
    "Yu, J., Hamilton, E., Wang, C.-H., & Hu, D. (2024). The application of ENA ",
    "to political discourse in Taiwan: A case study. In Y. J. Kim & Z. Swiecki ",
    "(Eds.), Advances in quantitative ethnography (pp. 273\u2013287). Springer. ",
    "https://doi.org/10.1007/978-3-031-76332-8_22"
  )
  learning_citation <- paste0(
    "Tu, Y.-F., Hwang, G.-J., & Hu, D. (2025). Effects on the learning ",
    "achievement, approaches to learning, and multi-stage reflection quality of ",
    "students with different levels of digital self-efficacy in a data literacy ",
    "course: An ARCS-based self-reflective online learning model. Computers & ",
    "Education, 238, 105397. https://doi.org/10.1016/j.compedu.2025.105397"
  )

  tags$main(
    class = "site-page ena3d-papers-page",
    tags$section(
      class = "ena3d-papers-hero",
      tags$div(
        class = "ena3d-papers-hero-copy",
        tags$h1("Cite the work behind 3D ENA."),
        tags$p(
          class = "ena3d-papers-lede",
          "If 3D ENA supports your analysis, cite the foundational method paper. ",
          "The application studies below show how the approach has been used in ",
          "educational research and political research."
        )
      ),
      tags$aside(
        class = "ena3d-citation-guidance",
        tags$p(class = "ena3d-card-label", "CITATION GUIDANCE"),
        tags$h2("Start with the method paper."),
        tags$p(
          "Citing the original development paper recognizes the researchers and ",
          "developers who created and advanced 3D ENA. Add an application paper ",
          "when it directly informs your study."
        )
      )
    ),
    tags$section(
      class = "ena3d-papers-library",
      tags$div(
        class = "ena3d-papers-heading",
        tags$div(
          tags$h2("Three verified references")
        ),
        tags$p(
          "Bibliographic details were checked against publisher and DOI records. ",
          tags$a(
            href = "https://www.ena3d.org/papers.html",
            target = "_blank",
            rel = "noopener noreferrer",
            "View the source collection",
            tags$span(`aria-hidden` = "true", "\u2197")
          )
        )
      ),
      tags$div(
        class = "ena3d-paper-list",
        ena3d_paper_citation_ui(
          number = 1,
          type = "FOUNDATIONAL METHOD",
          title = paste(
            "Development of ENA 3D: A Tool for Epistemic Network Analysis",
            "in Three-Dimensional Space"
          ),
          citation_id = "ena3d-citation-method",
          citation_text = method_citation,
          citation_html = tagList(
            "Yu, J., Hu, D., & Wang, C.-H. (2024). Development of ENA 3D: A tool ",
            "for epistemic network analysis in three-dimensional space. In Y. J. ",
            "Kim & Z. Swiecki (Eds.), ",
            tags$em("Advances in quantitative ethnography"),
            " (pp. 152\u2013165). Springer. ",
            tags$a(
              href = "https://doi.org/10.1007/978-3-031-76335-9_11",
              target = "_blank",
              rel = "noopener noreferrer",
              "https://doi.org/10.1007/978-3-031-76335-9_11"
            )
          ),
          doi = "https://doi.org/10.1007/978-3-031-76335-9_11",
          featured = TRUE
        ),
        ena3d_paper_citation_ui(
          number = 2,
          type = "APPLICATION · POLITICAL DISCOURSE",
          title = paste(
            "The Application of ENA to Political Discourse in Taiwan:",
            "A Case Study"
          ),
          citation_id = "ena3d-citation-political",
          citation_text = political_citation,
          citation_html = tagList(
            "Yu, J., Hamilton, E., Wang, C.-H., & Hu, D. (2024). The application ",
            "of ENA to political discourse in Taiwan: A case study. In Y. J. Kim ",
            "& Z. Swiecki (Eds.), ",
            tags$em("Advances in quantitative ethnography"),
            " (pp. 273\u2013287). Springer. ",
            tags$a(
              href = "https://doi.org/10.1007/978-3-031-76332-8_22",
              target = "_blank",
              rel = "noopener noreferrer",
              "https://doi.org/10.1007/978-3-031-76332-8_22"
            )
          ),
          doi = "https://doi.org/10.1007/978-3-031-76332-8_22"
        ),
        ena3d_paper_citation_ui(
          number = 3,
          type = "APPLICATION · LEARNING RESEARCH",
          title = paste(
            "Effects on the Learning Achievement, Approaches to Learning, and",
            "Multi-Stage Reflection Quality of Students with Different Levels",
            "of Digital Self-Efficacy in a Data Literacy Course: An ARCS-Based",
            "Self-Reflective Online Learning Model"
          ),
          citation_id = "ena3d-citation-learning",
          citation_text = learning_citation,
          citation_html = tagList(
            "Tu, Y.-F., Hwang, G.-J., & Hu, D. (2025). Effects on the learning ",
            "achievement, approaches to learning, and multi-stage reflection ",
            "quality of students with different levels of digital self-efficacy ",
            "in a data literacy course: An ARCS-based self-reflective online ",
            "learning model. ",
            tags$em("Computers & Education"),
            ", ",
            tags$em("238"),
            ", 105397. ",
            tags$a(
              href = "https://doi.org/10.1016/j.compedu.2025.105397",
              target = "_blank",
              rel = "noopener noreferrer",
              "https://doi.org/10.1016/j.compedu.2025.105397"
            )
          ),
          doi = "https://doi.org/10.1016/j.compedu.2025.105397"
        )
      )
    )
  )
}

ena3d_team_member_ui <- function(
  number,
  name,
  role,
  affiliation,
  portrait,
  portrait_width,
  portrait_height,
  bio,
  expertise,
  variant = "standard",
  member_class = NULL,
  profile_url = NULL,
  profile_label = "View profile",
  site_link_id = NULL,
  site_page = NULL,
  site_path = NULL
) {
  profile_link <- NULL
  if (!is.null(site_page) && !is.null(site_path)) {
    profile_link <- tags$a(
      id = site_link_id,
      class = "ena3d-team-profile-link",
      href = site_path,
      `data-site-page` = site_page,
      profile_label,
      tags$span(`aria-hidden` = "true", "\u2192")
    )
  } else if (!is.null(profile_url)) {
    profile_link <- tags$a(
      class = "ena3d-team-profile-link",
      href = profile_url,
      target = "_blank",
      rel = "noopener noreferrer",
      profile_label,
      tags$span(`aria-hidden` = "true", "\u2197")
    )
  }

  tags$article(
    class = paste(
      "ena3d-team-member",
      paste0("ena3d-team-member--", variant),
      member_class
    ),
    role = "listitem",
    `data-team-order` = number,
    tags$figure(
      class = paste(
        "ena3d-team-portrait",
        if (!identical(variant, "featured")) {
          "ena3d-team-portrait--balanced"
        }
      ),
      tags$img(
        src = paste0("ena3d-assets/", portrait),
        alt = paste("Portrait of", name),
        width = as.character(portrait_width),
        height = as.character(portrait_height),
        loading = if (identical(number, "01")) "eager" else "lazy",
        decoding = "async"
      )
    ),
    tags$div(
      class = "ena3d-team-member-copy",
      tags$p(class = "ena3d-team-role", role),
      tags$h2(name),
      if (!is.null(affiliation)) {
        tags$p(class = "ena3d-team-affiliation", affiliation)
      },
      tags$p(class = "ena3d-team-bio", bio),
      tags$ul(
        class = "ena3d-team-expertise",
        `aria-label` = paste("Research expertise of", name),
        lapply(expertise, tags$li)
      ),
      profile_link
    )
  )
}

ena3d_team_ui <- function() {
  tags$main(
    class = "site-page ena3d-team-page",
    tags$section(
      class = "ena3d-team-roster",
      `aria-labelledby` = "ena3d-team-roster-title",
      tags$header(
        class = "ena3d-team-roster-heading",
        tags$div(
          tags$p(class = "ena3d-kicker", "SCHOLARS"),
          tags$h1(id = "ena3d-team-roster-title", "Meet the team.")
        ),
        tags$p(
          paste(
            "An interdisciplinary group united by a shared interest in how",
            "people learn, how evidence is modeled, and how technology can",
            "support better educational decisions."
          )
        )
      ),
      tags$div(
        class = "ena3d-team-list",
        role = "list",
        ena3d_team_member_ui(
          number = "01",
          name = "Prof. Gwo-Jen Hwang",
          role = "Chair Professor \u00b7 Educational Technology",
          affiliation = "National Taichung University of Education",
          portrait = "team-gwo-jen-hwang.jpg",
          portrait_width = 875,
          portrait_height = 1000,
          bio = paste(
            "Prof. Hwang advances technology-enhanced learning through mobile",
            "and ubiquitous learning, game-based and flipped approaches, and",
            "artificial intelligence in education. His work connects intelligent",
            "learning design with evidence-based teaching and assessment."
          ),
          expertise = c(
            "AI in education",
            "Mobile and ubiquitous learning",
            "Game-based learning",
            "Flipped learning"
          ),
          variant = "lead",
          member_class = "ena3d-team-member--hwang",
          profile_url = paste0(
            "https://ibp.ntcu.edu.tw/front/teacher_profile/teacher/member.php",
            "?ID=d7c61e9baffe7384fdecc7dbbb87e41d&PID=6"
          ),
          profile_label = "University profile"
        ),
        ena3d_team_member_ui(
          number = "02",
          name = "Dr. Yun-Fang Tu",
          role = "Assistant Professor \u00b7 Educational Technology",
          affiliation = "National Taiwan University of Science and Technology",
          portrait = "team-yun-fang-tu.jpg",
          portrait_width = 1000,
          portrait_height = 1000,
          bio = paste(
            "Dr. Tu studies how generative AI and digital learning shape learner",
            "perceptions, behavior, and outcomes. Her work combines educational",
            "data mining, visual and network analysis, and mobile-learning",
            "research to make learning processes more visible and actionable."
          ),
          expertise = c(
            "Generative AI in education",
            "Digital and mobile learning",
            "Educational data mining",
            "Network analysis"
          ),
          variant = "lead",
          member_class = "ena3d-team-member--tu",
          profile_url = "https://doi.org/10.1016/j.compedu.2025.105397",
          profile_label = "Recent research"
        ),
        ena3d_team_member_ui(
          number = "03",
          name = "Dr. Peter Hu Dongpin",
          role = "Educational researcher \u00b7 Application developer",
          affiliation = "Developer of 3D ENA Version 2.0",
          portrait = "peter-hu-portrait-web.jpg",
          portrait_width = 800,
          portrait_height = 533,
          bio = paste(
            "Dr. Hu develops theory-informed learning environments and",
            "analytical tools that connect educational research with practical",
            "technology. His work uses learning analytics, network analysis,",
            "and AI to explain and improve learning."
          ),
          expertise = c(
            "Technology-enhanced learning",
            "Learning analytics",
            "Artificial intelligence in education",
            "Application development"
          ),
          variant = "lead",
          member_class = "ena3d-team-member--peter",
          site_link_id = "meet_developer",
          site_page = "about",
          site_path = "/about",
          profile_label = "More on About"
        ),
        ena3d_team_member_ui(
          number = "04",
          name = "Mr. YU Jianxing",
          role = "Quantitative Ethnography \u00b7 Application Development",
          affiliation = tagList(
            tags$span("3D ENA Research Group \u00b7 Hong Kong"),
            tags$span("BSc Computer Science \u00b7 MA Psychology")
          ),
          portrait = "team-yu-jianxing.jpg",
          portrait_width = 1000,
          portrait_height = 1000,
          bio = paste(
            "Mr. Yu combines computer science and psychology to develop",
            "interactive tools for epistemic network analysis and quantitative",
            "ethnography. He is the key developer of 3D ENA 1.0. His research",
            "applies network analysis to political discourse, social-media data,",
            "and three-dimensional ENA visualization."
          ),
          expertise = c(
            "Epistemic network analysis",
            "Quantitative ethnography",
            "Political discourse",
            "Research software development"
          ),
          member_class = "ena3d-team-member--yu",
          profile_url = paste0(
            "https://researchoutput.ncku.edu.tw/zh/publications/",
            "development-of-ena-3d-a-tool-for-epistemic-network-analysis-in-th/"
          ),
          profile_label = "ENA 3D research"
        ),
        ena3d_team_member_ui(
          number = "05",
          name = "Dr. Huang Lingyun",
          role = "Assistant Professor \u00b7 Curriculum and Instruction",
          affiliation = tagList(
            tags$span("The Education University of Hong Kong"),
            tags$span("PhD \u00b7 McGill University")
          ),
          portrait = "team-huang-lingyun.jpg",
          portrait_width = 690,
          portrait_height = 860,
          bio = paste(
            "Dr. Huang designs intelligent, adaptive learning environments and",
            "uses AI-enabled learning analytics to uncover cognitive,",
            "metacognitive, behavioral, and emotional patterns in learning",
            "trajectories. His work translates learning-science evidence into",
            "more effective teaching and learning."
          ),
          expertise = c(
            "Technology-rich learning",
            "AI-enabled learning analytics",
            "Regulated learning",
            "Teacher digital literacy"
          ),
          member_class = "ena3d-team-member--huang",
          profile_url = "https://repository.eduhk.hk/en/persons/lingyun-huang/",
          profile_label = "University profile"
        ),
        ena3d_team_member_ui(
          number = "06",
          name = "Dr. Phoebe, KANG Xia",
          role = "Senior Lecturer \u00b7 Mathematics Education",
          affiliation = tagList(
            tags$span("School of Mathematics and Information Science \u00b7 Guangzhou University"),
            tags$span("PhD \u00b7 The University of Hong Kong")
          ),
          portrait = "team-phoebe-kang-xia.jpg",
          portrait_width = 1000,
          portrait_height = 1000,
          bio = paste(
            "Dr. Kang researches how emotions, motivation, and culture shape",
            "mathematics learning. Her work combines educational measurement",
            "with quantitative and qualitative inquiry to understand differences",
            "in secondary students\u2019 mathematical experiences."
          ),
          expertise = c(
            "Mathematics education",
            "Achievement emotions",
            "Educational measurement",
            "Cross-cultural learning"
          ),
          member_class = "ena3d-team-member--kang"
        ),
        ena3d_team_member_ui(
          number = "07",
          name = "Dr. WU Yajun",
          role = "Senior Lecturer \u00b7 Applied Linguistics",
          affiliation = "School of Humanities \u00b7 Foshan University",
          portrait = "team-wu-yajun.jpg",
          portrait_width = 1000,
          portrait_height = 667,
          bio = paste(
            "Dr. Wu studies digital literacy and the motivational and relational",
            "foundations of English-language learning. His research examines how",
            "teacher support, learner grit, engagement, and technology use shape",
            "EFL achievement."
          ),
          expertise = c(
            "Teacher digital literacy",
            "Applied linguistics",
            "EFL motivation",
            "Student engagement"
          ),
          member_class = "ena3d-team-member--wu"
        ),
        ena3d_team_member_ui(
          number = "08",
          name = "Dr. Cao Yuan",
          role = "Postdoctoral Fellow \u00b7 Curriculum and Instruction",
          affiliation = "The Education University of Hong Kong",
          portrait = "team-cao-yuan.jpg",
          portrait_width = 1000,
          portrait_height = 667,
          bio = paste(
            "Dr. Cao researches how digital and AI-supported self-assessment can",
            "strengthen reflection, engagement, and learning in higher education.",
            "Her work connects educational technology with evidence-based",
            "assessment design and data-informed teaching."
          ),
          expertise = c(
            "AI-supported self-assessment",
            "Higher education assessment",
            "Educational technology",
            "Learner engagement"
          ),
          member_class = "ena3d-team-member--cao"
        ),
        ena3d_team_member_ui(
          number = "09",
          name = "Dr. LI Jun",
          role = "Shadow Education \u00b7 Education Policy",
          affiliation = tagList(
            tags$span("Member \u00b7 HKU Shadow Education SIG"),
            tags$span("PhD \u00b7 The University of Hong Kong")
          ),
          portrait = "team-li-jun.jpg",
          portrait_width = 1000,
          portrait_height = 667,
          bio = paste(
            "Dr. Li studies shadow education and private tutoring through",
            "qualitative, social-network, and policy lenses. His work examines",
            "guanxi, brokerage, regulation, and educational equity within",
            "changing education systems."
          ),
          expertise = c(
            "Shadow education",
            "Education policy",
            "Social network research",
            "Educational equity"
          ),
          member_class = "ena3d-team-member--li",
          profile_url = "https://doi.org/10.1111/ejed.70184",
          profile_label = "Recent research"
        )
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
              "Developer of 3D ENA Version 2.0"
            )
          )
        ),
        tags$div(
          class = "ena3d-about-summary",
          tags$p(
            "Dr. Peter Hu develops theory-informed, evidence-based digital learning environments ",
            "and learning analytical tools that connect educational research with artificial ",
            "intelligence technology."
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
      tags$a(
        id = "launch_ena_about",
        href = "/app",
        tags$span("Open workspace", tags$span(`aria-hidden` = "true", "\u2192")),
        class = "ena3d-primary-action",
        `aria-label` = "Open the 3D ENA research workspace"
      )
    ),
    tags$p(
      class = "ena3d-profile-source",
      paste(
        "The 3D ENA Version 2.0 project is built on the previous 3D ENA",
        "Version 1.0. Dr. Peter Hu is charge of revolutionizing the 3D ENA",
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
      tags$span("Research Visualization for Epistemic Network Analysis")
    ),
    tags$span(paste0("\u00a9 ", format(Sys.Date(), "%Y"), " Dr. Peter Hu Dongpin"))
  )
}
