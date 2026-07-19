library(testthat)
library(shiny)

.network_selector_roots <- c(".", "../..", "..")
.network_selector_root <- .network_selector_roots[file.exists(
  file.path(.network_selector_roots, "R", "app_module_load_dataset.R")
)][1L]
if (is.na(.network_selector_root)) {
  stop("Could not locate the 3D ENA project root.")
}
.network_selector_root <- normalizePath(.network_selector_root)

source(
  file.path(.network_selector_root, "R", "app_module_load_dataset.R"),
  local = FALSE
)
.network_selector_old_wd <- getwd()
tryCatch(
  {
    setwd(file.path(.network_selector_root, "R"))
    source("transition.R", local = FALSE)
    source("app_module_network.R", local = FALSE)
  },
  finally = setwd(.network_selector_old_wd)
)


test_that("Network selector values are typed, reversible, and collision-free", {
  values <- c(
    "shared",
    "No Network",
    "ena3d-network-v1:none",
    "中文 / café / 🚀",
    ""
  )

  encoded <- unlist(lapply(c("group", "unit"), function(type) {
    vapply(values, function(value) {
      ena3d_network_selector_encode(type, value)
    }, character(1))
  }), use.names = FALSE)

  expect_length(unique(encoded), 2L * length(values))
  expect_false(ena3d_network_selector_encode("none") %in% encoded)
  for (type in c("group", "unit")) {
    for (value in values) {
      expect_identical(
        ena3d_network_selector_decode(
          ena3d_network_selector_encode(type, value)
        ),
        list(type = type, value = enc2utf8(value))
      )
    }
  }
  expect_identical(
    ena3d_network_selector_decode(ena3d_network_selector_encode("none")),
    list(type = "none", value = NULL)
  )
  expect_null(ena3d_network_selector_decode("No Network"))
  expect_null(ena3d_network_selector_decode("ena3d-network-v1:group:f"))
  expect_null(ena3d_network_selector_decode("ena3d-network-v1:other:41"))
})


test_that("Network choices keep same-named groups and units selectable", {
  choices <- ena3d_network_choices(
    groups = c("shared", "No Network"),
    units = c("shared", "No Network")
  )
  all_values <- unname(unlist(choices, use.names = FALSE))

  expect_identical(names(choices), c("No Network", "Groups", "Units"))
  expect_identical(names(choices$Groups), c("shared", "No Network"))
  expect_identical(names(choices$Units), c("shared", "No Network"))
  expect_length(unique(all_values), 5L)

  html <- htmltools::renderTags(selectInput(
    "network_selector",
    "Show Network",
    choices = choices,
    selected = ena3d_network_selector_encode("none")
  ))$html
  expect_match(html, "<optgroup label=\"Groups\">", fixed = TRUE)
  expect_match(html, "<optgroup label=\"Units\">", fixed = TRUE)
  expect_equal(lengths(regmatches(
    html, gregexpr(">No Network</option>", html, fixed = TRUE)
  )), 3L)
})


test_that("Network renderer resolves selection type instead of guessing by text", {
  group_shared <- ena3d_network_selector_encode("group", "shared")
  unit_shared <- ena3d_network_selector_encode("unit", "shared")
  group_no_network <- ena3d_network_selector_encode("group", "No Network")
  unit_no_network <- ena3d_network_selector_encode("unit", "No Network")

  expect_identical(
    ena3d_network_selection_target(group_shared, "cohort"),
    list(type = "group", variable = "cohort", value = "shared")
  )
  expect_identical(
    ena3d_network_selection_target(unit_shared, "cohort"),
    list(type = "unit", variable = "ENA_UNIT", value = "shared")
  )
  expect_identical(
    ena3d_network_selection_target(group_no_network, "cohort"),
    list(type = "group", variable = "cohort", value = "No Network")
  )
  expect_identical(
    ena3d_network_selection_target(unit_no_network, "cohort"),
    list(type = "unit", variable = "ENA_UNIT", value = "No Network")
  )
  expect_null(ena3d_network_selection_target(
    ena3d_network_selector_encode("none"), "cohort"
  ))
  expect_null(ena3d_network_selection_target("No Network", "cohort"))
})


test_that("typed Network targets select distinct group and unit line weights", {
  ena <- list(
    line.weights = data.frame(
      cohort = structure(
        c("shared", "No Network"),
        class = c("ena.metadata", "character")
      ),
      ENA_UNIT = structure(
        c("No Network", "shared"),
        class = c("ena.metadata", "character")
      ),
      edge = structure(c(1, 9), class = c("ena.dimension", "numeric")),
      check.names = FALSE
    )
  )
  selected_mean <- function(type, value) {
    target <- ena3d_network_selection_target(
      ena3d_network_selector_encode(type, value), "cohort"
    )
    get_mean_group_lineweights_in_groups(
      ena, target$variable, target$value
    )
  }

  expect_identical(selected_mean("group", "shared"), 1)
  expect_identical(selected_mean("unit", "shared"), 9)
  expect_identical(selected_mean("group", "No Network"), 9)
  expect_identical(selected_mean("unit", "No Network"), 1)
})
