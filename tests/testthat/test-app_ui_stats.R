library(shiny)
`%>%` <- magrittr::`%>%`
source("../../R/app_ui_stats.R")

test_that("paired Wilcoxon alternative is exposed in the Stats UI", {
  html <- htmltools::renderTags(stats_ui("stats"))$html

  expect_match(html, "stats-stats_design", fixed = TRUE)
  expect_match(html, 'value="between"', fixed = TRUE)
  expect_match(html, 'value="within"', fixed = TRUE)
  expect_match(html, "stats-stats_p_adjust_method", fixed = TRUE)
  expect_match(html, 'value="holm"', fixed = TRUE)
  expect_match(html, "stats-stats_paired_alternative", fixed = TRUE)
  expect_match(html, 'value="two.sided"', fixed = TRUE)
  expect_match(html, 'value="greater"', fixed = TRUE)
  expect_match(html, 'value="less"', fixed = TRUE)
  expect_match(html, "p_adjusted", fixed = TRUE)
  expect_false(grepl('class="container"', html, fixed = TRUE))
})
