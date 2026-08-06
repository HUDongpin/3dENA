library(testthat)

.property_fuzz_roots <- c(".", "../..", "..")
.property_fuzz_root <- .property_fuzz_roots[file.exists(
  file.path(.property_fuzz_roots, "tests", "audit", "property_fuzz_runner.R")
)][1L]
if (is.na(.property_fuzz_root)) {
  stop("Could not locate the 3D ENA project root.")
}
.property_fuzz_root <- normalizePath(.property_fuzz_root, mustWork = TRUE)

.property_fuzz_test_env <- new.env(parent = globalenv())
sys.source(
  file.path(.property_fuzz_root, "tests", "audit", "audit_harness.R"),
  envir = .property_fuzz_test_env
)
sys.source(
  file.path(.property_fuzz_root, "tests", "audit", "property_fuzz_runner.R"),
  envir = .property_fuzz_test_env
)


.property_fuzz_inventory_result <- function(expected, processed) {
  c(
    .property_fuzz_test_env$ena3d_property_bundled_inventory(
      expected, processed
    ),
    list(
      dataset_count = length(processed),
      deterministic_sha = TRUE,
      deterministic_bytes = TRUE,
      native_payload_object_identity = TRUE
    )
  )
}


test_that("bundled round-trip gate reconciles discovery and processed manifest", {
  expected <- .property_fuzz_test_env$ena3d_property_discover_bundled_fixtures(
    .property_fuzz_root
  )
  processed <- .property_fuzz_test_env$ena3d_property_bundled_fixtures(
    .property_fuzz_root
  )
  expect_true(length(expected) > 0L)
  expect_true(all(file.exists(expected)))
  expect_true(all(file.exists(processed)))
  expect_setequal(basename(expected), basename(processed))

  check <- .property_fuzz_test_env$ena3d_property_bundled_round_trip(
    .property_fuzz_root,
    seed = 20260719L,
    iterations = 1L
  )
  expect_identical(check$status, "passed")
  expect_identical(check$observations$expected_dataset_count, length(expected))
  expect_identical(
    check$observations$processed_fixture_count,
    length(processed)
  )
  expect_identical(check$observations$dataset_count, length(processed))
  expect_true(check$observations$inventory_set_identity)
})


test_that("bundled round-trip gate requires native payload identity", {
  gate_passed <-
    .property_fuzz_test_env$ena3d_property_bundled_round_trip_passed
  fixtures <- paste0("fixture-", seq_len(7L), ".RData")
  result <- .property_fuzz_inventory_result(fixtures, fixtures)

  expect_true(gate_passed(result))
  result$native_payload_object_identity <- FALSE
  expect_false(gate_passed(result))
  result$native_payload_object_identity <- TRUE
  result$dataset_count <- result$dataset_count - 1L
  expect_false(gate_passed(result))
})


test_that("bundled round-trip gate rejects an empty inventory", {
  gate_passed <-
    .property_fuzz_test_env$ena3d_property_bundled_round_trip_passed
  result <- .property_fuzz_inventory_result(character(), character())

  expect_false(gate_passed(result))
  expect_false(result$expected_inventory_nonempty)
  expect_false(result$processed_inventory_nonempty)
})


test_that("bundled round-trip gate rejects omitted and duplicate fixtures", {
  gate_passed <-
    .property_fuzz_test_env$ena3d_property_bundled_round_trip_passed
  expected <- c("first.RData", "second.Rdata")

  omitted <- .property_fuzz_inventory_result(expected, expected[1L])
  expect_false(gate_passed(omitted))
  expect_false(omitted$inventory_set_identity)

  duplicated <- .property_fuzz_inventory_result(
    expected,
    c(expected, expected[2L])
  )
  expect_false(gate_passed(duplicated))
  expect_false(duplicated$processed_inventory_unique)
})


test_that("a newly discovered fixture fails an unchanged processed manifest", {
  gate_passed <-
    .property_fuzz_test_env$ena3d_property_bundled_round_trip_passed
  temporary_root <- tempfile("ena3d-property-inventory-")
  sample_root <- file.path(temporary_root, "sample_data")
  dir.create(sample_root, recursive = TRUE)
  on.exit(unlink(temporary_root, recursive = TRUE), add = TRUE)
  first <- file.path(sample_root, "first.RData")
  added <- file.path(sample_root, "new-fixture.Rdata")
  expect_true(file.create(first))
  processed <- .property_fuzz_test_env$ena3d_property_discover_bundled_fixtures(
    temporary_root
  )
  expect_true(file.create(added))
  expected <- .property_fuzz_test_env$ena3d_property_discover_bundled_fixtures(
    temporary_root
  )

  result <- .property_fuzz_inventory_result(expected, processed)
  expect_setequal(basename(expected), c("first.RData", "new-fixture.Rdata"))
  expect_false(gate_passed(result))
  expect_false(result$inventory_set_identity)
})
