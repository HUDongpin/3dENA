library(testthat)

.release_workflow_roots <- c(".", "../..", "..")
.release_workflow_root <- .release_workflow_roots[file.exists(
  file.path(.release_workflow_roots, ".github", "workflows",
            "pre-release-audit.yml")
)][1L]
if (is.na(.release_workflow_root)) {
  stop("Could not locate the 3D ENA project root.")
}
.release_workflow_root <- normalizePath(
  .release_workflow_root,
  mustWork = TRUE
)

.release_workflow_text <- function(name) {
  paste(
    readLines(
      file.path(.release_workflow_root, ".github", "workflows", name),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}

.release_workflow_fixed_count <- function(text, pattern) {
  length(strsplit(text, pattern, fixed = TRUE)[[1L]]) - 1L
}


test_that("the strict audit is reusable and every job checks out the exact SHA", {
  workflow <- .release_workflow_text("pre-release-audit.yml")

  expect_match(workflow, "  workflow_call:\n", fixed = TRUE)
  expect_match(workflow, "      audit_sha:\n", fixed = TRUE)
  expect_match(workflow, "Exact 40-character commit SHA to audit", fixed = TRUE)
  expect_match(
    workflow,
    "ENA3D_AUDIT_SHA: ${{ github.event_name == 'release' && github.sha || inputs.audit_sha }}",
    fixed = TRUE
  )
  checkout_count <- .release_workflow_fixed_count(
    workflow,
    "uses: actions/checkout@"
  )
  exact_ref_count <- .release_workflow_fixed_count(
    workflow,
    "ref: ${{ env.ENA3D_AUDIT_SHA }}"
  )
  expect_gt(checkout_count, 0L)
  expect_identical(exact_ref_count, checkout_count)
  expect_match(
    workflow,
    "grep -Eq '^[0-9a-f]{40}$'",
    fixed = TRUE
  )
  expect_match(
    workflow,
    'test "$(git rev-parse HEAD)" = "$ENA3D_AUDIT_SHA"',
    fixed = TRUE
  )
  expect_match(workflow, "  release:\n    types:\n", fixed = TRUE)
  expect_match(workflow, "      - prereleased\n", fixed = TRUE)
  expect_match(workflow, "      - published\n", fixed = TRUE)
})


test_that("release publication is manual, strict-gated, and SHA-bound", {
  workflow <- .release_workflow_text("manual-audited-release.yml")
  trigger <- strsplit(workflow, "\npermissions:\n", fixed = TRUE)[[1L]][[1L]]

  expect_match(trigger, "\non:\n  workflow_dispatch:\n", fixed = TRUE)
  expect_false(grepl(
    "\n  (push|pull_request|release|schedule):",
    trigger,
    perl = TRUE
  ))
  expect_match(workflow, "confirmation:", fixed = TRUE)
  expect_match(
    workflow,
    'test "$CONFIRMATION" = "${RELEASE_TAG}@${TARGET_SHA}"',
    fixed = TRUE
  )
  expect_match(
    workflow,
    'test "$resolved_sha" = "$TARGET_SHA"',
    fixed = TRUE
  )
  expect_match(
    workflow,
    'tag_sha=$(git rev-parse --verify "refs/tags/${RELEASE_TAG}^{commit}")',
    fixed = TRUE
  )
  expect_match(
    workflow,
    "uses: ./.github/workflows/pre-release-audit.yml",
    fixed = TRUE
  )
  expect_match(
    workflow,
    "audit_sha: ${{ needs.validate-release-target.outputs.target_sha }}",
    fixed = TRUE
  )
  expect_match(workflow, "enforcement: strict", fixed = TRUE)
  expect_match(workflow, "      - strict-audit", fixed = TRUE)
  expect_match(
    workflow,
    "ref: ${{ needs.validate-release-target.outputs.target_sha }}",
    fixed = TRUE
  )
  expect_match(workflow, "--verify-tag", fixed = TRUE)
  expect_match(workflow, '--target "$TARGET_SHA"', fixed = TRUE)
  expect_match(workflow, 'gh release create "${release_args[@]}"', fixed = TRUE)
})
