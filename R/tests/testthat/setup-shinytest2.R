# This directory contains optional historical screenshot recordings. It is
# intentionally outside the standard suite and must remain harmless when the
# optional shinytest2 package is unavailable.
if (requireNamespace("shinytest2", quietly = TRUE)) {
  shinytest2::load_app_env()
}
