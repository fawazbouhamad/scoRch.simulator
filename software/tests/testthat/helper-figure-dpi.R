# Exercise the real renderer at screen resolution in repeated workflow tests.
# The demo job separately renders every PNG at its production resolution.
local_figure_test_dpi <- function(.env = parent.frame()) {
  labels <- character()
  record <- function(value) if (is.character(value)) labels <<- c(labels, value)
  mtext <- graphics::mtext
  text <- graphics::text.default
  testthat::local_mocked_bindings(mtext = function(text, ...) {
    record(text)
    mtext(text, ...)
  }, text.default = function(x, y = NULL, labels = seq_along(x), ...) {
    record(labels)
    text(x, y, labels, ...)
  }, .package = "graphics", .env = .env)
  withr::defer(testthat::expect_false(any(grepl("^\\(?[a-eA-E]\\)?$", labels))), envir = .env)
  specs <- scoRch.simulator:::figure_specs()
  testthat::local_mocked_bindings(figure_specs = function() lapply(specs, function(spec) {
    spec$dpi <- 72
    spec
  }), .package = "scoRch.simulator", .env = .env)
}
