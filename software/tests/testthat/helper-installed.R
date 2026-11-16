# Two runs give the same stage results bit for bit only on an installed,
# byte-compiled build. Interpreted and compiled code differ in the last bit of
# a few results, and source that pkgload loaded (devtools::test(),
# testthat::test_local()) is compiled only while it runs, so the first run of
# such a session differs from the second. A test calls this directly before
# it compares whole stage results; everything above the call runs either way.
skip_stage_identity_from_source <- function() {
  # pkgload is asked only when it is loaded, so the tests do not depend on it.
  from_source <- "pkgload" %in% loadedNamespaces() && asNamespace("pkgload")$is_dev_package("scoRch.simulator")
  skip_if(from_source, paste("Comparing whole stage results bit for bit needs an installed build of",
                             "scoRch.simulator, not one loaded from source."))
}
