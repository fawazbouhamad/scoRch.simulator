# Two runs give the same stage results bit for bit only on an installed,
# byte-compiled build. Interpreted and compiled code differ in the last bit of
# a few results, and source that pkgload loaded (devtools::test(),
# testthat::test_local()) is compiled only while it runs, so the first run of
# such a session differs from the second. The same holds under covr, which
# replaces the installed functions with traced ones that are not compiled. A
# test calls this directly before it compares whole stage results; everything
# above the call runs either way.
skip_stage_identity_from_source <- function() {
  # pkgload is asked only when it is loaded, so the tests do not depend on it.
  # covr marks the session in which it runs the tests with R_COVR.
  from_source <- ("pkgload" %in% loadedNamespaces() && asNamespace("pkgload")$is_dev_package("scoRch.simulator")) ||
    identical(Sys.getenv("R_COVR"), "true")
  skip_if(from_source, paste("Comparing whole stage results bit for bit needs an installed, byte-compiled build of",
                             "scoRch.simulator, not one loaded from source or traced by covr."))
}
