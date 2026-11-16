# An installation made after this session loaded the package leaves old code
# in memory beside new files on disk. The window must say so, not open broken.
test_that("a session that loaded an earlier build is told to restart R", {
  built <- scoRch.simulator:::session_built
  stamp <- built$stamp
  skip_if_not(is.character(stamp) && length(stamp) == 1L && !is.na(stamp),
              "needs an installed build; a package loaded from source has no build stamp")
  withr::defer(built$stamp <- stamp)

  expect_silent(scoRch.simulator:::stale_session())

  built$stamp <- paste(stamp, "(an earlier build)")
  expect_error(scoRch.simulator:::stale_session(), "Restart R")

  # The window refuses to open in that state. Should the check ever be lost,
  # the window that then opens shows nowhere and is closed again, so this
  # fails instead of waiting.
  withr::local_options(shinygadgets.showdialog = function(...) NULL)
  cancel <- later::later(function() shiny::stopApp(), delay = 5)
  withr::defer(cancel())
  expect_error(suppressMessages(scoRch.simulator:::scorch_launcher()), "Restart R")

  built$stamp <- stamp
  expect_silent(scoRch.simulator:::stale_session())
})
