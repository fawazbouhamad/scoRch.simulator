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

# One window can be several sessions: a reload ends one and begins the next,
# and a second tab is another. The app must outlive all of that and still end
# once the last window has gone.
window_sessions <- function(grace, patience) {
  tally <- new.env()
  tally$quits <- 0L
  seen <- new.env()
  app <- scoRch.simulator:::scorch_app("s3cret", new.env(), quit = function() tally$quits <- tally$quits + 1L,
                                       grace = grace, seen = seen, patience = patience)
  list(tally = tally, seen = seen,
       # The browser asks for the page with its key, as a reload does.
       page = scoRch.simulator:::scorch_page("the window", "s3cret", seen),
       # A window opens: the server of the app runs for a new session, which is returned.
       join = function() {
         session <- shiny::MockShinySession$new()
         session$clientData <- list(url_search = "?token=s3cret")
         server <- app$serverFuncSource()
         shiny::isolate(shiny::withReactiveDomain(session, server(session$input, session$output, session)))
         session
       },
       wait = function(seconds) {
         Sys.sleep(seconds)
         later::run_now()
       })
}

test_that("a departure ends the app only when nothing happened after it, and a reload is waited for", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  w <- window_sessions(grace = 0.3, patience = 30)
  first <- w$join()
  second <- w$join()
  expect_false(first$isClosed())
  # The second tab is closed: its wait begins.
  second$close()
  Sys.sleep(0.05)
  # The first tab is reloaded: the browser asks for the page, with its key, ...
  expect_identical(w$page(list(QUERY_STRING = "?token=s3cret")), "the window")
  expect_true(w$seen$page > w$seen$joined)
  # ... and its old session ends. Nobody is connected while the wait of the
  # second tab runs out, yet that tab did not leave last, and the page that was
  # handed out has still to open its window: the app stays, however long that takes.
  first$close()
  w$wait(0.6)
  expect_identical(w$tally$quits, 0L)
  w$wait(0.6)
  expect_identical(w$tally$quits, 0L)
  reloaded <- w$join()
  w$wait(0.6)
  expect_identical(w$tally$quits, 0L)
  # The reloaded window goes as well, and nothing was asked for since: the app ends, once.
  reloaded$close()
  w$wait(0.6)
  expect_identical(w$tally$quits, 1L)
  w$wait(0.6)
  expect_identical(w$tally$quits, 1L)
})

test_that("a second tab that is closed leaves the first one open, and the last one ends the app", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  w <- window_sessions(grace = 0.2, patience = 30)
  first <- w$join()
  second <- w$join()
  second$close()
  w$wait(0.5)
  expect_identical(w$tally$quits, 0L)
  first$close()
  w$wait(0.5)
  expect_identical(w$tally$quits, 1L)
})

test_that("a page that was handed out but never opened its window does not keep the app for ever", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  w <- window_sessions(grace = 0.1, patience = 1)
  only <- w$join()
  Sys.sleep(0.05)
  w$page(list(QUERY_STRING = "?token=s3cret"))
  only$close()
  for (i in 1:12) w$wait(0.25)
  expect_identical(w$tally$quits, 1L)
})

test_that("a window that opens for an earlier request does not hide a page that is still loading", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  w <- window_sessions(grace = 0.2, patience = 30)
  key <- list(QUERY_STRING = "?token=s3cret")
  old <- w$join()
  Sys.sleep(0.05)
  # Reload: the page is asked for and the old window goes.
  w$page(key)
  old$close()
  Sys.sleep(0.05)
  # Reload again: the page is asked for once more, while the document of the first reload still connects ...
  w$page(key)
  Sys.sleep(0.05)
  first <- w$join()
  # ... and is replaced at once. The page of the second reload has not opened its window yet.
  first$close()
  w$wait(0.5)
  expect_identical(w$tally$quits, 0L)
  w$wait(0.5)
  expect_identical(w$tally$quits, 0L)
  second <- w$join()
  w$wait(0.5)
  expect_identical(w$tally$quits, 0L)
  second$close()
  w$wait(0.5)
  expect_identical(w$tally$quits, 1L)
})

test_that("a page that opened no window long ago does not delay the end of the app", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  w <- window_sessions(grace = 0.1, patience = 1)
  only <- w$join()
  Sys.sleep(0.05)
  w$page(list(QUERY_STRING = "?token=s3cret"))
  # Longer than `patience` later the window closes: nothing can still be loading.
  Sys.sleep(1.3)
  only$close()
  w$wait(0.4)
  expect_identical(w$tally$quits, 1L)
})

test_that("a window that opens accounts for its own page, not for one that expired long ago", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  w <- window_sessions(grace = 0.1, patience = 1)
  key <- list(QUERY_STRING = "?token=s3cret")
  only <- w$join()
  Sys.sleep(0.05)
  # The page source is looked at: a hand-out that opens no window. It expires.
  w$page(key)
  Sys.sleep(1.3)
  # Reload: the page is asked for, the old window goes, the new one opens, and is closed at once.
  w$page(key)
  only$close()
  again <- w$join()
  again$close()
  w$wait(0.4)
  expect_identical(w$tally$quits, 1L)
})
