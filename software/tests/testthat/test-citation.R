test_that("installed R citations use the bundled CFF software and study metadata", {
  cff <- yaml::read_yaml(system.file("extdata", "scorch.cff", package = "scoRch.simulator", mustWork = TRUE))
  entries <- utils::citation("scoRch.simulator")
  expect_length(entries, 2L)
  expect_identical(entries[[1]]$title, cff$title)
  expect_identical(entries[[1]]$note, paste("R package version", cff$version))
  expect_identical(entries[[1]]$url, cff$url)
  expect_identical(entries[[1]]$doi, cff$doi)
  expect_match(entries[[1]]$year, "^[12][0-9]{3}$")
  expect_identical(unlist(entries[[1]]$author$family, use.names = FALSE),
                   vapply(cff$authors, `[[`, "", "family-names"))
  study <- cff$references[[1]]
  expect_identical(entries[[2]]$title, study$title)
  expect_identical(entries[[2]]$year, as.character(study$year))
  expect_identical(entries[[2]]$url, study$url)
  expect_identical(entries[[2]]$note, sub("[.]$", "", study$notes))
})

test_that("citation fields follow CFF changes and use release dates before build dates", {
  cff <- yaml::read_yaml(system.file("extdata", "scorch.cff", package = "scoRch.simulator", mustWork = TRUE))
  cff$title <- "Updated software title"
  cff$version <- "9.8.7"
  cff[["date-released"]] <- "2030-05-02"
  cff$references[[1]]$title <- "Updated study title"
  entries <- scoRch.simulator:::scorch_citation(list(Built = "R 4.5.1; ; 2024-01-01 UTC"), cff)
  expect_identical(entries[[1]]$title, "Updated software title")
  expect_identical(entries[[1]]$note, "R package version 9.8.7")
  expect_identical(entries[[1]]$year, "2030")
  expect_identical(entries[[2]]$title, "Updated study title")
  cff[["date-released"]] <- NULL
  entries <- scoRch.simulator:::scorch_citation(list(Built = "R 4.5.1; ; 2024-01-01 UTC"), cff)
  expect_identical(entries[[1]]$year, "2024")
})
