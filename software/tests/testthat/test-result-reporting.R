test_that("exported effects have explicit units and preserve original parameters", {
  parameters <- data.frame(parameter = c("(Intercept)", "lon", "lat", "mean_tmax", "std_tmax", "var_sigma2", "scale_km"),
                           value = c(-10, log(2), 0, log(1.1), log(0.9), 1.5, 200))
  exported <- scoRch.simulator:::lgcp_parameter_export(parameters)
  expect_identical(exported[c("parameter", "value")], parameters)
  expect_equal(exported$intensity_change_pct_per_1C, c(NA, NA, NA, 10, -10, NA, NA))
  expect_equal(exported$intensity_change_pct_per_1_degree, c(NA, 100, 0, NA, NA, NA, NA))
  path <- tempfile(fileext = ".xlsx")
  scoRch.simulator:::write_workbook(path, list(Parameters = exported))
  back <- openxlsx2::read_xlsx(path)
  rownames(back) <- NULL
  expect_equal(back, exported, tolerance = 1e-14)
  effects <- scoRch.simulator:::lgcp_effect_lines(parameters)
  expect_true(any(grepl("+1 C in mean daily Tmax: +10.000%", effects, fixed = TRUE)))
  expect_true(any(grepl("+1 degree in longitude: +100.000%", effects, fixed = TRUE)))
})

test_that("power-law reporting distinguishes a calculation from a valid goodness-of-fit test", {
  fit <- scoRch.simulator:::pl_fit(c(1, 2))
  expect_identical(fit$status, "ok")
  expect_equal(fit$n_tail, 2)  # The reference tries cutoffs without a five-observation minimum.
  areas <- c(rep(1, 8), 10, 20)  # A tied body can produce an undefined resampled fit.
  fit <- scoRch.simulator:::pl_fit(areas)
  boot <- scoRch.simulator:::pl_bootstrap(areas, fit, reps = 50, seed = 1)
  expect_gt(boot$n_undefined, 0)
  partial <- scoRch.simulator:::power_law_summary("daily_maxima", list(fit = fit, bootstrap = boot, seed = 1))
  unavailable <- scoRch.simulator:::power_law_summary("event_maxima",
    list(fit = scoRch.simulator:::pl_fit(rep(1, 4)), bootstrap = NULL, seed = 2))
  summary <- rbind(partial, unavailable)
  books <- scoRch.simulator:::power_law_export(summary)
  expect_identical(names(books), c("Power Law Fits", "Bootstrap Results"))
  expect_false("status" %in% names(books[[1]]))
  expect_true(all(is.na(books[[1]]$goodness_of_fit_p_value)))
  expect_equal(books[[2]]$undefined_bootstrap_fits[1], boot$n_undefined)
  expect_equal(books[[2]]$p_value_upper_bound[1] - books[[2]]$p_value_lower_bound[1], boot$n_undefined / 50)
  expect_true(all(vapply(books[[1]][-1], is.numeric, logical(1))))
  expect_true(all(vapply(books[[2]][-1], is.numeric, logical(1))))
  result <- list(power_law = list(summary = summary), lgcp = NULL, lgcp_error = "test input")
  lines <- scoRch.simulator:::readme_models(result)
  expect_true(any(grepl("daily_maxima: fit calculation completed", lines, fixed = TRUE)))
  expect_true(any(grepl("Goodness-of-fit test unavailable", lines, fixed = TRUE)))
  expect_true(any(grepl("event_maxima: fit unavailable", lines, fixed = TRUE)))
  expect_true(any(grepl("bootstrap fits undefined", scoRch.simulator:::readme_warnings(result), fixed = TRUE)))
  expect_identical(scoRch.simulator:::run_status(c(result, list(events = list()))), "COMPLETE WITH WARNINGS")

  complete <- partial
  complete$p_value <- 0.1
  complete$n_undefined <- 0
  result$power_law$summary <- complete
  expect_true(any(grepl("Power law rejected at the 10%", scoRch.simulator:::readme_models(result), fixed = TRUE)))
  result$power_law$summary$p_value <- 0.1001
  expect_true(any(grepl("Power law not rejected at the 10%", scoRch.simulator:::readme_models(result), fixed = TRUE)))
})

test_that("duration exports retain gaps in active years and do not invent zero durations", {
  events <- data.frame(event_type = c(3L, 3L, 3L, 3L, 4L), duration_days = c(3, 4, 6, 6, 3))
  year <- c(2000L, 2005L, 2010L, 2015L, 2014L)
  annual <- scoRch.simulator:::annual_duration(events, year)
  trends <- scoRch.simulator:::duration_trends(events, year)
  exported <- scoRch.simulator:::duration_trend_export(trends)
  expect_identical(annual$year[annual$event_type == 3], c(2000L, 2005L, 2010L, 2015L))
  expect_identical(exported$years_with_events, c(4L, 1L))
  expect_equal(exported$ols_slope_days_per_decade, c(2.2, NA))
  expect_equal(exported$sen_slope_days_per_decade, c(2, NA))
  expect_false("mann_kendall_result" %in% names(exported))
  expect_identical(exported$mann_kendall_p_value, trends$mann_kendall_p_value)
  path <- tempfile(fileext = ".xlsx")
  scoRch.simulator:::write_workbook(path, list("Duration Trends" = exported, "Annual Mean Duration" = annual))
  back <- openxlsx2::read_xlsx(path, sheet = "Duration Trends")
  expect_true(is.na(back$ols_slope_days_per_decade[2]))
  lines <- scoRch.simulator:::readme_duration_trends(list(events = list(trends = trends)))
  expect_true(any(grepl("Type 4: trend unavailable; 1 year with events", lines, fixed = TRUE)))
})
