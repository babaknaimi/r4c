test_that("SF template helpers classify the example workbook by year", {
  path <- system.file("extdata", "r4c_data.xlsx", package = "r4c", mustWork = TRUE)

  res <- classify_sf_template_years(
    path,
    sheets = c("sfd1", "sfd2"),
    year_names = c("Year 1", "Year 2"),
    score_profile = "template_rank"
  )

  expect_s3_class(res, "r4c_condition_series")
  expect_equal(res$summary$total_score, c(53, 56))
  expect_equal(res$summary$condition_class, c("Fair", "Fair"))
})

test_that("production helper reproduces example workbook checks", {
  path <- system.file("extdata", "r4c_data.xlsx", package = "r4c", mustWork = TRUE)

  prod <- estimate_fodder_production(path)

  expect_s3_class(prod, "r4c_production_result")
  expect_equal(prod$summary$method, c("CW", "CY", "DS"))
  expect_equal(round(prod$summary$dry_fodder_kg_ha, 1), c(779.6, 772.5, 811.1))
  expect_equal(round(prod$summary$livestock_units), c(1516, 1502, 1577))
})
