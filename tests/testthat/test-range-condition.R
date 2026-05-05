test_that("range condition works for a single data frame", {
  ex <- r4c_example_data()
  res <- classify_range_condition(
    ex$sf_single,
    palatability = ex$palatability,
    indicative_production = 60
  )

  expect_s3_class(res, "r4c_condition_result")
  expect_true(res$total_score > 0)
  expect_true(res$condition_class %in% c("Unusable", "Very Poor", "Poor", "Fair", "Good", "Excellent"))
  expect_equal(nrow(res$factor_scores), 6)
})

test_that("range condition works for replicated inputs", {
  ex <- r4c_example_data()
  res <- classify_range_condition(
    ex$sf_replicates,
    palatability = ex$palatability,
    indicative_production = 60
  )

  expect_s3_class(res, "r4c_condition_result")
  expect_equal(nrow(res$merged_data), 7)
})

test_that("year-wise classification returns one row per year", {
  ex <- r4c_example_data()
  res <- classify_range_condition_years(
    ex$sf_years,
    palatability = ex$palatability,
    indicative_production = 60
  )

  expect_s3_class(res, "r4c_condition_series")
  expect_equal(nrow(res$summary), 2)
})
