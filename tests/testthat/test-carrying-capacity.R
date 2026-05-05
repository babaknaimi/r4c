test_that("carrying capacity returns CY and DS summaries", {
  ex <- r4c_example_data()
  cc <- carrying_capacity(ex$cyd, ex$dsd)

  expect_s3_class(cc, "r4c_capacity_result")
  expect_equal(cc$summary$method, c("CY", "DS"))
  expect_true(all(cc$summary$DM_kg_ha > 0))
})

test_that("validation works with carrying-capacity output", {
  ex <- r4c_example_data()
  cc <- carrying_capacity(ex$cyd, ex$dsd)
  val <- validate_methods(ex$cwd, cc, test = "kruskal")

  expect_s3_class(val, "r4c_validation_result")
  expect_equal(levels(val$comparison_data$Method), c("CW", "CY", "DS"))
})
