test_that("multiplication works", {
  predict_rentals("month")
  predict_rentals("day")
  testthat::expect_error(predict_rentals("hourly"))
})
