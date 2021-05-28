test_that("Based on statistical test we create barchar per year and holiday. We expect this function to plot highcharter.", {
  plot_barchart("yr")
  plot_barchart("holiday")
  testthat::expect_error(plot_barchart("cnt"))
})
