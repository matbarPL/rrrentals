test_that("Simple checks for function that generate the best econometric model for day and month dataframe", {
  # Day modeling ------------------------------------------------------------
  general_variables <- day_df %>%
    dplyr::select(-instant, -date, -temp_denormalized, -atemp_denormalized,
                  -hum_denormalized, -windspeed_denormalized, -cnt, -casual, -registered) %>%
    names()
  general_formula <- as.formula(paste("cnt",
                                      paste(general_variables,
                                            collapse=" + "),
                                      sep=" ~ "))
  best_model <- choose_best_econometric_model(modeling_dataframe = day_df,
                                              general_formula = general_formula,
                                              target_var = "cnt")
  # Month modeling ----------------------------------------------------------

  general_variables <- month_df %>%
    dplyr::select(-cnt, -casual, -registered, -...1) %>%
    names()
  general_formula <- as.formula(paste("cnt",
                                      paste(general_variables,
                                            collapse=" + "),
                                      sep=" ~ "))
  best_model <- choose_best_econometric_model(modeling_dataframe = month_df,
                                              general_formula = general_formula,
                                              target_var = "cnt")
})
