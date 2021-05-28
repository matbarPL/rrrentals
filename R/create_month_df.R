#' Create aggregated month dataframe based on daily features
#'
#' @param path_to_day_df either path relative to server.R file
#' or complete path to day dataframe
#' @param write_month_df_to_file logical whether to save final dataframe
#'to Input_files_01 folder
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import outliers
#' @import tidyr
#' @import readxl
#' @import recipes
#' @import xlsx
#' @export
#'
create_month_df <- function(path_to_month_df = ""){
  day_df_cleaned <- day_df
  day_df_preprocessed <- preprocess_day_df()

  dt_df <- day_df_preprocessed$dt_df
  normalized_weather_df <-day_df_preprocessed$normalized_weather_df
  out_ind_df <- day_df_preprocessed$out_ind_df
  weather_df <- day_df_preprocessed$weather_df
  y_df <- day_df_preprocessed$y_df
  y_df_casual_outliers_filtered <-  day_df_preprocessed$y_df_casual_outliers_filtered
  y_df_cnt_outliers_filtered <- day_df_preprocessed$y_df_cnt_outliers_filtered
  y_df_outliers_marked <- day_df_preprocessed$y_df_outliers_marked
  y_df_registered_outliers_filtered <- day_df_preprocessed$y_df_registered_outliers_filtered

  # AGGREGATING DATASET BY MONTH --------------------------------------------
  dt_weather_df <-
    dt_df %>%
    inner_join(weather_df)

  mnth_df <-
    dt_weather_df %>%
    mutate(year = if_else(yr == 0, 2011, 2012)) %>%
    mutate(date = make_date(year = year, month = mnth)) %>%
    recipe(~ weathersit + workingday + holiday) %>%
    step_dummy(weathersit, one_hot = TRUE) %>%
    step_dummy(workingday, one_hot = TRUE) %>%
    step_dummy(holiday, one_hot = TRUE) %>%
    prep(training = dt_weather_df, retain = TRUE) %>%
    bake(new_data = dt_weather_df) %>%
    mutate(instant = 1:nrow(.)) %>%
    inner_join(dt_weather_df %>% select(-holiday, -weathersit)) %>%
    inner_join(y_df) %>%
    mutate(year = if_else(yr == 0, 2011, 2012)) %>%
    mutate(date = make_date(month = mnth, year = year)) %>%
    group_by(date) %>%
    summarise(across(c(temp, atemp, windspeed, hum, casual, registered, cnt), list(mean = ~mean(.))),
              across(c(weathersit_X1, weathersit_X2, weathersit_X3), list(count = ~sum(.))),
              across(c(workingday_X0, workingday_X1), list(count = ~sum(.))),
              across(c(holiday_X0, holiday_X1), list(count = ~sum(.))))

  season_change_df <-
    dt_df %>%
    mutate(season = as.numeric(season)) %>%
    mutate(season_diff = abs(season - lag(season, default = 0))) %>%
    relocate(instant, dteday, season_diff) %>%
    filter(season_diff %in% c(1,3)) %>%
    mutate(mnth = as.numeric(mnth)) %>%
    select(dteday, season_diff, season, mnth)

  mnth_df <-
    mnth_df %>%
    mutate(month = month(date)) %>%
    left_join(season_change_df %>% select(mnth, season), by = c("month" = "mnth")) %>%
    fill(season, .direction = "down") %>%
    unique() %>%
    mutate(yr = if_else( year(date) == 2011, 0, 1)) %>%
    relocate(date, season, yr, month, starts_with("holiday"), starts_with("weekday"),
             starts_with("workingday"), starts_with("weathersit"))

  if(path_to_month_df != ""){
    mnth_df %>%
      write.xlsx(path_to_month_df)
  }
  return (mnth_df)
}
