#' Reads the day dataframe and performs all necessary steps in  particular
#' splitting dataframe into separate dataframes
#' (datetime, weather, normalized regressors and outcome)
#'
#' @param path_to_day_df either path relative to server.R file
#' or complete path to day dataframe
#'
#' @return list
#' @export
#'
#' @import dplyr
#' @import outliers
#' @import readxl
#'
preprocess_day_df <- function(){
  day_df_cleaned <- day_df

  # SPLITTING DATASET -------------------------------------------------------

  # datetime ----------------------------------------------------------------

  dt_df <-
    day_df %>%
    select(instant, date, season, yr, mnth, holiday, weekday, workingday) %>%
    mutate(across(c(season, yr, mnth, holiday, weekday, workingday),
                  as.factor))

  day_df_cleaned <-
    day_df %>%
    select(-c(date, season, yr, mnth, holiday, weekday, workingday))

  # weather -----------------------------------------------------------------

  weather_df <- day_df_cleaned %>%
    select(instant, weathersit, temp_denormalized, atemp_denormalized, hum_denormalized, windspeed_denormalized) %>%
    rename(temp = temp_denormalized,
           atemp = atemp_denormalized,
           hum = hum_denormalized,
           windspeed = windspeed_denormalized) %>%
    mutate(weathersit = as.factor(weathersit))

  day_df_cleaned <- day_df_cleaned %>%
    select(-c(temp_denormalized, atemp_denormalized, hum_denormalized, windspeed_denormalized))


  # normalized regressors columns ------------------------------------------------------

  normalized_weather_df <- day_df_cleaned %>%
    select(instant, weathersit, temp, atemp, hum, windspeed) %>%
    rename_with(~paste0(., "_normalized"),
                c(weathersit, temp, atemp, hum, windspeed))


  day_df_cleaned <- day_df_cleaned %>%
    select(-c(weathersit, temp, atemp, hum, windspeed))


  # outcome -----------------------------------------------------------------

  y_df <- day_df_cleaned


  # SANITY CHECKS -----------------------------------------------------------
  # according to the description casual plus registered should equal to count

  y_df %>%
    mutate(sanity_check = casual + registered == cnt) %>%
    group_by(sanity_check) %>%
    count()

  # HANDLING OUTLIERS -------------------------------------------------------


  out_ind_df <- y_df

  out_ind_df <-
    out_ind_df %>%
    mutate(across(c(casual,registered,cnt),
                  ~scores(., type="iqr"),
                  .names="{col}_iqr_score"))

  out_ind_df %>%
    mutate(across(c(casual_iqr_score,registered_iqr_score,cnt_iqr_score),
                  ~.!=0, #for the values between IQR values values are always equal to zero
                  .names="{col}_is_outlier")) %>%
    group_by(casual_iqr_score_is_outlier,registered_iqr_score_is_outlier,cnt_iqr_score_is_outlier) %>%
    count() %>%
    filter(cnt_iqr_score_is_outlier == FALSE)

  y_df_outliers_marked <-
    out_ind_df %>%
    mutate(across(c(casual_iqr_score,registered_iqr_score,cnt_iqr_score),
                  ~.!=0, #for the values between IQR values values are always equal to zero
                  .names="{col}_is_outlier"))

  y_df_cnt_outliers_filtered <-
    y_df_outliers_marked %>%
    filter(cnt_iqr_score_is_outlier == FALSE) %>%
    select(instant, cnt)

  y_df_registered_outliers_filtered <-
    y_df_outliers_marked %>%
    filter(registered_iqr_score_is_outlier == FALSE) %>%
    select(instant, cnt)

  y_df_casual_outliers_filtered <-
    y_df_outliers_marked %>%
    filter(casual_iqr_score_is_outlier == FALSE) %>%
    select(instant, cnt)

return(list(
  dt_df = dt_df,
  normalized_weather_df = normalized_weather_df,
  out_ind_df = out_ind_df,
  weather_df = weather_df,
  y_df = y_df,
  y_df_casual_outliers_filtered = y_df_casual_outliers_filtered,
  y_df_cnt_outliers_filtered = y_df_cnt_outliers_filtered,
  y_df_outliers_marked = y_df_outliers_marked,
  y_df_registered_outliers_filtered = y_df_registered_outliers_filtered
))

}
