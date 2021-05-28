#' Create complex exploratory data analysis report for
#' daily rentals.
#'
#' @param path_to_day_df path to dataframe with daily statistics
#'
#' @return
#' @export
#'
#' @import DataExplorer
#' @import DataExplorer
#' @import dplyr
#' @import outliers
#' @import readxl
#' @import ggplot2
#'
eda_day <- function(){
  # READ PREPROCESSED DATA --------------------------------------------------

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


  # EDA WEATHER ----------------------------------------------------------
  weather_df_eda <-
    weather_df %>% select(-instant)

  plot_str(weather_df_eda)
  introduce(weather_df_eda)
  plot_intro(weather_df_eda)
  plot_missing(weather_df_eda)
  plot_histogram(weather_df_eda)
  plot_correlation(weather_df_eda, type="c")

  # very strong correlation visible for atemp and temp which might suggest
  # that one of these variables is useless since we want to omit multicollinearity
  # in th modeling phase! Intuition suggest that feeling temperature should
  # be lower than the actual temperature when there is wind or high humidity
  # which we check below by performing analysis

  weather_df_eda %>%
    mutate(atemp_higher_than_temp = atemp>temp) %>%
    group_by(atemp_higher_than_temp) %>%
    count()

  #indeed all the values of atemp are higher than temp, let's investigate
  #by how any degrees, maybe there is a constant value

  weather_df_eda %>%
    mutate(atemp_temp_diff = atemp - temp) %>%
    pull(atemp_temp_diff) %>%
    summary()

  #there is non-zero standard deviation so it's not the constant value
  # but we decide to drop atemp variable

  weather_df_eda <- weather_df_eda %>%
    select(-atemp)

  plot_correlation(weather_df_eda, type="c")

  #the same scenario appears for windspeed and humidity

  weather_df_eda %>%
    mutate(hum_higher_than_windspeed = hum>windspeed) %>%
    group_by(hum_higher_than_windspeed) %>%
    count()

  # in this case there appears values where humidity is lower than windspeed

  weather_df_eda <- weather_df_eda %>%
    select(-hum)

  plot_correlation(weather_df_eda, type="c")


  # EDA DATETIME ------------------------------------------------------------

  dt_df_eda <-
    dt_df %>%
    select(-c(instant, dteday))

  plot_str(dt_df_eda)
  introduce(dt_df_eda)
  plot_intro(dt_df_eda)
  plot_missing(dt_df_eda)
  plot_correlation(dt_df_eda, type="d")

  # multicolinearity for weekdays, months and seasons suggest that
  # we should decide on dummy variables

  # EDA OUTCOME -------------------------------------------------------------
  y_df_eda <-
    y_df %>%
    select(-instant)

  plot_str(y_df_eda)
  introduce(y_df_eda)
  plot_intro(y_df_eda)
  plot_missing(y_df_eda)
  plot_histogram(y_df_eda)
  plot_correlation(y_df_eda, type="c")

  # EDA REGRESSORS -> OUTCOME --------------------------------------------------

  x_y_df_eda <-
    dt_df %>%
    inner_join(weather_df) %>%
    inner_join(y_df, by = "instant") %>%
    select(-c(atemp,hum,dteday))

  x_y_df_eda %>%
    inner_join(dt_df %>%
                 select(instant, dteday), by = "instant") %>%
    group_by(dteday) %>%
    count()

  # We spot issues with missing values here, there are missing values for
  # some days. We want to understand it.

  x_y_df_eda %>%
    inner_join(dt_df %>%
                 select(instant, dteday), by = "instant") %>%
    group_by(dteday) %>%
    count() %>%
    arrange(n)

  x_y_df_eda %>%
    inner_join(dt_df %>%
                 select(instant, dteday),
               by = "instant") %>%
    group_by(dteday) %>%
    count() %>%
    mutate(missing_hours = n < 24) %>%
    group_by(missing_hours) %>%
    count()

  # hypothesis- people rent more bikes during holidays --------

  # equality of variances for f-test ----------------------------------------
  holiday_rents <-
    x_y_df_eda %>%
    filter(holiday == 1) %>%
    pull(cnt)

  non_holiday_rents <-
    x_y_df_eda %>%
    filter(holiday == 0) %>%
    pull(cnt)

  var.test(x = holiday_rents, y = non_holiday_rents)

  # we reject the null hypothesis that variances for bike rents for holidays and
  # not holidays are equal. Variances are significantly different. We proceed
  # to f-test for two independent samples with unequal variances

  t.test(holiday_rents, non_holiday_rents, var.equal = FALSE)

  # outcome of t test suggest that means for holidays and non holidays differ
  # significantly. We are curious if holidays variable will be significant.


  # hypothesis- people rent more bikes during 2012 than during 2011 --------

  cnt_2011 <-
    x_y_df_eda %>%
    filter(yr == 0) %>%
    pull(cnt)

  cnt_2012 <-
    x_y_df_eda %>%
    filter(holiday == 1) %>%
    pull(cnt)

  var.test(x = cnt_2011, y = cnt_2012)

  # we reject the null hypothesis that variances in total counts for 2011
  # and 2012 are the same

  t.test(cnt_2011, cnt_2012, var.equal = FALSE)

  # we fail to reject the null hypothesis that mean of total rents for 2011
  # and 2012 are different. We are curious if year variable will be significant


  # hypothesis - people rent more bikes on working day ----------------------

  nonworkingday_cnt <-
    x_y_df_eda %>%
    filter(workingday == 0) %>%
    pull(cnt)

  workingday_cnt <-
    x_y_df_eda %>%
    filter(workingday == 1) %>%
    pull(cnt)

  var.test(x = nonworkingday_cnt, y = workingday_cnt)

  # we reject the null hypothesis that variances in total counts for 2011
  # and 2012 are the same

  t.test(x = workingday_cnt, y = nonworkingday_cnt, var.equal = FALSE, alternative = "greater")

  #indeed

  # GGPLOT ------------------------------------------------------------------

  ggplot(data = x_y_df_eda %>%
           group_by(season) %>%
           summarise(across(cnt, list(mean = ~mean(., na.rm = TRUE)))),
         aes(x = season, y = cnt_mean)) +
    geom_bar(stat="identity") +
    ylab("Mean count of rentals") +
    ggtitle("We concude that average number of bikes' rentals differ significantly with respect to season") +
    theme(plot.title = element_text(hjust = 0.5))

  ggplot(data = x_y_df_eda %>%
           group_by(holiday) %>%
           summarise(across(cnt, list(mean = ~mean(., na.rm = TRUE)))),
         aes(x = holiday, y = cnt_mean)) +
    geom_bar(stat="identity") +
    ylab("Mean count of rentals in regards to holiday") +
    ggtitle("We concude that average number of bikes' rentals differ significantly with respect to holiday") +
    theme(plot.title = element_text(hjust = 0.5))

  ggplot(data = x_y_df_eda %>%
           group_by(holiday) %>%
           summarise(across(cnt, list(mean = ~mean(., na.rm = TRUE)))),
         aes(x = holiday, y = cnt_mean)) +
    geom_bar(stat="identity") +
    ylab("Mean count of rentals in regards to holiday") +
    ggtitle("We concude that average number of bikes' rentals differ significantly with respect to holiday") +
    theme(plot.title = element_text(hjust = 0.5))

  x_y_df_eda %>%
    group_by(yr,mnth,weekday,workingday) %>%
    count()

  x_y_df_eda %>%
    filter(yr == 0, mnth == 1, weekday == 0)

}
