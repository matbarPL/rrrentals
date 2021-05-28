#' Complete function to load a forecasting model and yield the plot of
#' forecast vs actual values. We use bike rentals data for two years. We
#' have built the forecast prediction for the next quarter as well as
#' for the next 30 days
#'
#' @param granularity_level either monthly or daily
#'
#' @return pretty plot of bike rentals prediction
#' @export
#'
#' @import prophet
#' @import magrittr
#' @import readxl
#' @import lubridate
#' @import dplyr
#' @import highcharter
#'
predict_rentals <- function(granularity_level, target_var){
  target_var_pretty_name <- get_target_pretty_name(target_var)

  if(granularity_level %in% c("day", "month")){
    model <- get(paste(c("fitted_prophet_model", target_var, granularity_level), collapse = "_"))
    test_df <- get(paste(c("test", target_var, granularity_level, "df"), collapse = "_"))
    forecast <- predict(model, test_df)
    train_df <- get(paste(c(granularity_level, "df"), collapse = "_"))
    cutoff <- get(paste(c("cutoff", granularity_level), collapse = "_"))
  }
  else{
    stop("incorrect argument for granularity level")
  }
  highchart_forecast <- forecast %>%
    select(ds, yhat_lower, yhat_upper, yhat) %>%
    inner_join(train_df %>%
                 select(date, target_var) %>%
                 rename(ds = date,
                        y = {{target_var}}))
  highchart_forecast %<>%
    rename(low = yhat_lower,
           high = yhat_upper,
           date = ds) %>%
    mutate(date = datetime_to_timestamp(date))
  forecast_to_highcharts<- list_parse2(highchart_forecast)
  highchart() %>%
    hc_add_series(name = 'Confidence boundaries', data = forecast_to_highcharts, type = "arearange", color = "#0072B2", fillOpacity = 0.1) %>%
    hc_add_series(name = 'Fitted values',data = highchart_forecast, type = "spline", color = "#0072B2", hcaes(x = date, y = yhat)) %>%
    hc_add_series(name = 'Actual values', data = highchart_forecast, type = "scatter", marker=list(symbol='circle', radius=3),
                  color = "black", size = 0.5, hcaes(x = date, y = y)) %>%
    hc_xAxis(type = "datetime",
             title = list(text = "ds"),
             plotBands = list(
               color = "rgb(255, 0, 0)",
               from = cutoff,
               to = cutoff)
             ) %>%
    hc_yAxis(title = list(text = "y"))
}
