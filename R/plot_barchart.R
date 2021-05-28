#' Function yield a barchart fo chosen rental variable
#'
#' @param group_var either yr or holiday, produces barplot for this group
#' where values represent rental counts per day
#' @param target_var variable to display on y axis in barcharts
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import highcharter
#'
plot_barchart <- function(group_var, target_var){
  day_df_copy <- day_df
  if(group_var == "yr"){
    categories <- c("2011", "2012")
    var_val <- day_df$yr
    day_df_copy %<>%
      rename(group_var = yr,
             target_var = !!target_var)
  } else if (group_var == "holiday") {
    categories <- c("Non-holiday", "Holiday")
    var_val <- day_df$holiday
    day_df_copy %<>%
      rename(group_var = holiday,
             target_var = !!target_var)
  } else{
    stop("Incorrect grouping variable")
  }
  if (target_var == "cnt"){
    target_var_pretty_name <- "Rentals total count"
  } else {
    target_var_pretty_name <- paste(c("Rentals", target_var, "count"), collapse = " ")
  }
  hcboxplot(
    x = day_df_copy %>% pull(target_var),
    var = var_val,
    name = "Length",
    color = "#2980b9",
    outliers = TRUE
  ) %>%
    hc_chart(type = "column") %>%
    hc_yAxis(title = list(text = target_var_pretty_name)) %>%
    hc_xAxis(categories = categories) %>%
    hc_add_series(
      data = day_df_copy,
      type = "scatter",
      hcaes(x = "group_var", y = target_var, group = "group_var")
    ) %>%
    hc_plotOptions(scatter = list(
      color = "darkblue",
      marker = list(
        radius = 2,
        symbol = "circle",
        lineWidth = 1
      )
    ))  %>%
    hc_plotOptions(scatter = list(jitter = list(x = .1, y = 0)))
}
