#' Title
#'
#' @param path_to_x_y_df_eda
#'
#' @return
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import highcharter
#' @import RColorBrewer
#'
#' @export
#'
correlation_plot <- function(target_var) {
  target_var_pretty_name <- get_target_pretty_name(target_var)

  cor_df <- day_df %>%
    select(temp_denormalized, atemp_denormalized, windspeed_denormalized, hum_denormalized, target_var) %>%
    rename("Temperature" = temp_denormalized,
           "Feeling temperature" = atemp_denormalized,
           "Windspeed" = windspeed_denormalized,
           "Humidity" = hum_denormalized,
           !!target_var_pretty_name := target_var)

  cors <- cor(cor_df) %>%
    as.data.frame() %>% # convert to dataframe
    rownames_to_column(var="col1") %>% # add rownames as a new column
    pivot_longer(cols=names(cor_df),
                 names_to="col2",
                 values_to='cor')

  hchart(cors,"heatmap",hcaes(x=col1,y=col2,value=round(cor,2)),
         dataLabels = list(enabled = TRUE)) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "")) %>%
    hc_colorAxis(minColor = brewer.pal(9, "Blues")[1],
                 maxColor = brewer.pal(9, "Blues")[9])
}

