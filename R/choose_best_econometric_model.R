#' With the usage of the specific dataset and given target variable by the user
#' regarding the explained variable, a function automatically selects the best
#' form of the model based on significance values and forward selection
#'
#' @param modeling_dataframe table chosen for modeling
#' @param general_formula a general modeling formula
#' @param target_var target variable to model
#'
#' @return
#' @export
#' @import dplyr
#' @import readxl
#'
choose_best_econometric_model <- function(modeling_dataframe,
                                          general_formula,
                                          target_var){
  if (typeof(modeling_dataframe) != "list"){
    stop("incorrect modeling dataframe")
  }
  modeling_dataframe_names <- modeling_dataframe %>% names()
  if (!(target_var %in% modeling_dataframe_names)){
    stop("target variable not in modeling dataframe")
  }
  if (is.error(lm(general_formula, data=modeling_dataframe))){
    stop("formula not specified correctly")
  }
  general_model <- lm(general_formula, data=modeling_dataframe)
  general_summary <- summary(general_model)
  predictive_vars <- rownames(general_summary$coefficients)
  coef_table <- as_tibble(general_summary$coefficients[,1:4]) %>%
    mutate(variable = predictive_vars) %>%
    rename(p_value=`Pr(>|t|)`)

  max_pvalue <- coef_table %>%
    pull(p_value) %>%
    max()
  col_names <- coef_table %>%
    pull(variable)
  col_names <- col_names[2:length(col_names)]

  if(max_pvalue<0.05){
    return(list(specific_model = specific_model,
                col_names = col_names))
  } else{
    while(max_pvalue>0.05){
      for(col_name in col_names[2:length(col_names)]){
        col_name_pvalue <- coef_table %>%
          filter(variable == col_name) %>%
          pull(p_value)
        if(col_name_pvalue > 0.05){
          col_names <- col_names[!col_names %in% col_name]
          specific_formula <- as.formula(paste(target_var,
                                               paste(col_names,
                                                     collapse=" + "),
                                               sep=" ~ "))
          specific_model <- lm(specific_formula,
                               data=modeling_dataframe)
          specific_summary <- summary(specific_model)
          coef_table <-
            as_tibble(specific_summary$coefficients[,1:4]) %>%
            mutate(variable = c('(Intercept)', col_names)) %>%
            rename(p_value=`Pr(>|t|)`)
          max_pvalue = max(specific_summary$coefficients[2:(specific_summary$df[1]),4])
        }
      }
    }
    return(list(specific_model = specific_model,
                col_names = col_names))
  }
}
