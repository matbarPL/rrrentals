#' Returns variable pretty name
#'
#' @param target_var target variable, one of casual, registered or total
#'
#' @return
#' @export
#'
#' @examples
get_target_pretty_name <- function(target_var){
  if(!(target_var %in% c("registered", "cnt", "casual"))){
    stop("incorrect target variable")
  }
  else{
    if (target_var == "cnt"){
      target_var_pretty_name <- "Rentals total count"
    } else {
      target_var_pretty_name <- paste(c("Rentals", target_var, "count"), collapse = " ")
    }
    return(target_var_pretty_name)
  }
}
