#' calculate formula based on given parameters
#'
#' small function that calculates a formula after replacing the parameter names with their values
#'
#' @param formula formula including only given parameters
#' @param submetric_name vector with names of all parameters mentioned in formula (only calculated values)
#' @param submetric_value vector with values of all parameters of metric name (in same order as submetric_name)
#' @param submetric_score_name vector with names of all parameters mentioned in formula (only scores of calculated values)
#' @param submetric_score vector with values of all parameters of metric score name (in same order as submetric_score_name)
#' @param surface value of variable surface (specific for location)
#'
#' @return a value which is the result of calculating the formula
#'
#' @importFrom dplyr filter
#' @importFrom pander evals
#'
#' @export
#'
#'
calculate_formula <-
  function(
    formula, submetric_name, submetric_value, submetric_score_name,
    submetric_score, surface
  ) {

  parameters <-
    data.frame(
      name = c(submetric_name, submetric_score_name, "surface"),
      value = c(submetric_value, submetric_score, surface),
      stringsAsFactors = FALSE
    ) %>%
    filter(!is.na(.data$name))

  for (i in seq_len(nrow(parameters))) {
    formula <-
      gsub(
        pattern = parameters$name[i], replacement = parameters$value[i],
        x = formula
      )
  }
  result <- evals(formula)[[1]]$result
  return(result)
}
