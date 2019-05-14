#' calculate formula based on given parameters
#'
#' small function that calculates a formula after replacing the parameter names with their values
#'
#' @param formula formula including only given parameters
#' @param metric_name vector with names of all parameters mentioned in formula
#' @param metric_value vector with values of all parameters of metric name (in same order as metric_name)
#'
#' @return a value which is the result of calculating the formula
#'
#' @importFrom pander evals
#'
#' @export
#'
#'
calculate_formula <-
  function(
    formula, metric_name, metric_value, metric_score_name, metric_score
  ) {
  for (i in seq_len(length(metric_name))) {
    formula <-
      gsub(
        pattern = metric_score_name[i], replacement = metric_score[i],
        x = formula
      )
    formula <-
      gsub(
        pattern = metric_name[i], replacement = metric_value[i], x = formula
      )
    result <- evals(formula)[[1]]$result
  }
  return(result)
}
