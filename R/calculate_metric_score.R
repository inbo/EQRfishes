#' calculate the metric scores of the EQR
#'
#' Calculates the metric score, given a calculated metric value (and some parameters specific to the sampling location), and a table with the indices and their tresholds (info from calculate_metric_score.csv).
#'
#' @param metric_score_name name of metric score to be calculated (NA if no calculation has to be done)
#' @param indices dataframe with indices and their tresholds (info from calculate_metric_score.csv)
#' @param metric_name name of the calculated metric
#' @param metric_value calculated value of the metric mentioned as metric_name
#' @param surface calculated surface of the sampling location (could be needed for calculation)
#' @param width_river width of the river at the sampling location (could be needed for calculation)
#' @param slope slope of the river at the sampling location (could be needed for calculation)
#'
#' @return A calculated metric score for the given values
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
#'
calculate_metric_score <-
  function(
    metric_score_name, indices, metric_name, metric_value, surface, width_river,
    slope
  ) {

  if (is.na(metric_score_name)) {
    return(NA)
  }

  result <- indices %>%
    filter(
      .data$metric == metric_name,
      var_in_interval(metric_value, .data$value_metric)
    )

  if (!is.na(unique(indices$add_category))) {
    variable <- unique(indices$add_category)
    result %<>%
      filter(
        var_in_interval(variable, .data$value_add_category)
      )
  }

  return(unique(result$score_id))
}
