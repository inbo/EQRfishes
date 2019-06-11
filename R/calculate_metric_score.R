#' calculate the metric scores of the EQR
#'
#' Calculates the metric score, given a calculated metric value (and some parameters specific to the sampling location), and a table with the indices and their tresholds (info from calculate_metric_score.csv).
#'
#' @param metric_score_name name of metric score to be calculated (NA if no calculation has to be done)
#' @param indices dataframe with indices and their tresholds (info from calculate_metric_score.csv)
#' @inheritParams calculate_formula
#'
#' @return A calculated metric score for the given values
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr bind_rows filter
#' @importFrom rlang .data
#'
#' @export
#'
calculate_metric_score <-
  function(
    metric_score_name, indices, sampledata
  ) {

  if (is.na(metric_score_name)) {
    return(sampledata)
  }

  metric_value1 <- sampledata %>%
    filter(.data$name == unique(indices$metric))

  result <- indices %>%
    filter(
      .data$metric == metric_value1$name,
      var_in_interval(metric_value1$value, .data$value_metric)
    )

  if (!is.na(unique(indices$add_category))) {
    metric_value_add <- sampledata %>%
      filter(.data$name == unique(indices$add_category))
    result %<>%
      filter(
        .data$add_category == unique(indices$add_category),
        var_in_interval(metric_value_add$value, .data$value_add_category)
      )
  }

  sampledata %<>%
    bind_rows(
      data.frame(
        name = metric_score_name,
        value = as.character(unique(result$score_id)),
        stringsAsFactors = FALSE
      )
    )

  return(sampledata)
}
