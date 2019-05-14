#' calculate the metrics of the EQR based on a formula
#'
#' Calculates the metrics that are based on formulas only, given fish data and other .
#'
#' @inheritParams calculate_metric
#'
#' @return Dataset with calculated metric for each record
#'
#' @importFrom readr read_csv2
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by left_join mutate summarise ungroup
#' @importFrom plyr .
#' @importFrom rlang .data
#'
#' @export
#'
calculate_metric_formula <- function(data_sample_fish) {

  result <- data_sample_fish %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_formula.csv", package = "EQRfishes"
        )
      ),
      by = "metric_formula_name"
    ) %>%
    filter(!is.na(.data$submetric_measures_name)) %>% #tijdelijk!!!!!
    mutate(
      metric_formula_name_parent = .data$metric_formula_name,
      metric_measures_name_parent = .data$metric_measures_name,
      metric_score_name_parent = .data$metric_measures_name,
      metric_formula_name = .data$submetric_formula_name,
      metric_measures_name = .data$submetric_measures_name,
      metric_score_name = .data$submetric_score_name
    ) %>%
    mutate(
      submetric_score = calculate_metric(.)
    ) %>%
    group_by(
      .data$sample_key, .data$metric_name, .data$metric_formula_name_parent,
      .data$metric_measures_name_parent, .data$metric_score_name_parent
    ) %>%
    summarise(
      metric =
        calculate_formula(
          formula = unique(.data$formula),
          metric_name = .data$metric_measures_name,
          metric_value = .data$submetric_score
        )
          #formule berekenen (nog uitwerken!), hier overal checken dat noemer niet 0 of NA is
    ) %>%
    ungroup()

  return(unlist(result$metric))
}
