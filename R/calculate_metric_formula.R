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
#' @importFrom dplyr arrange filter group_by left_join mutate rename summarise ungroup
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom purrr map
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
      submetric_results = calculate_metric(.),
      submetric_value = unlist(map(.data$submetric_results, unlist_value)),
      submetric_score = unlist(map(.data$submetric_results, unlist_score))
    ) %>%
    group_by(
      .data$sample_key, .data$metric_name, .data$metric_formula_name_parent,
      .data$metric_measures_name_parent, .data$metric_score_name_parent
    ) %>%
    summarise(
      metric_value =
        calculate_formula(
          formula = unique(.data$formula),
          submetric_name =
            ifelse(
              is.na(.data$submetric_formula_name),
              .data$submetric_measures_name,
              .data$submetric_formula_name
            ),
          submetric_value = .data$submetric_value,
          submetric_score_name = .data$submetric_score_name,
          submetric_score = .data$submetric_score
        )
          #formule berekenen (nog uitwerken!), hier overal checken dat noemer niet 0 of NA is
    ) %>%
    ungroup() %>%
    rename(
      metric_formula_name = .data$metric_formula_name_parent,
      metric_measures_name = .data$metric_measures_name_parent,
      metric_score_name = .data$metric_score_name_parent
    ) %>%
    arrange(.data$rownr)

  return(result$metric_value)
}
