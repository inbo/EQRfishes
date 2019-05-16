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
      by = "metric_formula_name", suffix = c("", "_")
    ) %>%
    filter(!is.na(.data$submetric_measures_name)) %>% #tijdelijk!!!!!
    mutate(
      submetric_results =
        ifelse(
          is.na(.data$formula),
          NA,
          calculate_metric(
            .,
            metric_names =
              c(
                "submetric_formula_name", "submetric_measures_name",
                "submetric_score_name"
              )
          )
        ),
      submetric_value = unlist(map(.data$submetric_results, unlist_value)),
      submetric_score = unlist(map(.data$submetric_results, unlist_score))
    ) %>%
    group_by(
      .data$sample_key, .data$metric_name, .data$metric_formula_name,
      .data$metric_measures_name, .data$metric_score_name,
      .data$rownr
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
          submetric_score = .data$submetric_score,
          surface = unique(.data$surface)
        )
          #formule berekenen (nog uitwerken!), hier overal checken dat noemer niet 0 of NA is
    ) %>%
    ungroup() %>%
    arrange(.data$rownr)

  return(result$metric_value)
}
