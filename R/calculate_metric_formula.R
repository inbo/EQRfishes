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
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate rename select summarise ungroup
#' @importFrom tidyr nest
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom purrr map
#'
#' @export
#'
calculate_metric_formula <- function(data_sample_fish) {

  if (nrow(data_sample_fish) == 0) {
    return(NA)
  }

  result <- data_sample_fish %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_formula.csv", package = "EQRfishes"
        )
      ) %>%
        filter(!is.na(.data$submetric_score_name)) %>%
        group_by(
          .data$metric_formula_name, .data$formula, .data$submetric_score_name
        ) %>%
        nest(.key = "submetric_name_group") %>%
        bind_rows(
          read_csv2(
            system.file(
              "extdata/calculate_metric_formula.csv", package = "EQRfishes"
            )
          ) %>%
            filter(is.na(.data$submetric_score_name)) %>%
            mutate(temp_row_nr = 1:length(.data$metric_formula_name)) %>%
            group_by(
              .data$temp_row_nr, .data$metric_formula_name, .data$formula,
              .data$submetric_score_name
            ) %>%
            nest(.key = "submetric_name_group") %>%
            select(-.data$temp_row_nr)
        ),
      by = "metric_formula_name", suffix = c("", "_")
    ) %>%
    arrange(.data$row_id) %>%
    mutate(
      new_row_id = 1:length(.data$row_id)
    ) %>%
    mutate(
      submetric_results =
        calculate_metric(
          .,
          aberant_column_names =
            c(
              "submetric_name_group", "submetric_formula_name",
              "submetric_measures_name", "submetric_score_name", "new_row_id"
            )
        ),
      submetric_value = unlist(map(.data$submetric_results, unlist_value)),
      submetric_score = unlist(map(.data$submetric_results, unlist_score)),
      submetric_name =
        unlist(map(.data$submetric_name_group, unlist_name_group))
    ) %>%
    group_by(
      .data$sample_key, .data$metric_name, .data$metric_formula_name,
      .data$metric_measures_name, .data$metric_score_name,
      .data$row_id
    ) %>%
    summarise(
      metric_value =
        calculate_formula(
          formula = unique(.data$formula),
          submetric_name = .data$submetric_name,
          submetric_value = .data$submetric_value,
          submetric_score_name = .data$submetric_score_name,
          submetric_score = .data$submetric_score,
          surface = unique(.data$surface)
        )
          #formule berekenen (nog uitwerken!), hier overal checken dat noemer niet 0 of NA is
    ) %>%
    ungroup() %>%
    arrange(.data$row_id)

  return(result$metric_value)
}
