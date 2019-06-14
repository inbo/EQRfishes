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
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select
#' @importFrom tidyr nest unnest
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom purrr pmap
#'
#' @export
#'
calculate_metric_formula <- function(data_sample_fish) {

  if (nrow(data_sample_fish) == 0) {
    return(NA)
  }

  result <- data_sample_fish %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file(
            "extdata/calculate_metric_formula.csv", package = "EQRfishes"
          )
        )
      ) %>%
        filter(!is.na(.data$submetric_score_name)) %>%
        group_by(
          .data$metric_formula_name, .data$formula, .data$submetric_score_name
        ) %>%
        nest(.key = "submetric_name_group") %>%
        bind_rows(
          suppressMessages(
            read_csv2(
              system.file(
                "extdata/calculate_metric_formula.csv", package = "EQRfishes"
              )
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
      sampledata =
        calculate_metric(
          .,
          aberant_column_names =
            c(
              "submetric_name_group", "submetric_formula_name",
              "submetric_measures_name", "submetric_score_name", "new_row_id"
            )
        )
    ) %>%
    unnest(.data$sampledata) %>%
    group_by(
      .data$sample_key, .data$metric_name, .data$metric_formula_name,
      .data$formula, .data$metric_measures_name, .data$metric_score_name,
      .data$row_id
    ) %>%
    nest(.data$name, .data$value, .key = "sampledata") %>%
    mutate(
      sampledata =
        pmap(
          list(
            formula = .data$formula,
            sampledata = .data$sampledata,
            metric_name = .data$metric_formula_name
          ),
          calculate_formula
        )
          #formule berekenen (nog uitwerken!), hier overal checken dat noemer niet 0 of NA is
    ) %>%
    arrange(.data$row_id)

  return(result$sampledata)
}
