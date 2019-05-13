#' calculate the metrics of the EQR
#'
#' Calculates the metrics of the ecological quality ratio based on sample data and fish data.
#'
#' @param data_sample_fish Data on the sample with additional paramaters guild (calculated by calculate_guild) and surface and fishdata included
#' @inheritParams calculate_eqr
#'
#' @return Dataset with calculated EQR for each sample
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate select distinct group_by
#' @importFrom readr read_csv2
#' @importFrom tidyr nest
#' @importFrom purrr map2
#'
#' @export
#'
calculate_metric <- function(data_sample_fish) {

  result <- data_sample_fish %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_formula.csv", package = "EQRfishes"
        )
      ),
      by = "metric_formula_name"
    ) %>%
    mutate(
      submetric_score =
        ifelse(
          is.na(.data$submetric_calculate),
          NA,
          calculate_metric(...)
        ),
      metric =
        ifelse(
          is.na(.data$formula),
          NA,
          #formule berekenen
        )
    ) %>%
    select(
      -.data$formula,
      -.data$submetric_formula_name,
      -.data$submetric_measures_name,
      -.data$submetric_score_name,
      -.data$submetric_score
    ) %>%
    distinct() %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_measures.csv", package = "EQRfishes"
        )
      ) %>%
        group_by(.data$metric_measures_name) %>%
        nest(.key = "metric_measures_info"),
      by = "metric_measures_name"
    ) %>%
    mutate(
      metric =
        ifelse(
          is.na(.data$metric_measures_name),
          .data$metric,
          map2(
            .data$fishdata, .data$metric_measures_info,
            calculate_metric_measures
          )
        )
    ) %>%
    select(-.data$metric_measure_info) #en dan nog de scores berekenen en metric + metric_score teruggeven!


  return(result)
}
