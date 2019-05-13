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
#' @importFrom purrr pmap
#'
#' @export
#'
calculate_metric <- function(data_sample_fish) {

  result <- data_sample_fish %>%
    mutate(
      metric =
        ifelse(
          is.na(.data$metric_formula_name),
          NA,
          calculate_metric_formula(.data)
        )
    ) %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_measures.csv", package = "EQRfishes"
        )
      ),
      by = "metric_measures_name"
    ) %>%
    mutate(
      metric =
        ifelse(
          is.na(.data$metric_measures_name),
          .data$metric,
          pmap(
            list(
              fishdata = .data$fishdata,
              metric_type = .data$metric_type,
              speciesfilter = .data$speciesfilter,
              exclude_species_length = .data$exclude_species_length,
              only_individual_measures = .data$only_individual_measures
            ),
            calculate_metric_measures
          )
        )
    ) %>%
    select(
      -.data$metric_type, -.data$speciesfilter, -.data$exclude_species_length,
      -.data$only_individual_measures
    )

  return(unlist(result$metric))
}
