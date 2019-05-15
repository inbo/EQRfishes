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
#' @importFrom dplyr distinct group_by left_join mutate row_number select
#' @importFrom plyr .
#' @importFrom readr read_csv2
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom purrr pmap map2
#'
#' @export
#'
calculate_metric <- function(data_sample_fish) {

  result <- data_sample_fish %>%
    mutate(
      rownr = row_number()
    ) %>%
    mutate(
      metric_value_formula = calculate_metric_formula(.)
    ) %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_measures.csv", package = "EQRfishes"
        )
      ),
      by = "metric_measures_name", suffix = c("", "_info_measures")
    ) %>%
    mutate(
      metric_value_measures =
        pmap(
          list(
            fishdata = .data$fishdata,
            metric_type = .data$metric_type,
            speciesfilter = .data$speciesfilter,
            exclude_species_length = .data$exclude_species_length,
            only_individual_measures = .data$only_individual_measures
          ),
          calculate_metric_measures
        ),
      metric_value =
        ifelse(
          is.na(.data$metric_value_formula),
          .data$metric_value_measures,
          .data$metric_value_formula
        )
    ) %>%
    select(
      -.data$metric_type, -.data$speciesfilter, -.data$exclude_species_length,
      -.data$only_individual_measures, -.data$NULL_to_0,  #de laatste 3 voorwaarden nog inwerken in script!
      -.data$method_info_measures, -.data$opmerking
    ) %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_score.csv", package = "EQRfishes"
        )
      ) %>%
        group_by(.data$metric_score) %>%
        nest(.key = "indices"),
      by = c("metric_score_name" = "metric_score")
    ) %>%
    mutate(
      metric_score =
        ifelse(
          is.na(.data$metric_score_name),
          NA,
          pmap(
            list(
              indices = .data$indices,
              metric_name =
                ifelse(
                  is.na(.data$metric_formula_name),
                  .data$metric_measures_name,
                  .data$metric_formula_name
                ),
              metric_value = .data$metric_value,
              width_river = .data$width_river,
              slope = .data$slope
            ),
            calculate_metric_score
          )
        ),
      metric_combi = map2(.data$metric_value, .data$metric_score, list)
    )

  return(result$metric_combi)
}
