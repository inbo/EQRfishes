#' calculate the metrics of the EQR
#'
#' Calculates the metrics of the ecological quality ratio based on sample data and fish data.
#'
#' @param data_sample_fish Data on the sample with additional paramaters guild (calculated by calculate_guild) and surface and fishdata included
#' @param metric_names default column names to refer to the metric names are metric_formula_name, metric_measures_name and metric_score_name. To recall this function in subfunctions, it could be necessary to rename column names to these standard names in this funtion.
#'
#' @return Dataset with calculated EQR for each sample
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr arrange distinct group_by left_join mutate select
#' @importFrom plyr .
#' @importFrom readr read_csv2
#' @importFrom rlang .data has_name
#' @importFrom tidyr nest
#' @importFrom purrr pmap map2
#'
#' @export
#'
calculate_metric <-
  function(
    data_sample_fish,
    metric_names =
      c("metric_formula_name", "metric_measures_name", "metric_score_name")
  ) {

  if (has_name(data_sample_fish, "rownr")) {
    data_sample_fish %<>%
      mutate(
        rownr = 1:length(.data$rownr)
      )
  } else {
    data_sample_fish %<>%
      mutate(
        rownr = 1:length(.data$sample_key)
      )
  }

  if (has_name(data_sample_fish, "formula")) {
    data_sample_fish %<>%
      select(
        -.data$formula, -.data$metric_formula_name, -.data$metric_measures_name,
        -.data$metric_score_name
      )
  }

  result <- data_sample_fish %>%
    rename(
      metric_formula_name = metric_names[1],
      metric_measures_name = metric_names[2],
      metric_score_name = metric_names[3]
    ) %>%
    mutate(
      metric_value_formula =
        ifelse(
          is.na(.data$metric_formula_name),
          NA,
          calculate_metric_formula(.)
        )
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
    ) %>%
    arrange(.data$rownr)

  return(result$metric_combi)
}
