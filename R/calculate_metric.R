#' calculate the metrics of the EQR
#'
#' Calculates the metrics of the ecological quality ratio based on sample data and fish data.
#'
#' @param data_sample_fish Data on the sample with additional paramaters guild (calculated by calculate_guild) and surface and fishdata included
#' @param aberant_column_names default column names to refer to the metric names are metric_formula_name, metric_measures_name and metric_score_name. To recall this function in subfunctions, it could be necessary to rename column names to these standard names in this funtion.
#'
#' @return Dataset with calculated EQR for each sample
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr arrange bind_rows distinct group_by left_join mutate select
#' @importFrom plyr .
#' @importFrom readr read_csv2
#' @importFrom rlang .data has_name
#' @importFrom tidyr nest unnest
#' @importFrom purrr pmap map2
#'
#' @export
#'
calculate_metric <-
  function(
    data_sample_fish,
    aberant_column_names =
      c(
        "metric_name_group", "metric_formula_name", "metric_measures_name",
        "metric_score_name", "row_id"
      )
  ) {

  if (has_name(data_sample_fish, "formula")) {
    data_sample_fish %<>%
      select(
        -.data$formula, -.data$metric_formula_name, -.data$metric_measures_name,
        -.data$metric_score_name, -.data$row_id
      )
  }

  data_sample_fish %<>%
    #filter(!metric_name %in% c("MpsRekr")) %>%
    rename(
      metric_name_group = aberant_column_names[1],
      metric_score_name = aberant_column_names[4],
      row_id = aberant_column_names[5]
    ) %>%
    unnest(.data$metric_name_group, .preserve = .data$fishdata) %>%
    rename(
      metric_formula_name = aberant_column_names[2],
      metric_measures_name = aberant_column_names[3]
    )

  result_formula <- data_sample_fish %>%
    filter(!is.na(.data$metric_formula_name)) %>%
    arrange(.data$row_id) %>%
    mutate(
      metric_value = calculate_metric_formula(.),
      metric_name_calc = .data$metric_formula_name
    ) %>%
    select(-.data$metric_formula_name, -.data$metric_measures_name)

  result_measures <- data_sample_fish %>%
    filter(!is.na(.data$metric_measures_name)) %>%
    arrange(.data$row_id) %>%
    left_join(
      read_csv2(
        system.file(
          "extdata/calculate_metric_measures.csv", package = "EQRfishes"
        )
      ),
      by = "metric_measures_name", suffix = c("", "_info_measures")
    ) %>%
    mutate(
      metric_value =
        unlist(
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
        ),
      metric_name_calc = .data$metric_measures_name
    ) %>%
    select(
      -.data$metric_formula_name, -.data$metric_measures_name,
      -.data$metric_type, -.data$speciesfilter, -.data$exclude_species_length,
      -.data$only_individual_measures, -.data$NULL_to_0,  #de laatste 3 voorwaarden nog inwerken in script!
      -.data$method_info_measures, -.data$opmerking
    )

  result <- result_formula %>%
    bind_rows(result_measures) %>%
    arrange(.data$row_id) %>%
    group_by(
      .data$row_id, .data$metric_score_name, .data$surface, .data$width_river,
      .data$slope
    ) %>%
    nest(.data$metric_name_calc, .data$metric_value, .key = "metric_values") %>%
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
    arrange(.data$row_id) %>%
    mutate(
      metric_combi =
        pmap(
          list(
            metric_score_name = .data$metric_score_name,
            indices = .data$indices,
            metric_values = .data$metric_values,
            surface = .data$surface,
            width_river = .data$width_river,
            slope = .data$slope
          ),
          calculate_metric_score
        )
    ) %>%
    arrange(.data$row_id)

  return(result$metric_combi)
}
