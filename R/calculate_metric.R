#' calculate the metrics of the EQR
#'
#' Calculates the metrics of the ecological quality ratio based on sample data and fish data.
#'
#' @param data_sample_fish Data on the sample with additional paramaters zonation (calculated by calculate_zonation) and surface and fishdata included
#' @param aberant_column_names default column names to refer to the metric names are metric_formula_name, metric_measures_name and metric_score_name. To recall this function in subfunctions, it could be necessary to rename column names to these standard names in this funtion.
#'
#' @return Dataset with calculated EQR for each sample
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr arrange bind_rows distinct filter left_join mutate rename select
#' @importFrom plyr .
#' @importFrom readr read_csv2
#' @importFrom rlang .data has_name
#' @importFrom tidyr nest unnest
#' @importFrom purrr pmap
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
        -"formula", -"metric_formula_name", -"metric_measures_name",
        -"metric_score_name", -"row_id"
      )
  }

  data_sample_fish %<>%
    rename(
      metric_name_group = aberant_column_names[1],
      metric_score_name = aberant_column_names[4],
      row_id = aberant_column_names[5]
    ) %>%
    unnest(
      cols = "metric_name_group"
    ) %>%
    rename(
      metric_formula_name = aberant_column_names[2],
      metric_measures_name = aberant_column_names[3]
    )

  result_formula <- data_sample_fish %>%
    filter(!is.na(.data$metric_formula_name)) %>%
    arrange(.data$row_id) %>%
    mutate(
      sampledata = calculate_metric_formula(.),
      metric_name_calc = .data$metric_formula_name
    ) %>%
    select(-"metric_formula_name", -"metric_measures_name")

  result_measures <- data_sample_fish %>%
    filter(!is.na(.data$metric_measures_name)) %>%
    arrange(.data$row_id) %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file(
            "extdata/calculate_metric_measures.csv", package = "EQRfishes"
          )
        )
      ) %>%
        select(-"opmerking"),  # tijdelijk zolang in deze csv een opmerking staat
      by = "metric_measures_name", suffix = c("", "_info_measures")
    ) %>%
    mutate(
      sampledata =
        pmap(
          list(
            fishdata = .data$fishdata,
            metric_name = .data$metric_measures_name,
            metric_type = .data$metric_type,
            values_column = .data$values_column,
            speciesfilter = .data$speciesfilter,
            exclude_species_length = .data$exclude_species_length,
            only_individual_measures = .data$only_individual_measures,
            NULL_to_0 = .data$NULL_to_0,
            SalTru_to_SalFar = .data$SalTru_to_SalFar,
            sampledata = .data$sampledata
          ),
          calculate_metric_measures
        ),
      metric_name_calc = .data$metric_measures_name
    ) %>%
    select(
      -"metric_formula_name", -"metric_measures_name",
      -"metric_type", -"speciesfilter", -"exclude_species_length",
      -"only_individual_measures", -"NULL_to_0",  #de laatste 3 voorwaarden nog inwerken in script!
      -"method" #, -"opmerking"
    )

  if (nrow(result_formula) > 0) {
    result <- result_formula %>%
      bind_rows(result_measures)
  } else {
    result <- result_measures
  }
  result %<>%
    arrange(.data$row_id) %>%
    unnest(cols = "sampledata") %>%
    distinct() %>%
    select(
      "sample_key", "zonation", "metric_name", "metric_score_name",
      "row_id", "name", "value"
    ) %>%
    nest(sampledata = c("name", "value")) %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file(
            "extdata/calculate_metric_score.csv", package = "EQRfishes"
          )
        ) %>%
          select(-"opmerkingen")  # tijdelijk zolang in deze csv een opmerking staat
      ) %>%
        nest(
          indices =
            c("metric", "value_metric", "add_category",
              "value_add_category", "score_id")
        ),
      by = c("metric_score_name" = "metric_score")
    ) %>%
    arrange(.data$row_id) %>%
    mutate(
      sampledata =
        pmap(
          list(
            metric_score_name = .data$metric_score_name,
            indices = .data$indices,
            sampledata = .data$sampledata
          ),
          calculate_metric_score
        )
    ) %>%
    arrange(.data$row_id)

  return(result$sampledata)
}
