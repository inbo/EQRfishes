#' calculate the metrics of the Ecological Quality Ratio
#'
#' Calculates the metrics of the Ecological Quality Ratio (EQR) based on sample
#' data and fish data.
#'
#' @param data_sample_fish Data on the sample with additional parameters
#' indextypology (calculated by `determine_indextypology()`) and surface
#' and `fishdata` included
#' @param aberant_column_names default column names to refer to the metric names
#' are `metric_formula_name`, `metric_measures_name` and `metric_score_name`.
#' To recall this function in subfunctions, it could be necessary to rename
#' column names to these standard names in this function.
#' @param specieslist dataframe with all fish species and data on species level
#' that are needed to calculate the metrics (e.g. tolerance or typical value,
#' or 0/1 to indicate if the species should be included).
#' Defaults to the table in
#' `system.file("extdata/data_taxonmetrics.csv", package = "EQRfishes")`.
#'
#' @return Dataset with calculated EQR for each sample
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat has_name
#' @importFrom dplyr arrange bind_rows distinct filter left_join mutate rename
#' select
#' @importFrom plyr .
#' @importFrom readr read_csv2
#' @importFrom rlang .data
#' @importFrom tidyr nest unnest
#' @importFrom purrr pmap
#'
#' @noRd
#' @family helper
#'
calculate_metric <- function(
  data_sample_fish,
  aberant_column_names =
    c(
      "metric_name_group", "metric_formula_name", "metric_measures_name",
      "metric_score_name", "row_id"
    ),
  specieslist = suppressMessages(
    read_csv2(
      system.file("extdata/data_taxonmetrics.csv", package = "EQRfishes")
    )
  )
) {

  if (has_name(data_sample_fish, "formula")) {
    data_sample_fish <- data_sample_fish %>%
      select(
        -"formula", -"metric_formula_name", -"metric_measures_name",
        -"metric_score_name", -"row_id"
      )
  }

  data_sample_fish <- data_sample_fish %>%
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
      sampledata = calculate_metric_formula(., specieslist = specieslist),
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
      ),
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
            null_to_0 = .data$null_to_0,
            saltru_to_salfar = .data$saltru_to_salfar,
            sampledata = .data$sampledata
          ),
          calculate_metric_measures,
          specieslist = specieslist
        ),
      metric_name_calc = .data$metric_measures_name
    ) %>%
    select(
      -"metric_formula_name", -"metric_measures_name",
      -"metric_type", -"speciesfilter", -"exclude_species_length",
      -"only_individual_measures", -"null_to_0"
    )

  if (nrow(result_formula) > 0) {
    result <- result_formula %>%
      bind_rows(result_measures)
  } else {
    result <- result_measures
  }
  result <- result %>%
    arrange(.data$row_id) %>%
    unnest(cols = "sampledata") %>%
    distinct() %>%
    select(
      "sample_id", "indextypology", "metric_name", "metric_score_name",
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
