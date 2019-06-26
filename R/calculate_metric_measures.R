#' calculate the metrics of the EQR based on measures
#'
#' Calculates the metrics that are based on measures only, given a dataset with the information from calculate_metric_measures.csv and fish data.
#'
#' @param fishdata dataframe with fishdata
#' @param metric_name name of the variable to be calculated
#' @param metric_type reflects which information must be calculated: number_of_species, number_of_individuals, total_weight,... (info from calculate_metric_measures.csv)
#' @param values_column additional information for column if metric_type refers to colomn from data_taxonmetrics.csv (info from calculate_metric_measures.csv)
#' @param speciesfilter formula indicating how to select the required species from data_taxonmetrics.csv (info from calculate_metric_measures.csv)
#' @param exclude_species_length formula indicating which individuals have to be EXCLUDED based on fish characteristics such as length (info from calculate_metric_measures.csv)
#' @param only_individual_measures value 1 indicates that only individually measured data should be used (info from calculate_metric_measures.csv)
#' @param NULL_to_0 value 1 indicates that result NULL should be replaced by 0
#' @param sampledata table with earlier calculated variables to which the newly calculated variable should be added
#'
#' @return table sampledata in which the newly calculated metric is added
#'
#' @importFrom readr read_csv2
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr bind_rows filter
#' @importFrom rlang .data
#'
#' @export
#'
calculate_metric_measures <-
  function(
    fishdata, metric_name, metric_type, values_column, speciesfilter,
    exclude_species_length, only_individual_measures, NULL_to_0, sampledata
  ) {

  specieslist <-
    suppressMessages(
      read_csv2(
        system.file("extdata/data_taxonmetrics.csv", package = "EQRfishes")
      )
    )
  if (!is.na(speciesfilter)) {
    specieslist %<>%
      filter(eval(parse(text = speciesfilter)))
    fishdata %<>%
      filter(.data$taxoncode %in% specieslist$taxoncode)
  }

  if (!is.na(exclude_species_length)) {
    fishdata %<>%
      filter(!eval(parse(text = exclude_species_length)))
  }

  if (!is.na(only_individual_measures)) {
    if (only_individual_measures == 1) {
      fishdata %<>%
        filter(.data$number == 1)
    }
  }

  result <-
    ifelse(
      nrow(fishdata) == 0,
      0,
      switch(
        metric_type,
        number_of_species = number_of_species(fishdata),
        number_of_individuals = number_of_individuals(fishdata),
        number_of_length_classes = number_of_length_classes(fishdata),
        sum_of_scored_length_classes =
          sum_of_scored_length_classes(fishdata, values_column),
        total_weight = total_weight(fishdata),
        sum_values_column =
          sum_values_column(fishdata, specieslist, values_column),
        shannon_wiener_index = shannon_wiener_index(fishdata, specieslist)
      )
    )

  if (!is.na(NULL_to_0) & is.null(result)) {
    if (NULL_to_0 == 1) {
      result <- 0
    }
  }

  sampledata %<>%
    bind_rows(
      data.frame(
        name = metric_name,
        value = as.character(result),
        stringsAsFactors = FALSE
      )
    )

  return(sampledata)
}
