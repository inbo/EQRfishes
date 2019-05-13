#' calculate the metrics of the EQR
#'
#' Calculates the metrics that are based on measures only, given a dataset with the information from calculate_metric_measures.csv and fish data.
#'
#' @param fishdata dataframe with fishdata
#' @param data_sample_metrics Dataset with at least the fields metric_type, speciesfilter, include_species_length, NULL_to_0, only_individual_measures, method (for 1 record)
#'
#' @return Dataset with calculated metric for each record
#'
#' @importFrom readr read_csv2
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter
#'
#' @export
#'
calculate_metric_measures <-
  function(
    fishdata, metric_type, speciesfilter, exclude_species_length,
    only_individual_measures
  ) {

  specieslist <-
    read_csv2(
      system.file("extdata/data_taxonmetrics.csv", package = "EQRfishes")
    )
  if (!is.na(speciesfilter)) {
    specieslist %<>%
      filter(eval(parse(text = speciesfilter)))
  }

  result <- fishdata %>%
    filter(.data$taxoncode %in% specieslist$taxoncode)

  if (!is.na(exclude_species_length)) {
    result %<>%
      filter(!eval(parse(text = exclude_species_length)))
  }

  if (!is.na(only_individual_measures)) {
    if (only_individual_measures == 1) {
      result %<>%
        filter(.data$number == 1)
    }
  }

  result <-
    switch(
      metric_type,
      number_of_species = number_of_species(result),
      number_of_individuals = number_of_individuals(result),
      total_weight = total_weight(result),
      sum_WF_Tolerantie = sum_values(result, "WF_Tolerantie"),
      sum_WF_Type_Barbeel = sum_values(result, "WR_Type_Barbeel")
    )

  return(unique(result))
}
