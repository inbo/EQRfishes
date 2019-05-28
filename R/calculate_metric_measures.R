#' calculate the metrics of the EQR based on measures
#'
#' Calculates the metrics that are based on measures only, given a dataset with the information from calculate_metric_measures.csv and fish data.
#'
#' @param fishdata dataframe with fishdata
#' @param metric_type reflects which information must be calculated: number_of_species, number_of_individuals, total_weight,... (info from calculate_metric_measures.csv)
#' @param speciesfilter formula indicating how to select the required species from data_taxonmetrics.csv (info from calculate_metric_measures.csv)
#' @param exclude_species_length formula indicating which individuals have to be EXCLUDED based on fish characteristics such as length (info from calculate_metric_measures.csv)
#' @param only_individual_measures value 1 indicates that only individually measured data should be used (info from calculate_metric_measures.csv)
#'
#' @return A calculated value for the metric based on the given dataset and information
#'
#' @importFrom readr read_csv2
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
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
    ifelse(
      nrow(result) == 0,
      0,
      switch(
        metric_type,
        number_of_species = number_of_species(result),
        number_of_individuals = number_of_individuals(result),
        total_weight = total_weight(result),
        sum_WF_Tolerantie = sum_values(result, specieslist, "WF_Tolerantie"),
        sum_WF_Type_Barbeel = sum_values(result, specieslist, "WF_Type_Barbeel")
      )
    )

  return(unique(result))
}
