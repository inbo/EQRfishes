#' calculate the metrics of the EQR
#'
#' Calculates the metrics that are based on measures only, given a dataset with the information from calculate_metric_measures.csv and fish data.
#'
#' @param fishdata dataframe with fishdata
#' @param data_sample_metrics Dataset with at least the fields metric_type, speciesfilter, include_species_length, NULL_to_0, only_individual_measures, method (for 1 record)
#'
#' @return Dataset with calculated metric for each record
#'
calculate_metric_measures <- function(fishdata, data_sample_metrics) {

  specieslist <-
    read_csv2(
      system.file("extdata/data_taxonmetrics.csv", package = "EQRfishes")
    )
  if (!is.na(data_sample_metrics$speciesfilter)) {
    specieslist %<>%
      filter(eval(parse(text = data_sample_metrics$speciesfilter)))
  }

  result <- fishdata %>%
    filter(.data$taxoncode %in% specieslist$taxoncode)

}
