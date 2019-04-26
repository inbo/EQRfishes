#' calculate the ecological quality ratio
#'
#' Main function of this package, which calculates the EQR based on 2 tables of data.  Each table must contain a sample_key!
#'
#' @param data_sample Data on the sample: date, method, location and location characteristics
#' @param data_fish Measurements on fish: taxon, length, weight, number of individuals
#'
#' @return Dataset with calculated EQR for each sample
#'
calculate_eqr <- function(data_sample, data_fish) {

  data_sample <- calculate_vars(data_sample)
  data_sample <- determine_guild(data_sample)
  result <- calculate_metrics(data_sample, data_fish)

  return(result)
}
