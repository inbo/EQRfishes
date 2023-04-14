#' calculate IBI based on given parameters
#'
#' Small function that calculates IBI based on rules in calculate_IBI_EQR.csv or the formula 'sum of metric scores divided by number of metrics' (when using the old calculation method) or the formula 'sum of metric scores' (when using the new calculation method). Distinction between old an new method is made by the absence (old method) or presence (new method) of a specified method for the calculated methods.
#'
#' @param zonation_name zonation of the location
#' @param metrics calculated metrics and metric scores
#' @param calc_method_old does the calculation has to be done using the old method?
#'
#' @return single value being the result of the calculation
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr distinct filter left_join summarise
#' @importFrom readr read_csv2
#'
#' @export
#'
#'
calculate_ibi_score <-
  function(
    zonation_name, metrics, calc_method_old
  ) {

  IBI_exceptions <-
    suppressMessages(
      read_csv2(
        system.file("extdata/calculate_IBI_EQR.csv", package = "EQRfishes")
      )
    ) %>%
    filter(
      .data$zonation == zonation_name,
      .data$to_calculate == "IBI"
    ) %>%
    left_join(
      metrics,
      by = c("calculated" = "metric_name")
    ) %>%
    filter(
      var_in_interval(.data$metric_value, .data$interval)
    )

  if (!all(is.na(IBI_exceptions$calculated2))) {
    IBI_exceptions %<>%
      left_join(
        metrics,
        by = c("calculated2" = "metric_name"),
        suffix = c("", "2")
      ) %>%
      filter(
        var_in_interval(.data$metric_value2, .data$interval2)
      )
  }

  if (nrow(IBI_exceptions) > 0) {
    IBI <- IBI_exceptions %>%
      select("result") %>%
      distinct()
  } else {
    IBI <- metrics %>%
      filter(!is.na(.data$metric_score_name)) %>%
      summarise(
        result = sum(as.numeric(.data$metric_score)),
        result = ifelse(calc_method_old, .data$result / n(), .data$result)
      )
  }

  return(IBI$result)
}
