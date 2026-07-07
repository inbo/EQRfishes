#' calculate IBI based on given parameters
#'
#' Small function that calculates IBI based on rules in `calculate_ibi_eqr.csv`
#' or the formula 'sum of metric scores divided by number of metrics'
#' (when using the old calculation method)
#' or the formula 'sum of metric scores'
#' (when using the new calculation method).
#' Distinction between old an new method is made by the absence (old method)
#' or presence (new method) of a specified method for the calculated methods.
#'
#' @param zonation_name indextypology of the location
#' @param metrics calculated metrics and metric scores
#' @param calc_method_old does the calculation has to be done using the old
#' method?
#'
#' @return single value being the result of the calculation
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr distinct filter left_join summarise
#' @importFrom readr read_csv2
#'
#' @noRd
#' @family helper
#'
#'
calculate_ibi_score <- function(
  zonation_name, metrics, calc_method_old, ibi_exceptions
) {

  if (!is.null(ibi_exceptions)) {
    ibi_exceptions <- ibi_exceptions %>%
      filter(
        .data$to_calculate == "IBI"
      ) %>%
      left_join(
        metrics,
        by = c("calculated" = "metric_name")
      ) %>%
      filter(
        var_in_interval(.data$metric_value, .data$interval)
      )
  }

  if (!is.null(ibi_exceptions) && !all(is.na(ibi_exceptions$calculated2))) {
    ibi_exceptions <- ibi_exceptions %>%
      left_join(
        metrics,
        by = c("calculated2" = "metric_name"),
        suffix = c("", "2")
      ) %>%
      filter(
        var_in_interval(.data$metric_value2, .data$interval2)
      )
  }

  if (!is.null(ibi_exceptions) && nrow(ibi_exceptions) > 0) {
    ibi <- ibi_exceptions %>%
      select("result") %>%
      distinct()
  } else {
    ibi <- metrics %>%
      filter(!is.na(.data$metric_score_name)) %>%
      summarise(
        result = sum(as.numeric(.data$metric_score)),
        result =
          ifelse(calc_method_old, .data$result / n(), .data$result)
      )
  }

  return(ibi$result)
}
