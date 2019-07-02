#' calculate IBI based on given parameters
#'
#' small function that calculates IBI based on rules in calculate_IBI_EQR.csv or the formula 'sum of metric scores divided by number of metrics'
#'
#' @param guild_name guild of the location
#' @param metrics calculated metrics and metric scores
#'
#' @return single value being the result of the calculation
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter left_join summarise
#' @importFrom readr read_csv2
#'
#' @export
#'
#'
calculate_ibi_score <-
  function(
    guild_name, metrics
  ) {

  IBI_exceptions <-
    suppressMessages(
      read_csv2(
        system.file("extdata/calculate_IBI_EQR.csv", package = "EQRfishes")
      )
    ) %>%
    filter(
      .data$guild == guild_name,
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
      select(.data$result) %>%
      distinct()
  } else {
    IBI <- metrics %>%
      filter(!is.na(.data$metric_score_name)) %>%
      summarise(
        result = sum(as.numeric(.data$metric_score)) / n()
      )
  }

  return(IBI$result)
}
