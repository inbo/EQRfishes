#' calculate a standardised IBI based on given parameters
#'
#' Small function that calculates a value to be used to calculate the EQR. This value is derived from the IBI and gives a score between 0 and 1 (and could be seen as a standardisation of the IBI). This calculation differs between the old (no sampling method specified) and new calculation method (metrics only calculated for specific sampling methods).For the old method (where IBI is already corrected for the number of metrics), this is IBI//c with c the number of EQR classes (or 1//the width of an EQR class, which is 0.2). For the new method, this is (IBI - x) // (y - x) with y the sum of the maximal scores of the metrics and x = n//c with n the number of metrics and c the number of EQR classes (or 1//the width of an EQR class, which is 0.2).
#'
#' @param IBI IBI-score, result of the function calculate_ibi_score
#' @inheritParams calculate_ibi_score
#'
#' @return single value being the result of the calculation
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr distinct group_by inner_join select summarise
#' @importFrom readr read_csv2
#'
#' @export
#'
#'
standardise_ibi <-
  function(
    IBI, metrics, calc_method_old, zonation
  ) {

  c <- ifelse(zonation %in% c("bron", "estuarien_IJzer"), 1, 5)
  #this is now always 5, but it would be better to derive this from score.csv!!!!!!!!!!
  metrics2 <- metrics %>%
    select("metric_score_name", "method_for_metric") %>%
    distinct() %>%
    inner_join(
      suppressMessages(
        read_csv2(
          system.file(
            "extdata/calculate_metric_score.csv", package = "EQRfishes"
          )
        )
      ) %>%
        group_by(.data$metric_score) %>%
        summarise(max_score = max(.data$score_id)),
      by = c("metric_score_name" = "metric_score")
    ) %>%
    summarise(
      max_score = sum(.data$max_score),
      number_of_metrics = n()
    )

  if (calc_method_old) {
    std_ibi <- IBI / c
  } else {
    std_ibi <-
      (IBI - metrics2$number_of_metrics / c) /
      (metrics2$max_score - metrics2$number_of_metrics / c)
  }

  return(std_ibi)
}
