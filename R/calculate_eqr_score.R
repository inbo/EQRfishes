#' calculate EQR based on given parameters
#'
#' small function that calculates EQR based on rules in calculate_IBI_EQR.csv
#'
#' @inheritParams calculate_ibi_score
#' @param IBI calculated IBI score
#'
#' @return single value being the result of the calculation
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom pander evals
#' @importFrom readr read_csv2
#'
#' @export
#'
#'
calculate_eqr_score <-
  function(
    guild_name, IBI
  ) {

  EQR_formula <-
    suppressMessages(
      read_csv2(
        system.file("extdata/calculate_IBI_EQR.csv", package = "EQRfishes")
      )
    ) %>%
    filter(
      .data$guild == guild_name,
      .data$to_calculate == "EQR"
    ) %>%
    filter(
      var_in_interval(IBI, .data$interval)
    ) %>%
    mutate(
      result = gsub("IBI", IBI, .data$result)
    )

  EQR <- evals(EQR_formula$result)[[1]]$result

  return(
    data.frame(
      EQR = EQR, interval_IBI = EQR_formula$interval, stringsAsFactors = FALSE
    )
  )
}
