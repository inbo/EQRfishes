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
#'
#' @export
#'
#'
calculate_eqr_score <-
  function(
    guild, IBI
  ) {

  EQR_formula <-
    read_csv2(
      system.file("extdata/calculate_IBI_EQR.csv", package = "EQRfishes")
    ) %>%
    filter(
      .data$guild == guild,
      .data$to_calculate == "EQR",
      var_in_interval(IBI, .data$interval)
    )

  EQR <- evals(EQR_formula$result)[[1]]$result

  return(EQR)
}
