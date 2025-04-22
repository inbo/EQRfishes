#' determine the index typology of one location
#'
#' This helper function determines the index typology (`zonation`) based on the
#' given measurements for one location.
#' To determine the index typology of the whole dataset at once, please use
#' function `determine_zonation()`.
#'
#' @param var_width width of the river at the sample location
#' @param var_slope slope of the river at the sample location
#' @param var_tidal tidal effect present (TRUE) or absent (FALSE)?
#' @param var_indextype type based on salinity of the sample location
#' @param version 'new' version with bron and brabeel, or 'old' version without
#' these two zonations?
#' Defaults to 'new'.
#'
#' @return zonation of the focal location(s)
#'
#' @importFrom readr read_csv2
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export
#' @family helper
#'
#' @examples
#' library(EQRfishes)
#' determine_zonation_helper(
#'   var_width = 3,
#'   var_slope = 3,
#'   var_tidal = FALSE,
#'   var_indextype = "ZTWA",
#'   version = "new"
#' )
#'
determine_zonation_helper <- function(
  var_width, var_slope, var_tidal, var_indextype,
  version = c("new", "old")
) {

  match.arg(version)
  data_zonation <-
    suppressMessages(
      read_csv2(system.file("extdata/data_zonation.csv", package = "EQRfishes"))
    )
  data_zonation <- data_zonation %>%
    filter(
      .data$indextypecode == var_indextype
    )
  if (var_indextype == "ZTWA") {
    data_zonation <- data_zonation %>%
      filter(
        var_in_interval(var_width, .data$width),
        var_in_interval(var_slope, .data$slope)
      )
  }
  if (nrow(data_zonation) == 0) {
    return("(undetermined)")
  }

  if (version[1] == "new") {
    return(data_zonation$zonation)
  } else {
    return(data_zonation$zonation_old)
  }
}
