#' determine the index typology of one location
#'
#' This helper function determines the index typology based on the
#' given measurements for one location.
#' In case the result is "stilstaand" (slope = 0), the index typology can be
#' "lakes" or "canals", which should be determined based on the morphology
#' of the water object.
#' To determine the index typology of the whole dataset at once, please use
#' function `determine_indextypology()`.
#'
#' @param var_width width of the river at the sample location (meters)
#' @param var_slope slope of the river at the sample location (per thousand)
#' @param var_tidal tidal effect present (TRUE) or absent (FALSE)?
#' @param var_indextype type based on salinity of the sample location:
#'   - `"ZTWA"` for freshwater,
#'   - `"YZRP"` for estuarine Ijzer,
#'   - `"SCHM"` for estuarine Schelde mesohaline,
#'   - `"SCHO"` for estuarine Schelde oligohaline,
#'   - `"SCHZ"` for estuarine Schelde freshwater,
#'   - `"ZTWZ"` for estuarine Schelde freshwater
#' @param version 'new' version with bron and brabeel, or 'old' version without
#' these two indextypologies?
#' Defaults to 'new'.
#'
#' @return indextypology of the focal location(s)
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
#' determine_indextypology_helper(
#'   var_width = 3,
#'   var_slope = 3,
#'   var_tidal = FALSE,
#'   var_indextype = "ZTWA",
#'   version = "new"
#' )
#'
determine_indextypology_helper <- function(
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
    return(data_zonation$indextypology)
  } else {
    return(data_zonation$indextypology_old)
  }
}
