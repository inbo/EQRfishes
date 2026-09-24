#' determine the index typology of one location
#'
#' This helper function determines the index typology based on the
#' given measurements for one location.
#' In case the result is "lentic" (slope = 0), the index typology can be
#' "lakes" or "canals", which should be determined based on the morphology
#' of the water object.
#' To determine the index typology of the whole dataset at once, please use
#' function `determine_indextypology()`.
#'
#' @param var_width width of the river at the sample location (meters)
#' @param var_slope slope of the river at the sample location (per thousand)
#' @param var_indextype type based on salinity of the sample location
#' (detailed description in
#' `vignette("indextypologies", package = "EQRfishes")`):
#'   - `"ZTWA"` for freshwater that is not under tidal influence,
#'   - `"YZRP"` for the tidal section of the IJzer estuary (Belgium),
#'   - `"SCHM"` for the mesohaline zone (salinity 5-18) of the Zeeschelde
#'     estuary between Zandvliet (Dutch-Belgian border) and Antwerpen (Belgium),
#'   - `"SCHO"` for the oligohaline zone (salinity 0.5-5) of the Zeeschelde
#'     estuary (Belgium) between Antwerpen en Rupelmonde including the Rupel
#'     river,
#'   - `"SCHZ"` for the freshwater zone of the Zeeschelde estuary (Belgium)
#'     between Rupelmonde and Gent including the River Durme
#'     (limnetic zone: < 0.5),
#'   - `"ZTWZ"` for freshwater tributaries of the Zeeschelde estuary (Belgium)
#'     that are under tidal influence
#' @param version 'new' version with `fish-based river source IBI` and
#' `fish-based lowland IBI`, or 'old' version without
#' these two index typologies?
#' Defaults to 'new'.
#'
#' @return index typology of the focal location(s)
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
#'   var_indextype = "ZTWA",
#'   version = "new"
#' )
#'
determine_indextypology_helper <- function(
  var_width, var_slope, var_indextype,
  version = c("new", "old")
) {

  match.arg(version)
  data_indextypology <-
    suppressMessages(
      read_csv2(
        system.file("extdata/data_indextypology.csv", package = "EQRfishes")
      )
    )
  data_indextypology <- data_indextypology %>%
    filter(
      .data$indextypecode == var_indextype
    )
  if (var_indextype == "ZTWA") {
    data_indextypology <- data_indextypology %>%
      filter(
        var_in_interval(var_width, .data$width),
        var_in_interval(var_slope, .data$slope)
      )
  }
  if (nrow(data_indextypology) == 0) {
    return("(undetermined)")
  }

  if (version[1] == "new") {
    return(data_indextypology$indextypology)
  } else {
    return(data_indextypology$indextypology_old)
  }
}
