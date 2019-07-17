#' determine the guild of the location
#'
#' This function determines the guild based on the given measurements for one location.
#'
#' @param var_width width of the river at the sample location
#' @param var_slope slope of the river at the sample location
#' @param var_tidal tidal effect present (TRUE) or absent (FALSE)?
#' @param var_indextype type based on salinity of the sample location
#' @param var_method sampling method
#'
#' @return guild of the focal location(s)
#'
#' @importFrom readr read_csv2
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom magrittr %<>%
#'
#' @export
#'
determine_zonation <-
  function(var_width, var_slope, var_tidal, var_indextype, var_method) {

  data_guild <-
    suppressMessages(
      read_csv2(system.file("extdata/data_guild.csv", package = "EQRfishes"))
    )
  data_guild %<>%
    filter(
      .data$indextypecode == var_indextype
    )
  if (var_indextype == "ZTWA") {
    data_guild %<>%
      filter(
        var_in_interval(var_width, .data$width),
        var_in_interval(var_slope, .data$slope)
      )
  } else {
    data_guild %<>%
      filter(
        is.na(.data$method) | str_detect(var_method, .data$method)
      )
  }
  if (nrow(data_guild) == 0) {
    return("(undetermined)")
  }

  return(data_guild$guild)
}
