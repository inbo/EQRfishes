#' determine the guild of the location
#'
#' This function determines the guild based on the given measurements for one location.
#'
#' @param var_width width of the river at the sample location
#' @param var_slope slope of the river at the sample location
#' @param var_tidal tidal effect present (TRUE) or absent (FALSE)?
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
determine_guild <- function(var_width, var_slope, var_tidal) {

  data_guild <-
    read_csv2(system.file("extdata/data_guild.csv", package = "EQRfishes"))
  data_guild %<>%
    filter(
      .data$tidal == as.logical(var_tidal),
      var_in_interval(var_width, .data$width),
      var_in_interval(var_slope, .data$slope)
    )

  return(data_guild$guild)
}
