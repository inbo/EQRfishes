#' calculate the ecological quality ratio
#'
#' Main function of this package, which calculates the EQR based on 2 tables of data.  Each table must contain a sample_key!
#'
#' @param data_sample Data on the sample: date, method, location and location characteristics
#' @param data_fish Measurements on fish: taxon, length, weight, number of individuals
#'
#' @return Dataset with calculated EQR for each sample
#'
#' @importFrom dplyr mutate rowwise
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#'
#' @export
#'
calculate_eqr <- function(data_sample, data_fish) {

  data_sample %<>%
    rowwise() %>%
    mutate(
      guild =
        determine_guild(
          .data$width_river, .data$slope, .data$tidal, .data$Bekken
        ),
      surface =
        ifelse(
          .data$method %in% c("PF", "SF"),
          .data$n_fyke_nets ^ 2 * 20,
          ifelse(
            .data$length_trajectory > 0,
            ifelse(
              !is.na(.data$width_transect) & .data$width_transect > 0,
              .data$width_transect * .data$length_trajectory,
              .data$width_river * .data$length_trajectory
            ),
            0
          )
        ),
      surface =
        ifelse(
          .data$method == "SF",
          .data$surface * 4,
          .data$surface
        ),
    )

  result <- calculate_metrics(data_sample, data_fish)

  return(result)
}
