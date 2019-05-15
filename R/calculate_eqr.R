#' calculate the ecological quality ratio
#'
#' Main function of this package, which calculates the EQR based on 2 tables of data.  Each table must contain a sample_key!
#'
#' @param data_sample Data on the sample: date, method, location and location characteristics
#' @param data_fish Measurements on fish: taxon, length, weight, number of individuals
#'
#' @return Dataset with calculated EQR for each sample
#'
#' @importFrom dplyr group_by left_join mutate rowwise ungroup
#' @importFrom plyr .
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom readr read_csv2
#' @importFrom purrr map
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
        )
    ) %>%
    ungroup()

  data_fish %<>%
    group_by(.data$sample_key) %>%
    nest(.key = "fishdata")

  result <- data_sample %>%
    left_join(
      read_csv2(
        system.file("extdata/guild_metric.csv", package = "EQRfishes")
      ),
      by = "guild"
    ) %>%
    left_join(
      data_fish,
      by = "sample_key"
    ) %>%
    mutate(
      metric_results = calculate_metric(.),
      submetric_value = unlist(map(.data$metric_results, unlist_value)),
      submetric_score = unlist(map(.data$metric_results, unlist_score))
    )

  return(result)
}
