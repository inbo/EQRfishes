#' calculate the ecological quality ratio
#'
#' Main function of this package, which calculates the EQR based on 2 tables of data.  Each table must contain a sample_key!
#'
#' @param data_sample Data on the sample: date, method, location and location characteristics
#' @param data_fish Measurements on fish: taxon, length, weight, number of individuals
#' @param output Which output do you wish?
#'   \itemize{
#'     \item \strong{EQR} (default) only gives the main result: a dataframe with calculated IBI and EQR for each sample (location),
#'     \item \strong{metric} gives a list of 2 dataframes: one with the IBI and EQR, and one with the results for each metric,
#'     \item \strong{detail} gives a list of 3 dataframes: one with the IBI and EQR, a second with the results for each metric and a third with all calculated values
#'   }
#'
#' @return Dataframe with calculated EQR for each sample, or list of dataframes if parameter output is specified
#'
#' @importFrom dplyr arrange distinct group_by left_join mutate n rowwise select summarise ungroup
#' @importFrom plyr .
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang .data
#' @importFrom tidyr gather nest unnest
#' @importFrom readr read_csv2
#' @importFrom purrr pmap
#' @importFrom stringr str_detect
#'
#' @export
#'
calculate_eqr <-
  function(data_sample, data_fish, output = c("EQR", "metric", "detail")) {

  data_sample %<>%
    rowwise() %>%
    mutate(
      guild =
        determine_zonation(
          .data$width_river, .data$slope, .data$tidal, .data$IndexTypeCode,
          .data$method
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
    ungroup() %>%
    gather(key = "name", value = "value", -.data$sample_key, -.data$guild) %>%
    group_by(.data$sample_key, .data$guild) %>%
    nest(.key = "sampledata")

  data_fish %<>%
    group_by(.data$sample_key) %>%
    nest(.key = "fishdata")

  result <- data_sample %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/guild_metric.csv", package = "EQRfishes")
        )
      ) %>%
        group_by(.data$guild, .data$metric_name, .data$metric_score_name) %>%
        nest(.key = "metric_name_group"),
      by = "guild"
    ) %>%
    left_join(
      data_fish,
      by = "sample_key"
    ) %>%
    mutate(
      row_id = 1:length(.data$sample_key)
    ) %>%
    arrange(.data$row_id) %>%
    mutate(
      sampledata = calculate_metric(.)
    )

  result_details <- result %>%
    select(.data$sample_key, .data$guild, .data$sampledata) %>%
    unnest() %>%
    distinct()

  result_metrics <- result %>%
    select(
      .data$sample_key, .data$guild, .data$sampledata, .data$metric_name,
      .data$metric_score_name
    ) %>%
    unnest() %>%
    mutate(
      metric_value =
        ifelse(
          str_detect(.data$name, paste0("^", .data$metric_name)),
          .data$value,
          NA
        ),
      metric_score =
        ifelse(
          .data$metric_score_name == .data$name,
          .data$value,
          NA
        )
    ) %>%
    group_by(
      .data$sample_key, .data$guild, .data$metric_name, .data$metric_score_name
    ) %>%
    summarise(
      metric_value =
        ifelse(
          all(is.na(.data$metric_value)),
          as.character(NA),
          max(.data$metric_value, na.rm = TRUE)
        ),
      metric_score =
        ifelse(
          all(is.na(.data$metric_score)),
          as.character(NA),
          max(.data$metric_score, na.rm = TRUE)
        )
    ) %>%
    ungroup()

  result_eqr <- result_metrics %>%
    group_by(.data$sample_key, .data$guild) %>%
    nest(.key = "metrics") %>%
    mutate(
      IBI =
        unlist(
          pmap(
            list(.data$guild, .data$metrics),
            calculate_ibi_score
          )
        ),
      EQR_interval =
        pmap(
          list(.data$guild, .data$IBI),
          calculate_eqr_score
        )
    ) %>%
    select(-.data$metrics) %>%
    unnest() %>%
    left_join(
      suppressMessages(
        read_csv2(system.file("extdata/score.csv", package = "EQRfishes"))
      ) %>%
        select(-.data$score_id),
      by = c("interval_IBI" = "IBI")
    ) %>%
    select(-.data$interval_IBI)

  if (output[[1]] == "EQR") {
    return(result_eqr)
  }
  if (output == "metric") {
    return(
      list(eqr = result_eqr, metric = result_metrics)
    )
  }
  if (output == "detail") {
    return(
      list(eqr = result_eqr, metric = result_metrics, details = result_details)
    )
  }
}
