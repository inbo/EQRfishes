#' calculate the ecological quality ratio
#'
#' Main function of this package, which calculates the EQR based on 2 tables of data.  Each table must contain a sample_key!
#'
#' @param data_sample Data on the sample: date, method, location and location
#' characteristics including zonation (which can be calculated using function
#' `determine_zonation()`)
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
#' @importFrom dplyr arrange distinct filter group_by left_join mutate n select summarise ungroup
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
    mutate(
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
      surface = as.character(.data$surface),
      n_fyke_days = as.character(.data$n_fyke_nets * .data$n_days),
      sample_key = as.character(.data$sample_key),
      width_transect = as.character(.data$width_transect),
      length_trajectory = as.character(.data$length_trajectory),
      width_river = as.character(.data$width_river),
      slope = as.character(.data$slope),
      n_fyke_nets = as.character(.data$n_fyke_nets),
      n_days = as.character(.data$n_days),
      Stilstaand = as.character(.data$Stilstaand),
      tidal = as.character(.data$tidal),
      Brak = as.character(.data$Brak)
    ) %>%
    gather(
      key = "name", value = "value",
      -.data$sample_key, -.data$zonation, -.data$LocationID, -.data$method
    ) %>%
    nest(sampledata = c(.data$name, .data$value))

  if (any(is.na(data_fish$taxoncode))) {
    warning("For some records of data_fish, no taxoncode is given (value is NA). These will be excluded from the analysis.")
  }
  data_fish %<>%
    filter(!is.na(.data$taxoncode)) %>%
    mutate(sample_key = as.character(.data$sample_key)) %>%
    nest(
      fishdata =
        c(.data$record_id, .data$taxoncode, .data$number, .data$length,
          .data$weight)
    )

  result <- data_sample %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/zonation_metric.csv", package = "EQRfishes")
        )
      ) %>%
        nest(
          metric_name_group =
            c(.data$metric_formula_name, .data$metric_measures_name)
        ),
      by = "zonation",
      suffix = c("", "_for_metric")
    ) %>%
    filter(
      is.na(.data$method_for_metric) |
        str_detect(.data$method, .data$method_for_metric)
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
    select(
      .data$sample_key, .data$zonation, .data$LocationID, .data$sampledata
    ) %>%
    unnest(cols = c(.data$sampledata)) %>%
    distinct()

  result_metrics <- result %>%
    select(
      .data$sample_key, .data$zonation, .data$LocationID,
      .data$sampledata, .data$metric_name, .data$metric_score_name,
      .data$method_for_metric
    ) %>%
    unnest(cols = c(.data$sampledata)) %>%
    mutate(
      metric_name_ext =
        ifelse(
          str_detect(.data$name, paste0("^", .data$metric_name)),
          .data$name,
          NA
        ),
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
      .data$sample_key, .data$zonation, .data$LocationID,
      .data$metric_name, .data$metric_score_name, .data$method_for_metric
    ) %>%
    summarise(
      metric_name_ext =
        ifelse(
          all(is.na(.data$metric_name_ext)),
          as.character(NA),
          max(.data$metric_name_ext, na.rm = TRUE)
        ),
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

  #for the new method (estuaries, lakes and canals), results are aggregated
  result_metrics_aggregated <- result_metrics %>%
    filter(!is.na(.data$method_for_metric)) %>%
    mutate(
      sample_key = substr(.data$sample_key, 1, nchar(.data$sample_key) - 10)
    ) %>%
    group_by(
      .data$sample_key, .data$zonation, .data$LocationID,
      .data$metric_name, .data$metric_score_name
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
    )

  result_metrics %<>%
    filter(is.na(.data$method_for_metric)) %>%
    bind_rows(result_metrics_aggregated)

  eqr_scores <-
    suppressMessages(
      read_csv2(system.file("extdata/score.csv", package = "EQRfishes"))
    )

  result_eqr <- result_metrics %>%
    group_by(.data$zonation, .data$LocationID) %>%
    mutate(calc_method_old = all(is.na(.data$method_for_metric))) %>%
    ungroup() %>%
    mutate() %>%
    nest(
      metrics =
        c(.data$metric_name, .data$metric_score_name, .data$method_for_metric,
          .data$metric_name_ext, .data$metric_value, .data$metric_score)
    ) %>%
    mutate(
      ibi =
        as.numeric(
          unlist(
            pmap(
              list(.data$zonation, .data$metrics, .data$calc_method_old),
              calculate_ibi_score
            )
          )
        ),
      std_ibi =
        unlist(
          pmap(
            list(.data$ibi, .data$metrics, .data$calc_method_old),
            standardise_ibi
          )
        )
    ) %>%
    select(-.data$metrics) %>%
    mutate(
      eqr_class =
        cut(
          .data$std_ibi,
          breaks = c(0, eqr_scores$std_ibi_new[!is.na(eqr_scores$std_ibi_new)]),
          labels = eqr_scores$EQR_class[!is.na(eqr_scores$std_ibi_new)],
          right = FALSE
        ),
      eqr_class =
        ifelse(
          .data$calc_method_old,
          cut(
            .data$std_ibi,
            breaks =
              c(0, eqr_scores$std_ibi_old[!is.na(eqr_scores$std_ibi_old)]),
            labels = eqr_scores$EQR_class[!is.na(eqr_scores$std_ibi_old)],
            right = FALSE
          ),
          .data$eqr_class
        ),
      eqr_class = as.numeric(as.character(.data$eqr_class)),
      ibi_classmin =
        cut(
          .data$std_ibi,
          breaks = c(0, eqr_scores$std_ibi_new[!is.na(eqr_scores$std_ibi_new)]),
          labels =
            c(0,
              eqr_scores$std_ibi_new[
                !is.na(eqr_scores$std_ibi_new)
              ][1:(sum(!is.na(eqr_scores$std_ibi_new)) - 1)]),
          right = FALSE
        ),
      ibi_classmin = as.numeric(as.character(.data$ibi_classmin)),
      ibi_classmin =
        ifelse(
          .data$calc_method_old,
          as.numeric(as.character(
            cut(
              .data$std_ibi,
              breaks =
                c(0, eqr_scores$std_ibi_old[!is.na(eqr_scores$std_ibi_old)]),
              labels =
                c(0,
                  eqr_scores$std_ibi_old[
                    !is.na(eqr_scores$std_ibi_old)
                  ][1:(sum(!is.na(eqr_scores$std_ibi_old)) - 1)]),
              right = FALSE
            ))
          ),
          .data$ibi_classmin
        ),
      ibi_classmax =
        cut(
          .data$std_ibi,
          breaks = c(0, eqr_scores$std_ibi_new[!is.na(eqr_scores$std_ibi_new)]),
          labels = eqr_scores$std_ibi_new[!is.na(eqr_scores$std_ibi_new)],
          right = FALSE
        ),
      ibi_classmax = as.numeric(as.character(.data$ibi_classmax)),
      ibi_classmax =
        ifelse(
          .data$calc_method_old,
          as.numeric(as.character(
            cut(
              .data$std_ibi,
              breaks =
                c(0, eqr_scores$std_ibi_old[!is.na(eqr_scores$std_ibi_old)]),
              labels = eqr_scores$std_ibi_old[!is.na(eqr_scores$std_ibi_old)],
              right = FALSE
            ))
          ),
          .data$ibi_classmax
        ),
      nclass = sum(!is.na(eqr_scores$std_ibi_new)),
      nclass =
        ifelse(
          .data$calc_method_old,
          sum(!is.na(eqr_scores$std_ibi_old)),
          .data$nclass
        ),
      eqr =
        (.data$eqr_class - 1) / .data$nclass +
        (.data$std_ibi - .data$ibi_classmin) /
        (.data$nclass * (.data$ibi_classmax - .data$ibi_classmin))
    ) %>%
    select(
      .data$sample_key, .data$zonation, .data$LocationID, .data$calc_method_old,
      .data$ibi, .data$eqr_class, .data$eqr
    ) %>%
    left_join(
      eqr_scores %>%
        select(-.data$std_ibi_old, -.data$std_ibi_new),
      by = c("eqr_class" = "EQR_class")
    )

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
