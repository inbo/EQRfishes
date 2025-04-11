#' calculate the ecological quality ratio
#'
#' Main function of this package, which calculates the EQR based on 2 tables of
#' data.
#' Each table must contain a sample_key!
#'
#' @param data_sample Data on the sample: date, method, location and location
#' characteristics including zonation (= indextypology, which can be calculated
#' using function `determine_zonation()`)
#' @param data_fish Measurements on fish: dataframe with columns
#'   - `sample_key` (reference to `data_sample`),
#'   - `record_id`: unique id for each row,
#'   - `taxoncode`: abbreviation of scientific fish name,
#'   - `number` of individuals for this record (1 if each separate fish is
#'     measured),
#'   - `length` of the fish in cm (NA if `number` > 1),
#'   - `weight` of the fish in g (total weight if `number` > 1)
#' @param output Which output do you wish?
#'   \itemize{
#'     \item \strong{EQR} (default) only gives the main result:
#'       a dataframe with calculated IBI and EQR for each sample (location),
#'     \item \strong{metric} gives a list of 2 dataframes:
#'       one with the IBI and EQR, and one with the results for each metric,
#'     \item \strong{detail} gives a list of 3 dataframes:
#'       one with the IBI and EQR, a second with the results for each metric
#'       and a third with all calculated values
#'   }
#' @param cluster Table with columns `sample_key` and `index_cluster`
#' that indicates how samples should be clustered into a 'waterbody'
#' in case of the indextypologies lakes, canals and estuarine zonations.
#' Defaults to NA, because it is not needed in case of freshwater rivers.
#'
#' @return Dataframe with calculated EQR for each sample, or list of dataframes
#' if parameter output is specified
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr arrange distinct filter group_by left_join mutate n select
#'   summarise ungroup
#' @importFrom plyr .
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr gather nest unnest
#' @importFrom readr read_csv2
#' @importFrom purrr pmap
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @examples
#' library(EQRfishes)
#' data_sample <- read.csv2(
#'   system.file("testdata/kallemoeie_sample.csv", package = "EQRfishes")
#' )
#' data_fish <- read.csv2(
#'   system.file("testdata/kallemoeie_fish_data.csv", package = "EQRfishes")
#' )
#' cluster <- data.frame(
#'   sample_key = c("Kallemoeie_e", "Kallemoeie_f"),
#'   index_cluster = "Kallemoeie"
#' )
#'
#' calculate_eqr(data_sample, data_fish, cluster = cluster)
#' calculate_eqr(data_sample, data_fish, output = "metric", cluster = cluster)
#'
calculate_eqr <- function(
  data_sample, data_fish, output = c("EQR", "metric", "detail"), cluster = NA
) {

  # make sure data_fish has all columns present, and remove additional columns
  # (these can cause problems if fish data are nested)
  assert_that(has_name(data_fish, "sample_key"))
  assert_that(has_name(data_fish, "record_id"))
  assert_that(has_name(data_fish, "taxoncode"))
  assert_that(has_name(data_fish, "number"))
  assert_that(has_name(data_fish, "length"))
  assert_that(has_name(data_fish, "weight"))
  data_fish <- data_fish %>%
    select("sample_key", "record_id", "taxoncode", "number", "length", "weight")

  match.arg(output)

  join_data_fish <- "sample_key"
  select_keys <- "sample_key"
  if (any(str_detect(data_sample$zonation, "estuarien|lakes|canals"))) {
    if (length(cluster) == 1 && is.na(cluster)) {
      stop("Argument cluster must be provided for indextypologies lakes, canals or estuarien") # nolint: line_length_linter
    }
    assert_that(has_name(cluster, c("sample_key", "index_cluster")))
    data_sample <- data_sample %>%
      left_join(
        cluster,
        by = "sample_key"
      ) %>%
      mutate(
        sample_key_replace =
          ifelse(
            is.na(.data$index_cluster),
            .data$sample_key,
            paste(
              .data$index_cluster,
              #paste method 2 times to end with at least 2 characters
              substr(paste0(.data$method, .data$method), 1, 2),
              sep = "_"
            )
          ),
        method =
          ifelse(str_detect(.data$method, "^E"), "E", .data$method),
        method =
          ifelse(str_detect(.data$method, "^SF"), "SF", .data$method)
      )
    test_index_cluster <- data_sample %>%
      filter(
        is.na(.data$index_cluster),
        str_detect(.data$zonation, "estuarien|lakes|canals")
      )
    if (nrow(test_index_cluster) > 0) {
      stop(
        sprintf(
          "Argument cluster must contain all sample keys with indextypologies lakes, canals or estuarien (not provided for sample_key %s)", # nolint: line_length_linter
          paste(unique(test_index_cluster$sample_key), collapse = ", ")
        )
      )
    }
    data_fish <- data_fish %>%
      inner_join(
        data_sample %>%
          select(
            "sample_key_replace", "method", "sample_key"
          ) %>%
          distinct(),
        by = "sample_key"
      ) %>%
      mutate(
        sample_key = .data$sample_key_replace,
        sample_key_replace = NULL
      )
    join_data_fish <- c("sample_key", "method")
    select_keys <- c("sample_key", "index_cluster")
    data_sample <- data_sample %>%
      mutate(
        sample_key = .data$sample_key_replace,
        LocationID =
          ifelse(
            is.na(.data$index_cluster),
            .data$LocationID,
            NA
          )
      ) %>%
      group_by(
        .data$sample_key, .data$LocationID, .data$method,
        .data$Stilstaand, .data$tidal, .data$Brak, .data$IndexTypeCode,
        .data$year, .data$zonation
      ) %>%
      summarise(
        surface = sum(.data$width_transect * .data$length_trajectory),
        length_trajectory = sum(.data$length_trajectory),
        n_fyke_days = sum(.data$n_fyke_nets * .data$n_days),
        # calculate n_fyke_days on original n_fyke_nets before aggregating
        # n_fyke_nets
        n_fyke_nets = sum(.data$n_fyke_nets),
        width_river = mean(.data$width_river),
        slope = mean(.data$slope)
      ) %>%
      ungroup() %>%
      group_by(
        .data$sample_key, .data$LocationID, .data$method,
        .data$Stilstaand, .data$tidal, .data$Brak, .data$IndexTypeCode,
        .data$year, .data$zonation  #group by year
      ) %>%
      summarise(
        surface = sum(.data$surface),
        length_trajectory = sum(.data$length_trajectory),
        n_fyke_nets = mean(.data$n_fyke_nets),
        n_fyke_days = sum(.data$n_fyke_days),
        width_river = mean(.data$width_river),
        slope = mean(.data$slope)
      ) %>%
      ungroup() %>%
      mutate(
        width_transect = .data$surface / .data$length_trajectory,
        surface = NULL,
        n_days = .data$n_fyke_days / .data$n_fyke_nets,
        n_fyke_days = NULL
      )
  }

  zero_width <- data_sample %>%
    filter(
      .data$zonation %in% c("barbeel", "brasem", "brabeel"),
      .data$width_river == 0
    )
  if (nrow(zero_width) > 0) {
    warning(
      "Some records of data_sample from zonation barbeel, brasem or brabeel have a width_river of 0. Scoring of metrics is done supposing the river width is smaller than 3 meters. Please redo the calculation with a valid river width if the river is 3 meter or wider." # nolint: line_length_linter
    )
  }

  data_sample <- data_sample %>%
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
      -"sample_key", -"zonation", -"LocationID", -"method", -"year"
    ) %>%
    nest(sampledata = c("name", "value"))

  if (any(is.na(data_fish$taxoncode))) {
    warning(
      "For some records of data_fish, no taxoncode is given (value is NA). These will be excluded from the analysis." # nolint: line_length_linter
    )
  }
  data_taxonmetrics <-
    suppressMessages(
      read_csv2(
        system.file("extdata/data_taxonmetrics.csv", package = "EQRfishes")
      )
    )
  no_fish <- data_fish %>%
    filter(!.data$taxoncode %in% data_taxonmetrics$taxoncode) %>%
    distinct(.data$taxoncode)
  if (nrow(no_fish) > 0) {
    warning(
      paste(
        "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis: ", # nolint: line_length_linter
        paste(no_fish$taxoncode, collapse = ", ")
      )
    )
  }

  fish_not_measured <- data_fish %>%
    filter(is.na(.data$number) & is.na(.data$length) & is.na(.data$weight))
  if (nrow(fish_not_measured) > 0) {
    stop(
      "Some fishes don't have any data (no number of fishes, no fish length and no weight). Please indicate at least the number of fishes or remove the record(s)." # nolint: line_length_linter
    )
  }
  rm(fish_not_measured)

  fish_zero_number <- data_fish %>%
    filter(.data$number <= 0)
  if (nrow(fish_zero_number) > 0) {
    stop(
      "For some fishdata, the number of fishes is zero or below. Please give a positive integer for the number of fishes (or remove the record(s))." # nolint: line_lenght_linter
    )
  }
  rm(fish_zero_number)

  data_fish <- data_fish %>%
    filter(
      !is.na(.data$taxoncode),
      !.data$taxoncode %in% no_fish$taxoncode
    ) %>%
    mutate(sample_key = as.character(.data$sample_key)) %>%
    nest(
      fishdata =
        c("record_id", "taxoncode", "number", "length",
          "weight")
    )
  rm(no_fish)

  result <- data_sample %>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/zonation_metric.csv", package = "EQRfishes")
        )
      ) %>%
        nest(
          metric_name_group =
            c("metric_formula_name", "metric_measures_name")
        ),
      by = "zonation",
      suffix = c("", "_for_metric"), relationship = "many-to-many"
    ) %>%
    filter(
      is.na(.data$method_for_metric) |
        str_detect(.data$method, .data$method_for_metric)
    ) %>%
    left_join(
      data_fish,
      by = join_data_fish
    ) %>%
    mutate(
      row_id = seq_along(.data$sample_key)
    ) %>%
    arrange(.data$row_id) %>%
    mutate(
      sampledata = calculate_metric(., specieslist = data_taxonmetrics)
    )
  rm(data_taxonmetrics)

  result_details <- result %>%
    select(
      "sample_key", "zonation", "LocationID", "year", "sampledata"
    ) %>%
    unnest(cols = c("sampledata")) %>%
    distinct()

  result_metrics <- result %>%
    select(
      "sample_key", "zonation", "LocationID", "year",
      "sampledata", "metric_name", "metric_score_name",
      "method_for_metric", "metric_name_group"
    ) %>%
    unnest(cols = c("metric_name_group")) %>%
    mutate(
      metric_name_ext =
        ifelse(is.na(.data$metric_formula_name), .data$metric_measures_name,
               .data$metric_formula_name),
      metric_formula_name = NULL,
      metric_measures_name = NULL
    ) %>%
    unnest(cols = c("sampledata")) %>%
    mutate(
      metric_value =
        ifelse(
          .data$name == .data$metric_name_ext,
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
      .data$sample_key, .data$zonation, .data$LocationID, .data$year,
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
    filter(str_detect(.data$zonation, "estuarien|lakes|canals")) %>%
    mutate(
      sample_key = substr(.data$sample_key, 1, nchar(.data$sample_key) - 3)
    ) %>%
    group_by(
      .data$sample_key, .data$zonation, .data$LocationID, .data$year,
      .data$metric_name, .data$metric_score_name, .data$method_for_metric
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

  # for some (/Schelde) estuariene indices, the metric scores should be 0
  # if MnsTot = 0
  # calculate MnsTot 6 times to accomplish this using the tables, would probably make the calculations too slow
  result_metrics_aggregated <- result_metrics_aggregated %>%
    left_join(
      result_metrics_aggregated %>%
        filter(
          .data$zonation %in%
            c("estuarien_Schelde_oligohaline", "estuarien_Schelde_mesohaline",
              "estuarien_Schelde_freshwater"),
          .data$metric_name == "MnsTot"
        ) %>%
        select(
          "sample_key", "zonation", "LocationID", "year",
          "MnsTot" = .data$metric_value
        ),
      by = c("sample_key", "zonation", "LocationID", "year")
    ) %>%
    mutate(
      metric_score = ifelse(
        !is.na(.data$MnsTot) & .data$MnsTot == 0, "0", .data$metric_score
      ),
      MnsTot = NULL,
      index_cluster = .data$sample_key,
      sample_key = NULL
    )

  if (nrow(result_metrics_aggregated) > 0) {
    result_metrics <- result_metrics %>%
      filter(!str_detect(.data$zonation, "estuarien|lakes|canals")) %>%
      bind_rows(result_metrics_aggregated)
  }

  eqr_scores <-
    suppressMessages(
      read_csv2(system.file("extdata/score.csv", package = "EQRfishes"))
    )

  result_eqr <- result_metrics %>%
    mutate(
      calc_method_old =
        .data$zonation %in% c("brasem", "barbeel", "upstream", "forel",
                              "vlagzalm")
    ) %>%
    nest(
      metrics =
        c("metric_name", "metric_score_name", "method_for_metric",
          "metric_name_ext", "metric_value", "metric_score")
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
      calc_method_old =
        ifelse(
          grepl("estuarien", .data$zonation) &
            !.data$zonation == "estuarien_zijrivieren_zoet",
          TRUE,
          .data$calc_method_old
        ),
      std_ibi =
        unlist(
          pmap(
            list(
              .data$ibi, .data$metrics, .data$calc_method_old, .data$zonation
            ),
            standardise_ibi
          )
        ),
      calc_method_old =
        ifelse(
          grepl("estuarien", .data$zonation) &
            !.data$zonation == "estuarien_zijrivieren",
          FALSE,
          .data$calc_method_old
        )
    ) %>%
    select(-"metrics") %>%
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
      eqr_class =
        ifelse(
          .data$zonation %in% c("lakes", "brabeel"),
          cut(
            .data$std_ibi,
            breaks =
              c(0, eqr_scores$std_ibi_newst[!is.na(eqr_scores$std_ibi_newst)]),
            labels = eqr_scores$EQR_class[!is.na(eqr_scores$std_ibi_newst)],
            right = FALSE
          ),
          .data$eqr_class
        ),
      eqr_class =
        ifelse(
          .data$zonation %in%
            c("estuarien_Schelde_freshwater", "estuarien_Schelde_oligohaline",
              "estuarien_Schelde_mesohaline"),
          cut(
            .data$std_ibi,
            breaks =
              c(0, eqr_scores$ibi_estuarien[!is.na(eqr_scores$ibi_estuarien)]),
            labels = eqr_scores$EQR_class[!is.na(eqr_scores$ibi_estuarien)],
            right = FALSE
          ),
          .data$eqr_class
        ),
      eqr_class =
        ifelse(
          .data$zonation == "estuarien_IJzer",
          cut(
            .data$std_ibi,
            breaks =
              c(0, eqr_scores$ibi_ijzer[!is.na(eqr_scores$ibi_ijzer)]),
            labels = eqr_scores$EQR_class[!is.na(eqr_scores$ibi_ijzer)],
            right = FALSE
          ),
          .data$eqr_class
        ),
      eqr_class = as.numeric(as.character(.data$eqr_class)),
      ibi_classmin =
        cut(
          .data$std_ibi,
          breaks = c(0, eqr_scores$std_ibi_new[!is.na(eqr_scores$std_ibi_new)]),
          labels = c(
            0,
            eqr_scores$std_ibi_new[
              !is.na(eqr_scores$std_ibi_new)
            ][1:(sum(!is.na(eqr_scores$std_ibi_new)) - 1)]
          ),
          right = FALSE
        ),
      ibi_classmin = as.numeric(as.character(.data$ibi_classmin)),
      ibi_classmin =
        ifelse(
          .data$calc_method_old,
          as.numeric(
            as.character(
              cut(
                .data$std_ibi,
                breaks =
                  c(0, eqr_scores$std_ibi_old[!is.na(eqr_scores$std_ibi_old)]),
                labels =
                  c(
                    0,
                    eqr_scores$std_ibi_old[
                      !is.na(eqr_scores$std_ibi_old)
                    ][1:(sum(!is.na(eqr_scores$std_ibi_old)) - 1)]
                  ),
                right = FALSE
              )
            )
          ),
          .data$ibi_classmin
        ),
      ibi_classmin =
        ifelse(
          grepl("estuarien_Schelde", .data$zonation),
          as.numeric(
            as.character(
              cut(
                .data$std_ibi,
                breaks =
                  c(
                    0,
                    eqr_scores$ibi_estuarien[!is.na(eqr_scores$ibi_estuarien)]
                  ),
                labels =
                  c(
                    0,
                    eqr_scores$ibi_estuarien[
                      !is.na(eqr_scores$ibi_estuarien)
                    ][1:(sum(!is.na(eqr_scores$ibi_estuarien)) - 1)]
                  ),
                right = FALSE
              )
            )
          ),
          .data$ibi_classmin
        ),
      ibi_classmin =
        ifelse(
          .data$zonation == "estuarien_IJzer",
          as.numeric(
            as.character(
              cut(
                .data$std_ibi,
                breaks =
                  c(0, eqr_scores$ibi_ijzer[!is.na(eqr_scores$ibi_ijzer)]),
                labels =
                  c(0,
                    eqr_scores$ibi_ijzer[
                      !is.na(eqr_scores$ibi_ijzer)
                    ][1:(sum(!is.na(eqr_scores$ibi_ijzer)) - 1)]
                  ),
                right = FALSE
              )
            )
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
          as.numeric(
            as.character(
              cut(
                .data$std_ibi,
                breaks =
                  c(0, eqr_scores$std_ibi_old[!is.na(eqr_scores$std_ibi_old)]),
                labels = eqr_scores$std_ibi_old[!is.na(eqr_scores$std_ibi_old)],
                right = FALSE
              )
            )
          ),
          .data$ibi_classmax
        ),
      ibi_classmax =
        ifelse(
          grepl("estuarien_Schelde", .data$zonation),
          as.numeric(
            as.character(
              cut(
                .data$ibi,
                breaks = c(
                  0, eqr_scores$ibi_estuarien[!is.na(eqr_scores$ibi_estuarien)]
                ),
                labels =
                  eqr_scores$ibi_estuarien[!is.na(eqr_scores$ibi_estuarien)],
                right = FALSE
              )
            )
          ),
          .data$ibi_classmax
        ),
      ibi_classmax =
        ifelse(
          .data$zonation == "estuarien_IJzer",
          as.numeric(
            as.character(
              cut(
                .data$ibi,
                breaks =
                  c(0, eqr_scores$ibi_ijzer[!is.na(eqr_scores$ibi_ijzer)]),
                labels = eqr_scores$ibi_ijzer[!is.na(eqr_scores$ibi_ijzer)],
                right = FALSE
              )
            )
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
        (.data$nclass * (.data$ibi_classmax - .data$ibi_classmin)),
      eqr =
        ifelse(
          .data$zonation %in%
            c("brabeel", "lakes", "bron", "estuarien_zijrivieren_zoet"),
          .data$std_ibi, .data$eqr
        ),
      eqr =
        ifelse(
          .data$zonation == "bron" & .data$ibi == 4,
          0.2,
          .data$eqr
        ),
      eqr =
        ifelse(
          .data$zonation == "estuarien_zijrivieren_zoet" & .data$ibi == 0.8,
          0.05,
          .data$eqr
        )
    ) %>%
    select(
      select_keys, "zonation", "LocationID", "year", "calc_method_old",
      "ibi", "eqr_class", "eqr"
    ) %>%
    left_join(
      eqr_scores %>%
        select(
          -"std_ibi_old", -"std_ibi_new", -"std_ibi_newst", -"ibi_ijzer",
          -"ibi_estuarien"
        ),
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
