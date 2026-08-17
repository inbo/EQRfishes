#' calculate the Ecological Quality Ratio
#'
#' Main function of this package, which calculates the Ecological Quality Ratio
#' (EQR) based on 2 tables of data.
#' Each table must contain a `sample_id`!
#'
#' @param data_sample Data on the sample with at least columns:
#'   \itemize{
#'     \item `sample_id`: unique id for each row, that should be referred to in
#'     `data_fish` (and `cluster`),
#'     \item `indextypology`: index that should be used for the calculation,
#'       which can be calculated using function `determine_indextypology()`.
#'       Possible values are `fish-based bream IBI` (or Dutch `brasem index`),
#'       `fish-based barbel IBI` (or Dutch `barbeel index`),
#'       `fish-based lowland IBI`
#'         (or Dutch `brasem & barbeel index versie 2021`),
#'       `fish-based upstream IBI` (or Dutch `bovenstroomse index`),
#'       `fish-based trout IBI` (or Dutch `forel index`),
#'       `fish-based grayling IBI` (or Dutch `vlagzalm index`),
#'       `fish-based river source IBI` (or Dutch `bron index`),
#'       `fish-based lakes IBI` (or Dutch `meren index`),
#'       `fish-based canals IBI` (or Dutch `kanaal index`),
#'       `fish-based IJzer estuarine index` (or Dutch `IJzer estuariene index`),
#'       `fish-based estuarine Schelde freshwater index`
#'         (or Dutch `estuariene Schelde zoetwater index`),
#'       `fish-based estuarine Schelde oligohaline index`
#'         (or Dutch `estuariene Schelde oligohaliene index`),
#'       `fish-based estuarine Schelde mesohaline index`
#'         (or Dutch `estuariene Schelde mesohaliene index`) and
#'       `fish-based estuarine tribuataries river Schelde index`
#'         (or Dutch `getijgebonden zijrivieren zoet index`).
#'       (Also Dutch names and abbreviations are accepted.)
#'     \item `method`: method used for the sampling, possible values are
#'      `E` (electrofishing), `SF` (fyke fishing) and `SN` (trawling)
#'      (or methods starting with `E` or `SF`).
#'      There should be a separate row (and different `sample_id`) for each
#'      method, so measures taken with different methods on the same spot
#'      are considered to be different samples.
#'     \item `year`: year of sampling
#'   }
#' For some indextypologies, additional columns are needed:
#'   \itemize{
#'     \item `width_transect` and `length_trajectory` in meter for
#'       electrofishing in `fish-based bream IBI`, `fish-based barbel IBI`,
#'       `fish-based upstream IBI` and `fish-based canals IBI`
#'     \item `width_river` in meter for `fish-based bream IBI`,
#'       `fish-based barbel IBI` and `fish-based lowland IBI`
#'     \item `slope` in per thousand for `fish-based upstream IBI`
#'     \item `n_fyke_nets` (number of nets) for fyke fishing in
#'       `fish-based bream IBI`, `fish-based barbel IBI`,
#'       `fish-based estuarine Schelde freshwater index` and
#'       `fish-based estuarine Schelde oligohaline index`
#'     \item `n_days` (number of days that the given number of nets was placed)
#'       for fyke fishing in `fish-based estuarine Schelde freshwater index` and
#'       `fish-based estuarine Schelde oligohaline index`
#'     \item `season` in which the sampling took place, possible values are
#'       `spring`, `summer` or `autumn` (samples in `winter` are not scored),
#'       only needed for `fish-based lowland IBI`
#'   }
#' @param data_fish Measurements on fish: dataframe with columns
#'   - `sample_id` (reference to `data_sample`),
#'   - `record_id`: unique id for each row,
#'   - `taxoncode`: abbreviation of scientific fish name,
#'   - `number` of individuals for this record (1 if each separate fish is
#'     measured),
#'   - `length` of the fish in centimeters (should be `NA` (not available) if
#'   `number` > 1),
#'   - `weight` of the fish in gram (total weight of all fish if `number` > 1)
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
#' @param cluster Table with columns `sample_id` and `index_cluster`
#' that indicates how samples should be clustered into a 'waterbody'
#' in case of the index typologies lakes, canals and estuarine zonations.
#' Defaults to `NA` (not available), because this input is not needed in case of
#' freshwater rivers.
#'
#' @return Dataframe with calculated EQR for each sample, or list of dataframes
#' if parameter output is specified
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr across all_of arrange distinct filter group_by left_join
#'   mutate n select summarise ungroup
#' @importFrom plyr .
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr gather nest pivot_longer pivot_wider unnest
#' @importFrom readr read_csv2
#' @importFrom purrr pmap
#' @importFrom stringr str_detect
#'
#' @export
#' @family main
#'
#' @examples
#' library(dplyr)
#' library(EQRfishes)
#' # freshwater river indexes (lowland IBI, upstream,...)
#' data_sample <- read.csv2(
#'   system.file("testdata/freshwater_sample.csv", package = "EQRfishes")
#' )
#' data_fish <- read.csv2(
#'   system.file("testdata/freshwater_fish_data.csv", package = "EQRfishes")
#' )
#' # add information on index that should be calculated
#' index_info <- data.frame(
#'   order_id = 1:12,
#'   sample_id = c(
#'     11652, 13384, 8681, 13282, 13561, 8434, 4550, 11611, 13534, 13512, 8507,
#'     2258
#'   ),
#'   indextypology = c(
#'     rep("brabeel", 6), "bron", "upstream", "vlagzalm", rep("brabeel", 2),
#'     "forel"
#'   )
#' )
#' data_sample <- data_sample |>
#'   inner_join(index_info, by = "sample_id")
#' # calculate index
#' calculate_eqr(data_sample, data_fish)
#' calculate_eqr(data_sample, data_fish, output = "metric")
#'
#' # lake index
#' data_sample <- read.csv2(
#'   system.file("testdata/kallemoeie_sample.csv", package = "EQRfishes")
#' )
#' data_fish <- read.csv2(
#'   system.file("testdata/kallemoeie_fish_data.csv", package = "EQRfishes")
#' )
#' cluster <- data.frame(
#'   sample_id = c("Kallemoeie_e", "Kallemoeie_f"),
#'   index_cluster = "Kallemoeie"
#' )
#'
#' calculate_eqr(data_sample, data_fish, cluster = cluster)
#' calculate_eqr(data_sample, data_fish, output = "metric", cluster = cluster)
#'
calculate_eqr <- function(
  data_sample, data_fish, output = c("EQR", "metric", "detail"), cluster = NA
) {

  stopifnot(inherits(data_sample, "data.frame"))
  assert_that(has_name(data_sample, "indextypology"))
  stopifnot(is.character(data_sample$indextypology))
  if (has_name(data_sample, "indextypology_short")) {
    # remove column with this name to avoid problems
    # other (unneeded) columns disappear as well, change this if others are kept
    data_sample$indextypology_short <- NULL
  }
  index_names <- suppressMessages(
    read_csv2(
      system.file(
        "extdata/index_names_english_dutch.csv", package = "EQRfishes"
      )
    )
  )
  data_sample <- data_sample |>
    left_join(
      index_names |>
        select("short_name_e" = "short_name", "indextypology" = "english_name"),
      by = "indextypology"
    ) |>
    left_join(
      index_names |>
        select("short_name_d" = "short_name", "indextypology" = "dutch_name"),
      by = "indextypology"
    ) |>
    mutate(
      indextypology_short = ifelse(
        .data$indextypology %in% index_names$short_name,
        .data$indextypology,
        ifelse(
          .data$indextypology %in% index_names$english_name,
          .data$short_name_e,
          ifelse(
            .data$indextypology %in% index_names$dutch_name,
            .data$short_name_d,
            NA
          )
        )
      ),
      short_name_d = NULL,
      short_name_e = NULL
    )

  if (any(is.na(data_sample$indextypology_short))) {
    invalid <- paste(
      unique(
        data_sample[is.na(data_sample$indextypology_short), "indextypology"]
      ),
      collapse = ", "
    )
    stop(
      paste(
        "indextypology", invalid,
        "is (are) invalid, please replace in data_sample"
      )
    )
  }

  do.call(
    rbind,
    by(
      data_sample,
      data_sample$indextypology_short,
      function(x) validate(new_sample(x))
    )
  )
  data_sample$sample_id <- as.character(data_sample$sample_id)

  # make sure data_fish has all columns present, and remove additional columns
  # (these can cause problems if fish data are nested)
  assert_that(has_name(data_fish, "sample_id"))
  data_fish$sample_id <- as.character(data_fish$sample_id)
  assert_that(has_name(data_fish, "record_id"))
  assert_that(has_name(data_fish, "taxoncode"))
  assert_that(has_name(data_fish, "number"))
  assert_that(
    inherits(data_fish$number, "integer") |
      inherits(data_fish$number, "numeric")
  )
  assert_that(all(is.na(data_fish$number) | data_fish$number >= 0))
  assert_that(has_name(data_fish, "length"))
  assert_that(is.numeric(data_fish$length))
  assert_that(has_name(data_fish, "weight"))
  assert_that(is.numeric(data_fish$length))
  data_fish <- data_fish %>%
    select("sample_id", "record_id", "taxoncode", "number", "length", "weight")

  match.arg(output)

  data_sample <- data_sample %>%
    mutate(
      method =
        ifelse(str_detect(.data$method, "^E"), "E", .data$method),
      method =
        ifelse(str_detect(.data$method, "^SF"), "SF", .data$method)
    )

  join_data_fish <- "sample_id"
  select_keys <- "sample_id"
  if (
    any(str_detect(data_sample$indextypology_short, "estuarien|lakes|canals"))
  ) {
    stopifnot(
      "Argument cluster must be provided for indextypologies lakes, canals or estuarien" # nolint: line_length_linter
      = length(cluster) != 1 || !is.na(cluster)
    )
    assert_that(has_name(cluster, c("sample_id", "index_cluster")))
    cluster$sample_id <- as.character(cluster$sample_id)
    data_sample <- data_sample %>%
      left_join(
        cluster,
        by = "sample_id"
      ) %>%
      mutate(
        sample_id_replace =
          ifelse(
            is.na(.data$index_cluster),
            .data$sample_id,
            paste(
              .data$index_cluster,
              #paste method 2 times to end with at least 2 characters
              substr(paste0(.data$method, .data$method), 1, 2),
              sep = "_"
            )
          )
      )
    test_index_cluster <- data_sample %>%
      filter(
        is.na(.data$index_cluster),
        str_detect(.data$indextypology_short, "estuarien|lakes|canals")
      )
    if (nrow(test_index_cluster) > 0) {
      stop(
        sprintf(
          "Argument cluster must contain all sample id's with indextypologies lakes, canals or estuarien (not provided for sample_id %s)", # nolint: line_length_linter
          paste(unique(test_index_cluster$sample_id), collapse = ", ")
        )
      )
    }
    data_fish <- data_fish %>%
      inner_join(
        data_sample %>%
          select(
            "sample_id_replace", "method", "sample_id"
          ) %>%
          distinct(),
        by = "sample_id"
      ) %>%
      mutate(
        sample_id = .data$sample_id_replace,
        sample_id_replace = NULL
      )
    join_data_fish <- c("sample_id", "method")
    select_keys <- c("sample_id", "index_cluster")
    data_sample <- data_sample %>%
      mutate(
        sample_id = .data$sample_id_replace
      ) %>%
      group_by(
        .data$sample_id, .data$method,
        .data$year, .data$indextypology, .data$indextypology_short
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
        .data$sample_id, .data$method, .data$year,  # group by year
        .data$indextypology, .data$indextypology_short
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
      sample_id = as.character(.data$sample_id),
      width_transect = as.character(.data$width_transect),
      length_trajectory = as.character(.data$length_trajectory),
      width_river = as.character(.data$width_river),
      slope = as.character(.data$slope),
      n_fyke_nets = as.character(.data$n_fyke_nets),
      n_days = as.character(.data$n_days)
    ) %>%
    gather(
      key = "name", value = "value",
      -"sample_id", -"indextypology", -"indextypology_short", -"method", -"year"
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
  stopifnot(
    "Some fishes don't have any data (no number of fishes, no fish length and no weight). Please indicate at least the number of fishes or remove the record(s)." # nolint: line_length_linter
    = nrow(fish_not_measured) == 0
  )
  rm(fish_not_measured)

  fish_zero_number <- data_fish %>%
    filter(.data$number <= 0)
  stopifnot(
    "For some fishdata, the number of fishes is zero or below. Please give a positive integer for the number of fishes (or remove the record(s))." # nolint: line_lenght_linter
    = nrow(fish_zero_number) == 0
  )
  rm(fish_zero_number)

  data_fish <- data_fish %>%
    filter(
      !is.na(.data$taxoncode),
      !.data$taxoncode %in% no_fish$taxoncode
    ) %>%
    mutate(sample_id = as.character(.data$sample_id)) %>%
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
          system.file("extdata/indextypology_metric.csv", package = "EQRfishes")
        )
      ) %>%
        nest(
          metric_name_group =
            c("metric_formula_name", "metric_measures_name")
        ),
      by = c("indextypology_short" = "indextypology"),
      suffix = c("", "_for_metric"), relationship = "many-to-many"
    ) %>%
    filter(
      is.na(.data$method_for_metric) |
        str_detect(.data$method, .data$method_for_metric)
    )
  if (nrow(result) == 0) {
    problem <- data_sample %>%
      distinct(
        .data$sample_id, .data$method, .data$year,
        .data$indextypology, .data$indextypology_short
      ) %>%
      left_join(
        suppressMessages(
          read_csv2(
            system.file(
              "extdata/indextypology_metric.csv", package = "EQRfishes"
            )
          )
        ) %>%
          distinct(.data$indextypology, .data$method),
        by = c("indextypology_short" = "indextypology"),
        suffix = c("", "_for_metric"), relationship = "many-to-many"
      ) %>%
      filter(
        !str_detect(.data$method, .data$method_for_metric)
      )
    stop(
      sprintf(
        "method %s cannot be used to calculate the index %s for sample_id %s,
        only %s is allowed",
        problem$method, problem$indextypology, problem$sample_id,
        problem$method_for_metric
      )
    )
  }
  result <- result %>%
    left_join(
      data_fish,
      by = join_data_fish
    ) %>%
    mutate(
      row_id = seq_along(.data$sample_id)
    ) %>%
    arrange(.data$row_id)
  result$sampledata <- calculate_metric(result, specieslist = data_taxonmetrics)
  rm(data_taxonmetrics)

  result_details <- result %>%
    select(
      "sample_id", "indextypology", "year", "sampledata"
    ) %>%
    unnest(cols = c("sampledata")) %>%
    distinct()

  result_metrics <- result %>%
    select(
      "sample_id", "indextypology", "indextypology_short", "year",
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
      .data$sample_id, .data$indextypology, .data$year,
      .data$metric_name, .data$metric_score_name, .data$method_for_metric,
      .data$indextypology_short
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
    filter(str_detect(.data$indextypology_short, "estuarien|lakes|canals")) %>%
    mutate(
      sample_id = substr(.data$sample_id, 1, nchar(.data$sample_id) - 3)
    ) %>%
    group_by(
      .data$sample_id, .data$indextypology, .data$year,
      .data$metric_name, .data$metric_score_name, .data$method_for_metric,
      .data$indextypology_short
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
          .data$indextypology_short %in%
            c("estuarien_Schelde_oligohaline", "estuarien_Schelde_mesohaline",
              "estuarien_Schelde_freshwater"),
          .data$metric_name == "MnsTot"
        ) %>%
        select(
          "sample_id", "indextypology_short", "year",
          "MnsTot" = .data$metric_value
        ),
      by = c("sample_id", "indextypology_short", "year")
    ) %>%
    mutate(
      metric_score = ifelse(
        !is.na(.data$MnsTot) & .data$MnsTot == 0, "0", .data$metric_score
      ),
      MnsTot = NULL,
      index_cluster = .data$sample_id,
      sample_id = NULL
    )

  if (nrow(result_metrics_aggregated) > 0) {
    result_metrics <- result_metrics %>%
      filter(
        !str_detect(.data$indextypology_short, "estuarien|lakes|canals")
      ) %>%
      mutate(
        sample_id_trim =
          substr(.data$sample_id, 1, nchar(.data$sample_id) - 3)
      ) %>%
      left_join(
        cluster %>%
          mutate(
            sample_id = as.character(.data$sample_id)
          ),
        by = c("sample_id_trim" = "sample_id")
      ) %>%
      mutate(
        sample_id = ifelse(
          !is.na(.data$index_cluster),
          .data$sample_id_trim,
          .data$sample_id
        ),
        sample_id_trim = NULL
      ) %>%
      bind_rows(result_metrics_aggregated)
  }

  eqr_scores <-
    suppressMessages(
      read_csv2(system.file("extdata/score.csv", package = "EQRfishes"))
    )
  ibi_exceptions <-
    suppressMessages(
      read_csv2(
        system.file("extdata/calculate_ibi_eqr.csv", package = "EQRfishes")
      )
    )

  result_eqr <- result_metrics %>%
    left_join(
      ibi_exceptions %>%
        nest(.by = "indextypology", .key = "ibi_exceptions"),
      by = c("indextypology_short" = "indextypology"),
      relationship = "many-to-one"
    ) %>%
    mutate(
      calc_method_old =
        .data$indextypology_short %in% c("brasem", "barbeel", "upstream",
                                         "forel", "vlagzalm")
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
              list(
                .data$metrics, .data$calc_method_old,
                ibi_exceptions
              ),
              calculate_ibi_score
            )
          )
        ),
      calc_method_old =
        ifelse(
          grepl("estuarien", .data$indextypology_short) &
            !.data$indextypology == "estuarien_zijrivieren_zoet",
          TRUE,
          .data$calc_method_old
        ),
      std_ibi =
        unlist(
          pmap(
            list(
              .data$ibi, .data$metrics, .data$calc_method_old,
              .data$indextypology_short
            ),
            standardise_ibi
          )
        ),
      calc_method_old =
        ifelse(
          grepl("estuarien", .data$indextypology_short) &
            !.data$indextypology_short == "estuarien_zijrivieren",
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
          .data$indextypology_short %in% c("lakes", "brabeel"),
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
          .data$indextypology_short %in%
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
          .data$indextypology_short == "estuarien_IJzer",
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
          grepl("estuarien_Schelde", .data$indextypology_short),
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
          .data$indextypology == "estuarien_IJzer",
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
          grepl("estuarien_Schelde", .data$indextypology_short),
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
          .data$indextypology_short == "estuarien_IJzer",
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
          .data$indextypology_short %in%
            c("brabeel", "lakes", "bron", "estuarien_zijrivieren_zoet"),
          .data$std_ibi, .data$eqr
        ),
      eqr =
        ifelse(
          .data$indextypology_short == "bron" & .data$ibi == 4,
          0.2,
          .data$eqr
        ),
      eqr =
        ifelse(
          .data$indextypology_short == "bron" & .data$ibi == 0,
          0,
          .data$eqr
        ),
      eqr = ifelse(
        .data$indextypology_short == "estuarien_zijrivieren_zoet" &
          .data$ibi == 0.8,
        0.05,
        .data$eqr
      )
    ) %>%
    select(
      select_keys, "indextypology", "year", "calc_method_old",
      "ibi", "eqr_class", "eqr"
    )

  if (output[[1]] == "EQR") {
    return(result_eqr)
  }

  # if defaults are used, results of metrics are replaced by NA
  result_metrics <- result_metrics %>%
    left_join(
      result_metrics %>%
        inner_join(
          ibi_exceptions,
          by = c(
            "indextypology_short" = "indextypology",
            "metric_name" = "calculated"
          ),
          relationship = "many-to-many"
        ) %>%
        filter(var_in_interval(.data$metric_value, .data$interval)) %>%
        left_join(
          result_metrics,
          by = c(
            select_keys, "indextypology", "indextypology_short",
            "calculated2" = "metric_name"
          ),
          suffix = c("", "2"),
          relationship = "many-to-many"
        ) %>%
        filter(
          var_in_interval(.data$metric_value2, .data$interval2)
        ) %>%
        select(
          all_of(select_keys), "metric_name_calc" = "metric_name", "calculated2"
        ) %>%
        pivot_longer(
          c("metric_name_calc", "calculated2"),
          names_to = NULL,
          values_to = "metric_name_calc",
          values_drop_na = TRUE
        ) %>%
        distinct() %>%
        group_by(across(all_of(select_keys))) %>%
        mutate(suffix_no = as.character(seq_len(n()))) %>%
        ungroup() %>%
        pivot_wider(
          names_from = "suffix_no",
          names_prefix  = "metric_name_calc",
          values_from = "metric_name_calc"
        ) %>%
        bind_rows(    #add empty table to add columns if they don't exist yet
          data.frame(
            metric_name_calc1 = character(), metric_name_calc2 = character()
          )
        ) %>%
        mutate(ibi_exception = TRUE),
      by = select_keys,
      relationship = "many-to-one"
    ) %>%
    mutate(
      metric_value =
        ifelse(
          !is.na(.data$ibi_exception) &
            .data$metric_name != .data$metric_name_calc1 &
            .data$metric_name != .data$metric_name_calc2,
          NA, .data$metric_value
        ),
      metric_score =
        ifelse(!is.na(.data$ibi_exception), NA, .data$metric_score),
      ibi_exception = NULL,
      metric_name_calc1 = NULL,
      metric_name_calc2 = NULL
    ) %>%
    select(-.data$indextypology_short)

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
