#' determine the index typology for records of a dataset
#'
#' This function determines the index typology based on the given
#' dataset with measurements.
#' Recently 2 new index typologies were added: 'fish-based river source IBI'
#' (replacing 'fish-based upstream IBI' in rivers up to a width of 2 m) and
#' 'fish-based lowland IBI' (replacing 'fish-based bream IBI' and
#' 'fish-based barbel IBI' in rivers up to a width of 30 m),
#' and both versions are available here:
#'   \itemize{
#'     \item \strong{new} (default) uses the new version with
#'     'fish-based river source IBI' and 'fish-based lowland IBI' included,
#'     \item \strong{old} uses the version before 'fish-based river source IBI'
#'     and 'fish-based lowland IBI' were added
#'   }
#' This version can be indicated on the level of the individual records in the
#' dataset as an additional column or on the level of the whole dataset by using
#' argument `version`.
#' When both are added, NA values of column version in the dataset will be
#' replaced by the version given in argument `version`.
#'
#' @param dataset dataset on location measures with at least columns
#'
#' - `width_river` width of the river at the sample location (meters),
#' - `slope` slope of the river at the sample location (per thousand),
#' - `index_type_code` type based on salinity of the sample location:
#'   - `"ZTWA"` for freshwater,
#'   - `"YZRP"` for estuarine Ijzer,
#'   - `"SCHM"` for estuarine Schelde mesohaline,
#'   - `"SCHO"` for estuarine Schelde oligohaline,
#'   - `"SCHZ"` for estuarine Schelde freshwater,
#'   - `"ZTWZ"` for estuarine tributaries with freshwater
#' @param version 'new' version with fish-based river source IBI and
#' fish-based lowland IBI or 'old' version without these two indextypologies?
#' Defaults to 'new'.
#' This information will be used if dataset has no column `version` or to
#' replace NA values in column `version`.
#'
#' @return input dataset with additional columns `indextypology`,
#' `indextypology_short` and `indextypology_dutch`.
#' (Any of these columns can be used in `calculate_eqr()`, but the column name
#' must be (changed to) `indextypology`)
#'
#' @importFrom assertthat has_name
#' @importFrom dplyr mutate rowwise ungroup
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export
#' @family main
#'
determine_indextypology <-
  function(dataset, version = c("new", "old")) {

    match.arg(version)
    if (!has_name(dataset, "version")) {
      dataset$version <- version[1]
    }
    dataset <- dataset %>%
      mutate(
        version = ifelse(is.na(.data$version), version, .data$version)
      ) %>%
      rowwise() %>%
      mutate(
        indextypology_short =
          determine_indextypology_helper(
            var_width = .data$width_river, var_slope = .data$slope,
            var_indextype = .data$index_type_code,
            version = .data$version
          )
      ) %>%
      ungroup()

    # add English and Dutch names
    translations <- suppressMessages(
      read_csv2(
        system.file(
          "extdata/index_names_english_dutch.csv", package = "EQRfishes"
        )
      )
    ) %>%
      select(
        "indextypology" = "english_name",
        "indextypology_short" = "short_name",
        "indextypology_dutch" = "dutch_name"
      )
    dataset <- dataset %>%
      left_join(translations, by = c("indextypology_short"))
    return(dataset)
  }
