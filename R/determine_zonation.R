#' determine the zonation for records of a dataset
#'
#' This function determines the zonation based on the given dataset with
#' measurements.
#' Recently 2 new zonations were added: 'bron' (replacing 'upstream' in rivers
#' up to a width of 2 m) and 'brabeel' (replacing 'brasem' and 'barbeel' in
#' rivers up to a width of 30 m), and both versions are available here:
#'   \itemize{
#'     \item \strong{new} (default) uses the new version with 'bron' and 'brabeel' included,
#'     \item \strong{old} uses the version before 'bron' and 'brabeel' were added
#'   }
#' This version can be indicated on the level of the individual records in the
#' dataset as an additional column or on the level of the whole dataset by using
#' argument `version`.
#' When both are added, NA values of column version in the dataset will be
#' replaced by the version given in argument `version`.
#'
#' @param dataset dataset on location measures with at least columns
#' \itemize{
#'     \item \strong{width_river} width of the river at the sample location,
#'     \item \strong{slope} slope of the river at the sample location,
#'     \item \strong{tidal} tidal effect present (TRUE) or absent (FALSE)?
#'     \item \strong{IndexTypeCode} type based on salinity of the sample location
#'   }
#' @param version 'new' version with bron and brabeel or 'old' version without
#' these two zonations?
#' Defaults to 'new'.
#' This information will be used if dataset has no column `version` or to
#' replace NA values in column `version`.
#'
#' @return input dataset with additional column 'zonation'
#'
#' @importFrom dplyr mutate rowwise ungroup
#' @importFrom rlang .data has_name
#' @importFrom magrittr %<>%
#'
#' @export
#'
determine_zonation <-
  function(dataset, version = c("new", "old")) {

    if (!has_name(dataset, "version")) {
      dataset$version <- version
    }
    dataset %<>%
      mutate(
        version = ifelse(is.na(.data$version), version, .data$version)
      ) %>%
      rowwise() %>%
      mutate(
        zonation =
          determine_zonation_helper(
            var_width = .data$width_river, var_slope = .data$slope,
            var_tidal = .data$tidal, var_indextype = .data$IndexTypeCode,
            version = .data$version
          )
      ) %>%
      ungroup()
    return(dataset)
  }
