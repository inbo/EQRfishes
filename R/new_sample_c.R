#' constructor to add data_sample in s3 classes for validation
#' (--> method validate)
#'
#' @noRd
#'
#' @importFrom assertthat assert_that has_name

new_sample <- function(data_sample = data.frame()) {
  stopifnot(inherits(data_sample, "data.frame"))
  assert_that(has_name(data_sample, "indextypology_short"))
  stopifnot(is.character(data_sample$indextypology_short))
  data_sample$class_eqr <- switch(
    unique(data_sample$indextypology_short),
    barbeel = ,
    brasem = "eqr_lowland_old",
    brabeel = "eqr_lowland_new",
    upstream = "eqr_upstream",
    canals = "eqr_canals",
    estuarien_Schelde_freshwater = ,
    estuarien_Schelde_oligohaline = "eqr_fykedays",
    estuarien_IJzer = ,
    estuarien_Schelde_mesohaline = ,
    estuarien_zijrivieren_zoet = "eqr_estuarien",
    lakes = "eqr_lakes",
    forel = ,
    vlagzalm = ,
    bron = "eqr_source"
  )

  structure(data_sample, class = unique(data_sample$class_eqr))
}
