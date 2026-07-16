#' validate sample objects created by constructor new_sample
#' (check and give warning/error if required input for index is not correct)
#'
#' @noRd
#'
#' @importFrom assertthat assert_that has_name

validate <- function(data_sample) {
  UseMethod("validate")
}

# this function does the validations that are relevant to all indextypologies
validate_basic <- function(data_sample) {
  assert_that(has_name(data_sample, "method"))
  data_sample$method <- as.character(data_sample$method)
  assert_that(
    all(grepl("^E|SF|SN", data_sample$method)),
    msg = "method abbreviations should start with E (electrofishing), SF (fykefishing) or SN (trawling), as index calculations are standardised to these methods" #nolint: line_length_linter
  )
  assert_that(has_name(data_sample, "sample_id"))
  assert_that(
    length(data_sample$sample_id) == length(unique(data_sample$sample_id)),
    msg = "sample_id should be unique for each row in data_sample"
  )
  assert_that(has_name(data_sample, "year"))
  assert_that(has_name(data_sample, "width_transect"))
  if (all(is.na(data_sample$width_transect))) {
    data_sample$width_transect <- as.numeric(data_sample$width_transect)
  }
  assert_that(is.numeric(data_sample$width_transect))
  assert_that(has_name(data_sample, "length_trajectory"))
  if (all(is.na(data_sample$length_trajectory))) {
    data_sample$length_trajectory <- as.numeric(data_sample$length_trajectory)
  }
  assert_that(is.numeric(data_sample$length_trajectory))
  assert_that(has_name(data_sample, "width_river"))
  if (all(is.na(data_sample$width_river))) {
    data_sample$width_river <- as.numeric(data_sample$width_river)
  }
  assert_that(is.numeric(data_sample$width_river))
  assert_that(has_name(data_sample, "slope"))
  if (all(is.na(data_sample$slope))) {
    data_sample$slope <- as.numeric(data_sample$slope)
  }
  assert_that(is.numeric(data_sample$slope))
  assert_that(has_name(data_sample, "n_fyke_nets"))
  if (all(is.na(data_sample$n_fyke_nets))) {
    data_sample$n_fyke_nets <- as.integer(data_sample$n_fyke_nets)
  }
  assert_that(is.numeric(data_sample$n_fyke_nets))
  assert_that(all(is.na(data_sample$n_fyke_nets) | data_sample$n_fyke_nets > 0))
  assert_that(has_name(data_sample, "n_days"))
  if (all(is.na(data_sample$n_days))) {
    data_sample$n_days <- as.integer(data_sample$n_days)
  }
  assert_that(is.numeric(data_sample$n_days))

  return(data_sample)
}

# this function does the validations that are relevant for surface in ManBio
validate_surface <- function(data_sample, indexname) {
  if (
    any(is.na(data_sample$width_transect) | data_sample$width_transect <= 0)
  ) {
    warning(
      paste(
        "In case width_transect is empty, zero or below, width_river will be",
        "used to calculate surface for ManBio."
      )
    )
    if (
      any(
        (is.na(data_sample$width_transect) | data_sample$width_transect <= 0) &
          (is.na(data_sample$width_river) | data_sample$width_river <= 0)
      )
    ) {
      warning(
        paste(
          "A width_river of zero or below leads to a score of 1 for ManBio",
          "in the fish-based", indexname, "IBI, even with a high biomass.",
          "No value (NA) for width_river leads to no score (NA) for ManBio.",
          "Make sure width_transect, river_width and length_trajectory are",
          "correct to obtain a reliable IBI for", indexname
        )
      )
    }
  }
  if (
    any(
      is.na(data_sample$length_trajectory) | data_sample$length_trajectory <= 0
    )
  ) {
    warning(
      paste(
        "A length_trajectory of zero or below leads to a score of 1 for ManBio",
        "in the fish-based", indexname, "IBI, even with a high biomass.",
        "No value (NA) for length_trajectory leads to no score (NA) for",
        "ManBio.",
        "Make sure length_trajectory, width_transect and river_width are",
        "correct to obtain a reliable IBI for", indexname
      )
    )
  }
}

# for forel, vlagzalm and bron
#' @export
validate.eqr_source <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  assert_that(
    all(startsWith(data_sample$method, "E")),
    msg = paste(
      "Data for the fish-based trout, grayling and river source IBI",
      "should be collected by electrofishing (method E)"
    )
  )
  return(list(source = TRUE))
}

#' @export
validate.eqr_upstream <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  assert_that(
    all(startsWith(data_sample$method, "E")),
    msg = paste(
      "Data for the fish-based upstream IBI should be collected by",
      "electrofishing (method E)"
    )
  )
  validate_surface(data_sample, "upstream")
  if (any(is.na(data_sample$slope) | data_sample$slope <= 0)) {
    warning(
      paste(
        "In the fish-based upstream index, slope is used to calibrate",
        "some metric scores, make sure to give a correct value",
        "(NA will lead to a NA score, and a slope of 0 or below to score 1)."
      )
    )
  }
  return(list(upstream = TRUE))
}

#' @export
validate.eqr_lowland_new <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  assert_that(
    all(startsWith(data_sample$method, "E")),
    msg = paste(
      "Data for the fish-based lowland IBI should be collected by",
      "electrofishing (method E)"
    )
  )
  assert_that(
    all(!is.na(data_sample$width_river) & data_sample$width_river > 0),
    msg = paste(
      "width_river should not be empty (NA) and above 0 for the fish-based",
      "lowland IBI, as the scores of some metrics depend on it"
    )
  )
  assert_that(has_name(data_sample, "season"))
  assert_that(is.character(data_sample$season))
  assert_that(all(data_sample$season %in% c("spring", "summer", "autumn")))
  return(list(brabeel = TRUE))
}

# brasem and barbeel
#' @export
validate.eqr_lowland_old <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  data_sample_e <- data_sample
  data_sample_e$length_trajectory <-
    data_sample_e$length_trajectory[grepl("^E|SN", data_sample$method)]
  data_sample_e$width_transect <-
    data_sample_e$width_transect[grepl("^E|SN", data_sample$method)]
  data_sample_e$width_river <-
    data_sample_e$width_river[grepl("^E|SN", data_sample$method)]
  if (length(data_sample_e$width_river) > 0) {
    assert_that(
      all(!is.na(data_sample$width_river) & data_sample_e$width_river > 0),
      msg = paste(
        "width_river should not be empty (NA) and above 0 for the fish-based",
        "bream and barbel IBI, as the score of metric MnsTot depend on it"
      )
    )
    validate_surface(data_sample_e, "bream and barbel")
  }
  data_sample_sf_nets <-
    data_sample$n_fyke_nets[grepl("^SF", data_sample$method)]
  assert_that(
    length(data_sample_sf_nets) == 0 || all(!is.na(data_sample_sf_nets)),
    msg = paste(
      "n_fyke_nets should have a correct value for fykefishing in the",
      "fish-based bream and barbel IBI, as the value surface to score ManBio",
      "depends on it."
    )
  )
  return(list(lowland_old = TRUE))
}

#' @export
validate.eqr_canals <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  assert_that(
    all(
      startsWith(data_sample$method, "E") | startsWith(data_sample$method, "SF")
    ),
    msg = paste(
      "Data for the fish-based canals IBI should be collected by",
      "electrofishing (method E) and fykefishing (method SF)."
    )
  )
  stopifnot(
    "The fish-based canals IBI requires samples from electric fishing (method E) and fykefishing (method SF)" =  #nolint: line_length_linter
      any(startsWith(data_sample$method, "E")) &
      any(startsWith(data_sample$method, "SF"))
  )
  data_sample_e <- data_sample
  data_sample_e$length_trajectory <-
    data_sample_e$length_trajectory[grepl("^E", data_sample$method)]
  data_sample_e$width_transect <-
    data_sample_e$width_transect[grepl("^E", data_sample$method)]
  data_sample_e$width_river <-
    data_sample_e$width_river[grepl("^E", data_sample$method)]

  validate_surface(data_sample_e, "canals")
  return(list(canals = TRUE))
}

#' @export
validate.eqr_lakes <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  assert_that(
    all(
      startsWith(data_sample$method, "E") | startsWith(data_sample$method, "SF")
    ),
    msg = paste(
      "Data for the fish-based lakes IBI should be collected by",
      "electrofishing (method E) and fykefishing (method SF)."
    )
  )
  stopifnot(
    "The fish-based lakes IBI requires samples from electric fishing (method E) and fykefishing (method SF)" =  #nolint: line_length_linter
      any(startsWith(data_sample$method, "E")) &
      any(startsWith(data_sample$method, "SF"))
  )
  return(list(lakes = TRUE))
}

# estuarien_Schelde_freshwater and estuarien_Schelde_oligohaline
#' @export
validate.eqr_fykedays <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  assert_that(
    all(startsWith(data_sample$method, "SF")),
    msg = paste(
      "Data for the fish-based estuarine indexes",
      "should be collected by fykefishing (method SF)"
    )
  )
  stopifnot(
    "n_fyke_nets and n_days should be positive integers to get reliable and valid results for the fish-based estuarine Schelde freshwater and oligohaline indexes" =  #nolint: line_length_linter
      all(
        !is.na(data_sample$n_fyke_nets) & data_sample$n_fyke_nets > 0 &
          !is.na(data_sample$n_days) & data_sample$n_days > 0
      )
  )
  return(list(fykedays = TRUE))
}

# estuarien_Schelde_mesohaline, estuarien_IJzer and estuarien_zijrivieren_zoet
#' @export
validate.eqr_estuarien <- function(data_sample) {
  data_sample <- validate_basic(data_sample)
  assert_that(
    all(startsWith(data_sample$method, "SF")),
    msg = paste(
      "Data for the fish-based estuarine indexes should be collected by",
      "fykefishing (method SF)"
    )
  )
  return(list(fykedays = TRUE))
}
