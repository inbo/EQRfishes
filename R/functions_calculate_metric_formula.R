# this file contains small helper functions that are called in the function calculate_metric_formula

unlist_value <- function(data) {
  return(unlist(data)[[1]])
}

unlist_score <- function(data) {
  if (all(is.na(data))) {
    return(NA)
  } else {
    return(unlist(data)[[2]])
  }
}

#' @importFrom magrittr %<>%
#' @importFrom dplyr transmute
#' @importFrom utils head
unlist_name_group <- function(data) {
  data %<>%
    transmute(
      submetric_name =
        ifelse(
          is.na(.data$submetric_formula_name),
          .data$submetric_measures_name,
          .data$submetric_formula_name
        )
    ) %>%
    head(1)
  return(data)
}
