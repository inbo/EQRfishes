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
