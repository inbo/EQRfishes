#' is variable present in interval?
#'
#' checks if a given variable is present in a given interval and returns TRUE or FALSE
#'
#' @param variable float, often a measurement
#' @param interval (vector of) string(s) of an interval, e.g. '[1,3['
#'
#' @return TRUE or FALSE
#'
#' @export
#'
#'
var_in_interval <- function(variable, interval) {
  operator_min <-
    gsub(
      "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
      "\\1", interval
    )
  value_min <-
    gsub(
      "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
      "\\2", interval
    )
  value_max <-
    gsub(
      "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
      "\\6", interval
    )
  operator_max <-
    gsub(
      "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
      "\\9", interval
    )

  result_min <-
    ifelse(
      operator_min == "]",
      ifelse(
        value_min == " ",
        TRUE,
        as.numeric(variable) > as.numeric(value_min)
      ),
      ifelse(
        operator_min == "[",
        as.numeric(variable) >= as.numeric(value_min),
        NA
      )
    )

  result_max <-
    ifelse(
      operator_max == "[",
      ifelse(
        value_max == " ",
        TRUE,
        as.numeric(variable) < as.numeric(value_max)
      ),
      ifelse(
        operator_max == "]",
        as.numeric(variable) <= as.numeric(value_max),
        NA
      )
    )

  if (max(is.na(result_min)) | max(is.na(result_max))) {
    stop("At least one interval is not recognised")
  }

  return(result_min & result_max)
}
