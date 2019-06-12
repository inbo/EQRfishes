#' calculate formula based on given parameters
#'
#' small function that calculates a formula after replacing the parameter names with their values
#'
#' @param formula formula including only given parameters
#' @param sampledata location specific variables (column name) with their values (value)
#' @param metric_name name of the variable to be calculated
#'
#' @return table sampledata with an additional metric which is the result of calculating the formula
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr arrange bind_rows desc distinct
#' @importFrom pander evals
#'
#' @export
#'
#'
calculate_formula <-
  function(
    formula, sampledata, metric_name
  ) {

  sampledata %<>%
    distinct() %>%
    arrange(desc(nchar(.data$name)))

  for (i in seq_len(nrow(sampledata))) {
    formula <-
      gsub(
        pattern = sampledata$name[i], replacement = sampledata$value[i],
        x = formula
      )
  }
  result <- evals(formula)[[1]]$result
  result <- ifelse(is.null(result), NA, result)

  sampledata %<>%
    bind_rows(
      data.frame(
        name = metric_name,
        value = as.character(result),
        stringsAsFactors = FALSE
      )
    )

  return(sampledata)
}
