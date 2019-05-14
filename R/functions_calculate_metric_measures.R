# this file contains small helper functions that are called in the function calculate_metric_measures

#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr count distinct left_join select summarise
#' @importFrom rlang .data

number_of_individuals <- function(data) {
  data %<>%
    summarise(n_ind = sum(.data$number))
  return(data$n_ind)
}

number_of_species <- function(data) {
  data %<>%
    select(.data$taxoncode) %>%
    distinct() %>%
    count()
  return(data$n)
}

total_weight <- function(data) {
  data %<>%
    summarise(weight = sum(.data$weight))
  return(data$weight)
}

sum_values <- function(data, specieslist, variable) {
  data %<>%
    left_join(specieslist, by = "taxoncode") %>%
    summarise(value = sum(get(variable)))
  return(data$value)
}
