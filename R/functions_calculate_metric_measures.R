# this file contains small helper functions that are called in the function calculate_metric_measures

#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr summarise select distinct count

number_of_individuals <- function(data) {
  data %<>%
    summarise(n_ind = sum(number))
  return(data$n_ind)
}

number_of_species <- function(data) {
  data %<>%
    select(taxoncode) %>%
    distinct() %>%
    count()
  return(data$n)
}

total_weight <- function(data) {
  data %<>%
    summarise(weight = sum(weight))
  return(data$weight)
}
