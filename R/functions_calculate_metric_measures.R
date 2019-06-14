# this file contains small helper functions that are called in the function calculate_metric_measures

#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr count distinct filter group_by left_join select summarise ungroup
#' @importFrom rlang .data
#' @importFrom readr read_csv2

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

number_of_length_classes <- function(data) {
  data %<>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/data_classes.csv", package = "EQRfishes")
        )
      ) %>%
        filter(.data$variable == "Recr"),
      by = "taxoncode"
    ) %>%
    filter(var_in_interval(.data$length, .data$interval)) %>%
    select(.data$taxoncode, .data$class) %>%
    distinct() %>%
    count()
  return(data$n)
}

number_of_species_with_multiple_length_classes <- function(data) {
  data %<>%
    left_join(
      suppressMessages(
        read_csv2(
          system.file("extdata/data_classes.csv", package = "EQRfishes")
        )
      ) %>%
        filter(.data$variable == "Recr"),
      by = "taxoncode"
    ) %>%
    filter(var_in_interval(.data$length, .data$interval)) %>%
    select(.data$taxoncode, .data$class) %>%
    distinct() %>%
    group_by(.data$taxoncode) %>%
    summarise(nclass = n()) %>%
    ungroup() %>%
    filter(.data$nclass > 1) %>%
    count()
  return(data$n)
}

total_weight <- function(data) {
  data %<>%
    summarise(weight = sum(.data$weight))
  return(data$weight)
}

sum_values_column <- function(data, specieslist, variable) {
  data %<>%
    select(.data$taxoncode, .data$number) %>%
    distinct() %>%
    left_join(specieslist, by = "taxoncode") %>%
    summarise(value = sum(get(variable)))
  return(data$value)
}
