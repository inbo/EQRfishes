# this file contains small helper functions that are called in the function calculate_metric_measures

#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr count distinct filter group_by left_join mutate select summarise ungroup
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

sum_of_scored_length_classes <- function(data, var) {
  scores <-
    suppressMessages(
      read_csv2(
        system.file("extdata/data_classes.csv", package = "EQRfishes")
      )
    ) %>%
    filter(.data$variable == var)
  if (all(is.na(scores$taxoncode))) {
    scores <-
      merge(
        scores %>% select(-.data$taxoncode),
        data.frame(taxoncode = unique(data$taxoncode), stringsAsFactors = FALSE)
      )
  }
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
    left_join(scores, by = c("taxoncode")) %>%
    filter(var_in_interval(.data$nclass, .data$interval)) %>%
    summarise(result = sum(.data$score))
  return(data$result)
}

total_weight <- function(data) {
  data %<>%
    summarise(weight = sum(.data$weight))
  return(data$weight)
}

sum_values_column <- function(data, specieslist, variable) {
  data %<>%
    select(.data$taxoncode) %>%
    distinct() %>%
    left_join(specieslist, by = "taxoncode") %>%
    summarise(value = sum(get(variable)))
  return(data$value)
}

shannon_wiener_index <- function(data, specieslist) {
  MniTot <- number_of_individuals(data)
  if (!is.numeric(MniTot) | MniTot == 0) {
    return(0)
  }
  data %<>%
    left_join(
      specieslist %>%
        select(.data$taxoncode, .data$Shannon_Weaner),
      by = "taxoncode"
    ) %>%
    filter(.data$Shannon_Weaner == 1) %>%
    group_by(.data$taxoncode) %>%
    summarise(
      number = sum(.data$number)
    ) %>%
    ungroup() %>%
    mutate(
      shannon_wiener =
        -.data$number / MniTot * log(.data$number / MniTot)
    ) %>%
    summarise(
      shannon_wiener = sum(.data$shannon_wiener)
    )
  return(data$shannon_wiener)
}
