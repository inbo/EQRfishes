
library(EQRfishes)
load(file = "inst/extrafiles/visdata.Rdata")

data_sample_indextypology <- determine_indextypology(data_sample[1:500, ])
data_sample_indextypology <- determine_indextypology(data_sample[1:500, ], "old")
results_eqr <-
  calculate_eqr(data_sample_indextypology[c(29:30,32,34:36,39:41,43:62,477), ], data_fish)

#onderstaande vervangen door nieuw voorbeeld, of alles in voorbeeld van documentatie van functie?
library(tidyverse)
results_eqr_yzer <-
  calculate_eqr(
    data_sample_new %>%
      filter(.data$index_type_code == "YZRP"),
    data_fish %>%
      mutate(
        sample_id = .data$sample_id_new,
        sample_id_new = NULL
      )
  )
