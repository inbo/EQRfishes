
library(EQRfishes)
load(file = "inst/extrafiles/visdata.Rdata")

data_sample_zonation <- determine_zonation(data_sample[1:500, ])
data_sample_zonation <- determine_zonation(data_sample[1:500, ], "old")
results_eqr <-
  calculate_eqr(data_sample_zonation[c(29:30,32,34:36,39:41,43:62,477), ], data_fish)

library(tidyverse)
results_eqr_yzer <-
  calculate_eqr(
    data_sample_new %>%
      filter(.data$IndexTypeCode == "YZRP"),
    data_fish %>%
      mutate(
        sample_key = .data$sample_key_new,
        sample_key_new = NULL
      )
  )
