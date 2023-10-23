context("test calculate_eqr canals")

library(dplyr)
library(tidyr)

load(system.file("extrafiles/visdata.Rdata", package = "EQRfishes"))

zonation_info <-
  data.frame(
    sample_key =
      c(
        13279, 13280, 13285:13292, 13294:13296,
        13207:13217, 13387
      ),
    zonation = "canals",
    location =
      c(rep("Kanaal van Bocholt naar Herentals", 13),
        rep("Kanaal Roeselare-Leie", 12))
  )
data_sample <- data_sample %>%
  inner_join(zonation_info, by = "sample_key") %>%
  mutate(
    sample_key_grouped = paste(.data$location, substr(.data$method, 1, 2), sep = "_"),
    LocationID = location
  )
data_fish <- data_fish %>%
  left_join(
    data_sample %>%
      select("sample_key", "sample_key_grouped"),
    by = "sample_key") %>%
  mutate(
    sample_key = .data$sample_key_grouped #,
    # number =
    #   ifelse(is.na(.data$number) & .data$taxoncode %in% c("POM.MIC.", "POM.MIN."), 0, .data$number)
  ) %>%
  select(-"sample_key_grouped", -"sample_key_new") %>%
  filter(!is.na(sample_key))

data_sample <- data_sample %>%
  mutate(
    sample_key = sample_key_grouped
  ) %>%
  group_by(sample_key, LocationID, method, Stilstaand, tidal, Brak, IndexTypeCode, year, zonation, location, sample_key_new) %>%
  summarise(
    surface = sum(.data$width_transect * .data$length_trajectory),
    length_trajectory = sum(.data$length_trajectory),
    n_fyke_nets = sum(.data$n_fyke_nets),
    n_days = mean(.data$n_days),
    width_river = mean(.data$width_river),
    slope = mean(.data$slope)
  ) %>%
  ungroup() %>%
  group_by(sample_key, LocationID, method, Stilstaand, tidal, Brak, IndexTypeCode, year, zonation, location) %>%  #group by year
  summarise(
    surface = sum(.data$surface),
    length_trajectory = sum(.data$length_trajectory),
    n_fyke_nets = mean(.data$n_fyke_nets),
    n_days = sum(.data$n_days),
    width_river = mean(.data$width_river),
    slope = mean(.data$slope)
  ) %>%
  ungroup() %>%
  mutate(
    width_transect = .data$surface / .data$length_trajectory,
    surface = NULL
  )

describe("IBI is calculated correctly", {
  it("canals", {
    expect_warning(
      results_eqr <- calculate_eqr(
          data_sample,
          data_fish
        ),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ORC.LIM., HYB.HYB., ERI.SIN., ATY.DES."
    )

    expect_equal(
      results_eqr$ibi,
      c(3.2, 2.6)
    )
    expect_equal(
      results_eqr$eqr,
      c(0.77941177, 0.5882353)
    )
    expect_equal(
      results_eqr$score_cat,
      c("GEP", "moderate")
    )
    expect_equal(
      results_eqr$beoordeling,
      c("goed", "matig")
    )
  })
})

describe("metrics are calculated correctly", {
  it("canals", {
    expect_warning(
      result_metrics <-
        calculate_eqr(
          data_sample,
          data_fish, output = "metric"
        )[["metric"]] %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ORC.LIM., HYB.HYB., ERI.SIN., ATY.DES."
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsTot"))$metric_value,
      c("9", "10")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsTot"))$metric_score,
      c("0.8", "0.8")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManBio"))$metric_value,
      c("16.455", "2.617")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManBio"))$metric_score,
      c("0.4", "0.2")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManSha"))$metric_value,
      c("1.65", "1.65")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManSha"))$metric_score,
      c("0.8", "0.8")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManRec"))$metric_value,
      c("60", "71.429")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManRec"))$metric_score,
      c("0.6", "0.6")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "BenWei"))$metric_value,
      c("35.036", "1.061")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "BenWei"))$metric_score,
      c("0.6", "0.2")
    )
  })
})
