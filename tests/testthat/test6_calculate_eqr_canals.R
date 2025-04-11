context("test calculate_eqr canals")

library(dplyr)
library(tidyr)

data_sample <-
  read.csv2(system.file("testdata/canals_sample.csv", package = "EQRfishes"))
data_fish <-
  read.csv2(system.file("testdata/canals_fish_data.csv", package = "EQRfishes"))

zonation_info <-
  data.frame(
    sample_key = c(
      13279, 13280, 13285:13292, 13294:13296,
      13207:13217, 13387
    ),
    zonation = "canals",
    index_cluster = c(
      rep("Kanaal van Bocholt naar Herentals", 13),
      rep("Kanaal Roeselare-Leie", 12)
    )
  )
data_sample <- data_sample %>%
  inner_join(
    zonation_info %>%
      select("sample_key", "zonation"),
    by = "sample_key"
  )
data_fish <- data_fish %>%
  filter(!is.na(sample_key))

describe("IBI is calculated correctly", {
  it("canals", {
    expect_warning(
      results_eqr <- calculate_eqr(
        data_sample,
        data_fish,
        cluster = zonation_info %>%
          select("sample_key", "index_cluster")
      ),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ORC.LIM., HYB.HYB., ERI.SIN., ATY.DES." # nolint: line_length_linter
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
          data_fish, output = "metric",
          cluster = zonation_info %>%
            select("sample_key", "index_cluster")
        )[["metric"]] %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ORC.LIM., HYB.HYB., ERI.SIN., ATY.DES." # nolint: line_length_linter
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
