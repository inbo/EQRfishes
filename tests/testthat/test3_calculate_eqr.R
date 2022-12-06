context("test determine_zonation")

library(dplyr)
library(tidyr)

load(system.file("extrafiles/visdata.Rdata", package = "EQRfishes"))

zonation_info <-
  data.frame(
    order_id = 1:22,
    sample_key =
      rep(
        c(11652, 13384, 8681, 13282, 13561, 8434, 4550, 11611, 13534, 13512,
          8507),
        2
      ),
    version = c(rep("new", 11), rep("old", 11)),
    zonation =
      c(
        rep("brabeel", 6), "bron", "upstream", "vlagzalm", rep("brabeel", 2),
        rep("brasem", 2), rep("barbeel", 4), rep("upstream", 2), "vlagzalm",
        rep("brasem", 2)
      )
  )

data_sample <- data_sample %>%
  inner_join(zonation_info, by = "sample_key")
order_location <- data_sample %>%
  select(order_id, LocationID, version) %>%
  distinct()

describe("IBI is calculated correctly", {
  it("brasem en barbeel", {
    results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "brabeel" & version == "new"),
        data_fish
      ) %>%
        left_join(
          order_location %>%
            filter(version == "new"),
          by = "LocationID"
        ) %>%
        arrange(order_id)
    expect_equal(
      results_eqr$ibi,
      c(NA, 9, 20, 11, NA, 12, 12, NA)
    )
    expect_equal(
      results_eqr$eqr,
      c(NA, 0.33333333333333, 0.7916666666666666667, 0.4166666666666667, NA,
        0.45833333333333333333, 0.45833333333333333333, NA)
    )
    expect_equal(
      results_eqr$beoordeling,
      c("niet genoeg individuen", "ontoereikend", "GEP", "ontoereikend",
        "niet genoeg individuen", "ontoereikend", "ontoereikend",
        "niet genoeg individuen")
    )
  })
  it("upstream", {
    results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "bron" & version == "new"),
        data_fish
      ) %>%
        left_join(
          order_location %>%
            filter(version == "new"),
          by = "LocationID"
        ) %>%
        arrange(order_id)
    expect_equal(
      results_eqr$ibi,
      1.6666666666666666666666666667
    )
    expect_equal(
      results_eqr$eqr,
      3
    )
    expect_equal(
      results_eqr$beoordeling,
      "ontoereikend"
    )
    results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "upstream" & version == "new"),
        data_fish
      ) %>%
        left_join(
          order_location %>%
            filter(version == "new"),
          by = "LocationID"
        ) %>%
        arrange(order_id)
    expect_equal(
      results_eqr$ibi,
      3
    )
    expect_equal(
      results_eqr$eqr,
      0.5
    )
    expect_equal(
      results_eqr$beoordeling,
      "matig"
    )
    results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "vlagzalm" & version == "new"),
        data_fish
      ) %>%
        left_join(
          order_location %>%
            filter(version == "new"),
          by = "LocationID"
        ) %>%
        arrange(order_id)
    expect_equal(
      results_eqr$ibi,
      3.6666666666666666666666667
    )
    expect_equal(
      results_eqr$eqr,
      7
    )
    expect_equal(
      results_eqr$beoordeling,
      "goed"
    )
  })
})

describe("metrics are calculated correctly", {
  result_metrics <-
    calculate_eqr(
      data_sample %>%
        filter(zonation == "brabeel" & version == "new"),
      data_fish %>% filter(!is.na(taxoncode)), output = "metric"
    )[["metric"]] %>%
    left_join(
      order_location %>%
        filter(version == "new"),
      by = "LocationID"
    ) %>%
    arrange(order_id) %>%
    mutate(metric_value = as.character(round(as.numeric(metric_value), 3)))
  it("brasem en barbeel", {
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManSha"))$metric_value,
      c("0.693", "0.534", "1.539", "0.722", "1.061", "1.14", "1.706", "0.6")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "ManSha"))$metric_score,
      c("2", "2", "5", "2", "4", "4", "4", "2")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsTot"))$metric_value,
      c("2", "7", "8", "4", "3", "5", "10", "2")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsTot"))$metric_score,
      c("1", "3", "3", "3", "2", "3", "3", "1")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiInv"))$metric_value,
      c("0", "0", "53.017", "77.228", "77.778", "0", "44.595", "0")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiInv"))$metric_score,
      c("0", "0", "5", "1", "1", "0", "3", "0")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiTol"))$metric_value,
      c("50", "94.145", "44.828", "6.931", "0", "72.727", "14.865", "100")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiTol"))$metric_score,
      c("3", "1", "4", "1", "1", "2", "1", "1")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpsRec"))$metric_value,
      c("0", "42.857", "37.5", "50", "33.333", "40", "20", "50")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpsRec"))$metric_score,
      c("0", "3", "3", "4", "2", "3", "1", "4")
    )
  })
})
