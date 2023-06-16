context("test calculate_eqr lakes")

library(dplyr)
library(tidyr)
library(readr)

data_sample <-
  read_csv2(
    system.file("testdata/kallemoeie_sample.csv", package = "EQRfishes"))
data_fish <-
  read_csv2(
    system.file("testdata/kallemoeie_fish_data.csv",
                package = "EQRfishes")) %>%
  mutate(
    record_id = 1:n()
  )

describe("IBI is calculated correctly", {
  it("lakes", {
    expect_no_warning(
      results_eqr <- calculate_eqr(
          data_sample,
          data_fish
        )
    ) #verschillende samples van locatie moeten samengenomen worden!

    # expect_equal(
    #   results_eqr$ibi,
    #   3
    # )
    # expect_equal(
    #   results_eqr$eqr,
    #   0.45
    # )
    # expect_equal(
    #   results_eqr$beoordeling,
    #   "ontoereikend"
    # )
  })
})

describe("metrics are calculated correctly", {
  it("lakes", {
    expect_no_warning(
      result_metrics <-
        calculate_eqr(
          data_sample,
          data_fish, output = "metric"
        )[["metric"]] %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3)))
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiSpa"))$metric_value,
      "82"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiSpa"))$metric_score,
      "0.6"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiInv"))$metric_value,
      "12"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiInv"))$metric_score,
      "0.6"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiOmn"))$metric_value,
      "10.448"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiOmn"))$metric_score,
      "0.8"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsPis"))$metric_value,
      "1"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsPis"))$metric_score,
      "0.4"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "BenWei"))$metric_value,
      "35.855"
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "BenWei"))$metric_score,
      "0.4"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_value,
      "11"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_score,
      "0.2"
    )
  })
})
