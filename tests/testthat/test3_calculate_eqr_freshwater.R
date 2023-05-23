context("test calculate_eqr freshwater")

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
  mutate( # because width_transect in macro example is different
    width_transect =
      ifelse(width_transect > width_river, width_river, width_transect)
  ) %>%
  inner_join(zonation_info, by = "sample_key")
order_location <- data_sample %>%
  select(order_id, LocationID, version) %>%
  distinct()
data_fish <- data_fish %>%
  filter(sample_key %in% zonation_info$sample_key)

describe("IBI is calculated correctly", {
  it("brasem en barbeel", {
    expect_warning(
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
          arrange(order_id),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )

    expect_equal(
      results_eqr$ibi,
      c(NA, 9, 20, 11, NA, 12, 12, NA)
    )
    # expect_equal(
    #   results_eqr$eqr,
    #   c(NA, 0.33333333333333, 0.7916666666666666667, 0.4166666666666667, NA,
    #     0.45833333333333333333, 0.45833333333333333333, NA)
    # )
    # expect_equal(
    #   results_eqr$beoordeling,
    #   c("niet genoeg individuen", "ontoereikend", "GEP", "ontoereikend",
    #     "niet genoeg individuen", "ontoereikend", "ontoereikend",
    #     "niet genoeg individuen")
    # )
  })
  it("upstream", {
    expect_warning(
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
          arrange(order_id),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    # expect_equal(
    #   results_eqr$ibi,
    #   1.6666666666666666666666666667
    # )
    # expect_equal(
    #   results_eqr$eqr,
    #   3
    # )
    # expect_equal(
    #   results_eqr$beoordeling,
    #   "ontoereikend"
    # )
    expect_warning(
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
          arrange(order_id),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    # expect_equal(
    #   results_eqr$ibi,
    #   3
    # )
    # expect_equal(
    #   results_eqr$eqr,
    #   0.5
    # )
    # expect_equal(
    #   results_eqr$beoordeling,
    #   "matig"
    # )
    expect_warning(
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
          arrange(order_id),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    # expect_equal(
    #   results_eqr$ibi,
    #   3.6666666666666666666666667
    # )
    # expect_equal(
    #   results_eqr$eqr,
    #   7
    # )
    # expect_equal(
    #   results_eqr$beoordeling,
    #   "goed"
    # )
  })
  it("brasem en barbeel old", {
    expect_warning(
      results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "brasem" & version == "old"),
        data_fish
      ) %>%
        left_join(
          order_location %>%
            filter(version == "old"),
          by = "LocationID"
        ) %>%
        arrange(order_id),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )

    expect_equal(
      results_eqr$ibi,
      c(1, 2.875, 2.875, 1.75)
    )
    expect_equal(
      results_eqr$eqr,
      c(0.2, 0.475, 0.475, 0.3)
    )
    # expect_equal(
    #   results_eqr$beoordeling,
    #   c("slecht", "matig", "matig", "ontoereikend")
    # )

    expect_warning(
      results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "barbeel" & version == "old"),
        data_fish
      ) %>%
        left_join(
          order_location %>%
            filter(version == "old"),
          by = "LocationID"
        ) %>%
        arrange(order_id),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )

    # expect_equal(
    #   results_eqr$ibi,
    #   c(3.625, 3.25, 2.875, 2.5)
    # )
    # expect_equal(
    #   results_eqr$eqr,
    #   c(0.625, 0.55, 0.475, 0.4)
    # )
    # expect_equal(
    #   results_eqr$beoordeling,
    #   c("goed", "matig", "matig", "ontoereikend")
    # )
  })
})

describe("metrics are calculated correctly", {
  it("brasem en barbeel", {
    expect_warning(
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
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
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
  it("upstream", {
    expect_warning(
      result_metrics <- calculate_eqr(
        data_sample %>%
          filter(zonation == "bron" & version == "new"),
        data_fish,
        output = "metric"
      )[["metric"]] %>%
        left_join(
          order_location %>%
            filter(version == "new"),
          by = "LocationID"
        ) %>%
        arrange(order_id) %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_score,
      "3"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_value,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManSha"))$metric_score,
      "2"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManSha"))$metric_value,
      "0.209"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiSpa"))$metric_score,
      "1"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiSpa"))$metric_value,
      "2.174"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiRhe"))$metric_score,
      "1"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiRhe"))$metric_value,
      "2.174"
    )
    expect_warning(
      result_metrics <- calculate_eqr(
        data_sample %>%
          filter(zonation == "upstream" & version == "new"),
        data_fish,
        output = "metric"
      )[["metric"]] %>%
        left_join(
          order_location %>%
            filter(version == "new"),
          by = "LocationID"
        ) %>%
        arrange(order_id) %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManBio"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManBio"))$metric_value,
      "71.439"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Mangkw"))$metric_score,
      "1"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Mangkw"))$metric_value,
      "1.667"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manmigw"))$metric_score,
      "1"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manmigw"))$metric_value,
      "0"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manswi"))$metric_score,
      "3"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manswi"))$metric_value,
      "0.998"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTyp"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTyp"))$metric_value,
      "3.75"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MniInd"))$metric_score,
      NA_character_
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MniInd"))$metric_value,
      "268"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsBen"))$metric_score,
      "3"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsBen"))$metric_value,
      "2"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_score,
      "3"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_value,
      "4"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Mpigesp"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Mpigesp"))$metric_value,
      "53.731"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiInvt"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiInvt"))$metric_value,
      "53.731"
    )
    expect_warning(
      result_metrics <- calculate_eqr(
        data_sample %>%
          filter(zonation == "vlagzalm" & version == "new"),
        data_fish,
        output = "metric"
      )[["metric"]] %>%
        left_join(
          order_location %>%
            filter(version == "new"),
          by = "LocationID"
        ) %>%
        arrange(order_id) %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_value,
      "14"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsBen"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsBen"))$metric_value,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiInvt"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiInvt"))$metric_value,
      "54.128"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manswi"))$metric_score,
      "3"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manswi"))$metric_value,
      "1.723"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Mangkw"))$metric_score,
      "3"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Mangkw"))$metric_value,
      "3"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manmigw"))$metric_score,
      "5"
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Manmigw"))$metric_value,
      "5"
    )
  })
  it("brasem old", {
    expect_warning(
      result_metrics <-
        calculate_eqr(
          data_sample %>%
            filter(zonation == "brasem" & version == "old"),
          data_fish %>% filter(!is.na(taxoncode)), output = "metric"
        )[["metric"]] %>%
        left_join(
          order_location %>%
            filter(version == "old"),
          by = "LocationID"
        ) %>%
        arrange(order_id) %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_value,
      c("2", "8", "13", "3")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_score,
      c("2", "3", "4", "2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_value,
      c("2.5", "1.571", "2.6", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_score,
      c("5", "2", "5", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTyp"))$metric_value,
      c("2", "4.286", "3.182", "3")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTyp"))$metric_score,
      c("1", "5", "4", "4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManSoort"))$metric_value,
      c("1", "1.333", "1", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManSoort"))$metric_score,
      c("1", "1", "1", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManBio"))$metric_value,
      c("2.267", "132.551", "50.773", "0.714")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManBio"))$metric_score,
      c("1", "5", "3", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpgExo"))$metric_value,
      c("0", "0.462", "2.649", "6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpgExo"))$metric_score,
      c("5", "5", "4", "3")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Trofcomp"))$metric_value,
      c("1", "1", "1.667", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Trofcomp"))$metric_score,
      c("1", "1", "1", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpsRekr"))$metric_value,
      c("0", "37.5", "23.077", "33.333")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpsRekr"))$metric_score,
      c("1", "1", "1", "1")
    )
  })
  it("barbeel old", {
    expect_warning(
      result_metrics <-
        calculate_eqr(
          data_sample %>%
            filter(zonation == "barbeel" & version == "old") %>%
            mutate(
              width_transect =
                ifelse(.data$width_transect == 3.7, 3.725, .data$width_transect)
            ),
          data_fish %>%
            filter(!is.na(taxoncode)) %>%
            mutate(
              weight =
                ifelse(is.na(.data$weight), 0, .data$weight)
            ),
          output = "metric"
        )[["metric"]] %>%
        left_join(
          order_location %>%
            filter(version == "old"),
          by = "LocationID"
        ) %>%
        arrange(order_id) %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN."
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_value,
      c("9", "4", "3", "6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_score,
      c("5", "3", "2", "4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_value,
      c("2.25", "1.75", "2.667", "2.2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTol"))$metric_score,
      c("4", "3", "5", "4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTyp"))$metric_value,
      c("3.125", "3.5", "3.667", "3")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManTyp"))$metric_score,
      c("5", "5", "5", "4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManSoort"))$metric_value,
      c("3.333", "2.333", "2.333", "2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManSoort"))$metric_score,
      c("3", "2", "2", "2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManBio"))$metric_value,
      c("99.237", "148.087", "17.81", "57.391")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "ManBio"))$metric_score,
      c("3", "4", "1", "2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpgExo"))$metric_value,
      c("0.453", "0", "0", "16.204")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpgExo"))$metric_score,
      c("5", "5", "5", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Trofcomp"))$metric_value,
      c("3", "2.333", "2.333", "1.667")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "Trofcomp"))$metric_score,
      c("3", "2", "2", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpsRekr"))$metric_value,
      c("33.333", "50", "33.333", "40")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpsRekr"))$metric_score,
      c("1", "2", "1", "2")
    )
  })
})
