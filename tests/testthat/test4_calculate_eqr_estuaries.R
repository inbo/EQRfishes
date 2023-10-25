context("test calculate_eqr estuaries")

library(dplyr)
library(tidyr)

load(system.file("extrafiles/visdata.Rdata", package = "EQRfishes"))

zonation_info <-
  data.frame(
    sample_key =
      rep(
        c(9877, 9878, 9900:9903, 9906, 9907, 10260:10266, 10268, 10269,
          10276, 10277, 10347:10356, 10392, 10393, 10398:10401, 11274),
        2
      ),
    version = "new",
    zonation =
      rep(
        c(rep("estuarien_Schelde_mesohaline", 2),
          rep("estuarien_Schelde_freshwater", 6),
          rep("estuarien_Schelde_oligohaline", 4),
          rep("estuarien_Schelde_freshwater", 5),
          rep("estuarien_Schelde_mesohaline", 2),
          rep("estuarien_Schelde_freshwater", 6),
          rep("estuarien_Schelde_oligohaline", 4),
          rep("estuarien_Schelde_mesohaline", 2),
          rep("estuarien_Schelde_oligohaline", 4),
          rep("estuarien_Schelde_freshwater", 1)),
        2
      ),
    location =
      c(rep("moneos_meso", 2), rep("moneos_zoet", 6), rep("moneos_oligo", 4),
        rep("moneos_zoet", 5), rep("moneos_meso", 2), rep("moneos_zoet", 6),
        rep("moneos_oligo", 4), rep("moneos_meso", 2), rep("moneos_oligo", 4),
        rep("moneos_zoet", 1),
        rep("SCHELDE IV Paardenschor", 2), rep("SCHELDE II", 2),
        rep("SCHELDE I", 4), rep("SCHELDE III", 2),
        rep("SCHELDE IV Antwerpen", 2), rep("SCHELDE II", 2),
        rep("SCHELDE I", 3), rep("SCHELDE IV Paardenschor", 2),
        rep("SCHELDE II", 2), rep("SCHELDE I", 4),
        rep("SCHELDE IV Antwerpen", 2), rep("SCHELDE III", 2),
        rep("SCHELDE IV Paardenschor", 2), rep("SCHELDE III", 2),
        rep("SCHELDE IV Antwerpen", 2), rep("SCHELDE I", 1))
  )

data_sample <- data_sample %>%
  inner_join(zonation_info, by = "sample_key") %>%
  mutate(
    LocationID = .data$location,
    sample_key = .data$location
  ) %>%
  group_by(sample_key, LocationID, method, Stilstaand, tidal, Bekken, Brak, IndexTypeCode, year, version, zonation, location, sample_key_new) %>%
  summarise(
    surface = sum(.data$width_transect * .data$length_trajectory),
    length_trajectory = sum(.data$length_trajectory),
    n_fyke_nets = sum(.data$n_fyke_nets),
    n_days = mean(.data$n_days),
    width_river = mean(.data$width_river),
    slope = mean(.data$slope)
  ) %>%
  ungroup() %>%
  group_by(sample_key, LocationID, method, Stilstaand, tidal, Bekken, Brak, IndexTypeCode, year, version, zonation, location) %>%  #group by year
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
data_fish <- data_fish %>%
  left_join(zonation_info, by = "sample_key", relationship = "many-to-many") %>%
  mutate(
    sample_key = .data$location,
    number =
      ifelse(is.na(.data$number) & .data$taxoncode %in% c("POM.MIC.", "POM.MIN."), 0, .data$number)
  ) %>%
  select(-"version", -"zonation", -"location", -"sample_key_new") %>%
  filter(!is.na(sample_key), number > 0)

# Metrieken aangepast van 0-5 naar 0-1, dus waarschijnlijk moet EQR-berekening hier ook aan aangepast worden
describe("IBI is calculated correctly", {
  it("estuarien freshwater", {
    expect_warning(
      results_eqr <- calculate_eqr(
          data_sample %>%
            filter(zonation == "estuarien_Schelde_freshwater"),
          data_fish
        ),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN., CRA.CRA., PAL.FFF., CAR.MAE., HEM.TAK."
    )
    expect_equal(
      results_eqr$ibi,
      c(3.2, 2.8, 3.2)
    )
    expect_equal(
      results_eqr$eqr,
      c(0.4166666666667, 0.333333333, 0.41666666667)
    )
    expect_equal(
      results_eqr$score_cat,
      c("poor", "poor", "poor")
    )
  })
  it("estuarien mesohaline", {
    expect_warning(
      results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "estuarien_Schelde_mesohaline"),
        data_fish
      ),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN., CRA.CRA., PAL.FFF., CAR.MAE., HEM.TAK."
    )
    expect_equal(
      results_eqr$ibi,
      c(3.6, 3.6)
    )
    expect_equal(
      results_eqr$eqr,
      c(0.5, 0.5)
    )
    expect_equal(
      results_eqr$score_cat,
      c("moderate", "moderate")
    )
  })
  it("estuarien oligohaline", {
    expect_warning(
      results_eqr <- calculate_eqr(
        data_sample %>%
          filter(zonation == "estuarien_Schelde_oligohaline"),
        data_fish
      ),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN., CRA.CRA., PAL.FFF., CAR.MAE., HEM.TAK."
    )

    expect_equal(
      results_eqr$ibi,
      c(2.4, 2.4, 2.8)
    )
    expect_equal(
      results_eqr$eqr,
      c(0.25, 0.25, 0.33333333)
    )
    expect_equal(
      results_eqr$score_cat,
      c("poor", "poor", "poor")
    )
  })
})

describe("metrics are calculated correctly", {
  it("estuarien freshwater", {
    expect_warning(
      result_metrics <-
        calculate_eqr(
          data_sample %>%
            filter(zonation == "estuarien_Schelde_freshwater"),
          data_fish, output = "metric"
        )[["metric"]] %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN., CRA.CRA., PAL.FFF., CAR.MAE., HEM.TAK."
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsTot"))$metric_value,
      c("14", "12", "16")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MnsTot"))$metric_score,
      c("0.6", "0.6", "0.8")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MniInd"))$metric_value,
      c("67.667", "191", "108.778")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MniInd"))$metric_score,
      c("0.4", "1", "0.6")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiDia"))$metric_value,
      c("26.478", "14.398", "19.408")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiDia"))$metric_score,
      c("0.8", "0.4", "0.6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiSpa"))$metric_value,
      c("3.325", "5.192", "4.418")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiSpa"))$metric_score,
      c("0.2", "0.2", "0.2")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiPis"))$metric_value,
      c("15.456", "8.115", "11.159")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiPis"))$metric_score,
      c("0.4", "0.2", "0.4")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiBen"))$metric_value,
      c("23.461", "9.25", "15.143")
    )
    expect_equal(
      (result_metrics %>%
        filter(metric_name == "MpiBen"))$metric_score,
      c("0.8", "0.4", "0.6")
    )
  })
  it("estuarien mesohaline", {
    expect_warning(
      result_metrics <-
        calculate_eqr(
          data_sample %>%
            filter(zonation == "estuarien_Schelde_mesohaline"),
          data_fish, output = "metric"
        )[["metric"]] %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN., CRA.CRA., PAL.FFF., CAR.MAE., HEM.TAK."
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_value,
      c("14", "14")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsTot"))$metric_score,
      c("0.6", "0.6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsDia"))$metric_value,
      c("6", "6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsDia"))$metric_score,
      c("0.6", "0.6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsSpa"))$metric_value,
      c("2", "2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsSpa"))$metric_score,
      c("0.4", "0.4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsHab"))$metric_value,
      c("6", "6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsHab"))$metric_score,
      c("0.4", "0.4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiInt"))$metric_value,
      c("51.84", "51.84")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MpiInt"))$metric_score,
      c("1", "1")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsMms"))$metric_value,
      c("5", "5")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsMms"))$metric_score,
      c("0.6", "0.6")
    )
  })
  it("estuarien oligohaline", {
    expect_warning(
      result_metrics <-
        calculate_eqr(
          data_sample %>%
            filter(zonation == "estuarien_Schelde_oligohaline"),
          data_fish, output = "metric"
        )[["metric"]] %>%
        mutate(metric_value = as.character(round(as.numeric(metric_value), 3))),
      "Some taxoncodes given in data_fish are unknown fishes and these records will be excluded from the analysis:  ERI.SIN., CRA.CRA., PAL.FFF., CAR.MAE., HEM.TAK."
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsPis"))$metric_value,
      c("6", "5", "7")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsPis"))$metric_score,
      c("0.4", "0.4", "0.6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MniInd"))$metric_value,
      c("51.333", "57.667", "54.5")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MniInd"))$metric_score,
      c("0.4", "0.4", "0.4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsInt"))$metric_value,
      c("2", "1", "2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsInt"))$metric_score,
      c("0.2", "0.2", "0.2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsDia"))$metric_value,
      c("4", "4", "4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsDia"))$metric_score,
      c("0.4", "0.4", "0.4")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsMms"))$metric_value,
      c("1", "2", "2")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsMms"))$metric_score,
      c("0.4", "0.6", "0.6")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsErs"))$metric_value,
      c("3", "2", "3")
    )
    expect_equal(
      (result_metrics %>%
         filter(metric_name == "MnsErs"))$metric_score,
      c("0.6", "0.4", "0.6")
    )
  })
})
