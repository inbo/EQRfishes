library(dplyr)
library(tidyr)

data_sample <-
  data.frame(
    sample_id_part1 = c(
      "brasem", "barbeel", "brabeel", "upstream", "forel", "vlagzalm", "bron",
      "lakes_f", "lakes_e", "canals_f", "canals_e", "estuarien_IJzer",
      "estuarien_Schelde_freshwater", "estuarien_Schelde_oligohaline",
      "estuarien_Schelde_mesohaline", "estuarien_zijrivieren_zoet"
    ),
    width_transect = c(rep(2, 7), NA, 2.5, NA, 2.5, rep(NA, 5)),
    length_trajectory = c(rep(100, 7), NA, 250, NA, 250, rep(NA, 5)),
    width_river = 10,
    slope = 0,
    n_fyke_nets = c(rep(NA, 7), 10, NA, 2, NA, rep(2, 5)),
    n_days = c(rep(NA, 7), 2, NA, 2, NA, rep(1, 5)),
    method = c(rep("EW1", 7), "SF", "E", "SF", "EB2", rep("SF", 5)),
    year = 2020,
    season = "autumn",
    Stilstaand = NA,
    tidal = NA,
    Brak = NA,
    LocationID = NA,
    indextypology = c(
      "brasem", "barbeel", "brabeel", "upstream", "forel", "vlagzalm", "bron",
      "lakes", "lakes", "canals", "canals", "estuarien_IJzer",
      "estuarien_Schelde_freshwater", "estuarien_Schelde_oligohaline",
      "estuarien_Schelde_mesohaline", "estuarien_zijrivieren_zoet"
    )
  ) |>
  merge(
    y = data.frame(
      sample_id_part2 = c("_0soorten", "_blankvoorn", "_blauwbandgrondel")
    )
  ) |>
  mutate(
    sample_id = paste0(sample_id_part1, sample_id_part2),
    index_type_code = .data$indextypology
  )
data_fish <-
  data.frame(
    sample_id = c("blankvoorn", "blauwbandgrondel"),
    taxoncode = c("RUT.RUT.", "PSE.PAR."),
    number = 1,
    length = c(8, 5.3),
    weight = c(4.7, 1.2)
  ) |>
  merge(
    y = data_sample |>
      distinct(.data$sample_id_part1)
  ) |>
  mutate(
    sample_id = paste(sample_id_part1, sample_id, sep = "_"),
    sample_id_part1 = NULL,
    record_id = seq_len(n())
  )

cluster <- data_sample |>
  select("sample_id") |>
  mutate(
    index_cluster = .data$sample_id,
    index_cluster = gsub("(\\w*)_[ef]_(.*)", "\\1_\\2", .data$index_cluster)
  )

data_sample <- data_sample |>
  select(-"sample_id_part1", -"sample_id_part2")

test_that("IBI and EQR are calculated correctly", {
    expect_warning(
      results_eqr <- calculate_eqr(
        data_sample,
        data_fish,
        cluster = cluster,
        output = "metric"
      ),
      "No fishdata for one of the records, metric gets value 0"
    )

    expect_equal(
      results_eqr[["eqr"]] |>
        filter(
          grepl("_0soorten", sample_id) | grepl("_0soorten", index_cluster)
        ) |>
        pull(eqr),
      c(rep(0.0, 14))
    )
    expect_equal(
      results_eqr[["eqr"]] |>
        filter(
          grepl("_blankvoorn", sample_id) | grepl("_blankvoorn", index_cluster)
        ) |>
        pull(eqr),
      c(0.2, 0.17, rep(0.2, 5), 0.0, 0.192307692, rep(0.1, 3), 0.01, 0.1)
    )
    expect_equal(
      results_eqr[["eqr"]] |>
        filter(
          grepl("_blauwbandgrondel", sample_id) |
            grepl("_blauwbandgrondel", index_cluster)
        ) |>
        pull(eqr),
      c(0.2, 0.0, rep(0.2, 5), 0.0, 0.192307692, rep(0.0, 3), -0.000000003, 0.0)
    )
    expect_equal(
      results_eqr[["eqr"]] |>
        filter(
          grepl("_0soorten", sample_id) | grepl("_0soorten", index_cluster)
        ) |>
        pull(ibi),
      c(0.0, 1.0, rep(0.0, 5), 1.0, rep(0.0, 4), 0.8, 1.2)
    )
    expect_equal(
      results_eqr[["eqr"]] |>
        filter(
          grepl("_blankvoorn", sample_id) | grepl("_blankvoorn", index_cluster)
        ) |>
        pull(ibi),
      c(1, 5.08, 1.0, 4.0, rep(1.0, 5), rep(0.96, 3), 0.832, 1.6)
    )
    expect_equal(
      results_eqr[["eqr"]] |>
        filter(
          grepl("_blauwbandgrondel", sample_id) |
            grepl("_blauwbandgrondel", index_cluster)
        ) |>
        pull(ibi),
      c(1, 1.0, 1.0, 4.0, rep(1.0, 5), rep(0.0, 3), 0.8, 1.2)
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_0soorten", sample_id) | grepl("_0soorten", index_cluster),
          index_cluster != "canals_0soorten"
        ) |>
        pull(metric_score),
      rep(NA_character_, 87)
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              index_cluster == "canals_0soorten"
            ) |>
            pull(metric_score)
        )
      )
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_0soorten", sample_id) | grepl("_0soorten", index_cluster),
          index_cluster != "canals_0soorten",
          !metric_name %in% c("MnsTot", "MniYzer"),
          !(indextypology == "lakes" & metric_name == "MniInd")
        ) |>
        pull(metric_value),
      rep(NA_character_, 74)
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_0soorten", sample_id) |
                grepl("_0soorten", index_cluster),
              index_cluster == "canals_0soorten"
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_0soorten", sample_id) |
                grepl("_0soorten", index_cluster),
              metric_name %in% c("MnsTot", "MniYzer")
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_0soorten", sample_id) |
                grepl("_0soorten", index_cluster),
              indextypology == "lakes" & metric_name == "MniInd"
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_blankvoorn", sample_id) |
            grepl("_blankvoorn", index_cluster),
          !index_cluster %in%
            c("canals_blankvoorn", "forel_blankvoorn", "vlagzalm_blankvoorn",
              "estuarien_IJzer_blankvoorn", "lakes_blankvoorn")
        ) |>
        pull(metric_score),
      rep(NA_character_, 62)
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_blankvoorn", sample_id) |
            grepl("_blankvoorn", index_cluster),
          index_cluster == "estuarien_IJzer_blankvoorn",
          metric_name == "MniYzer"
        ) |>
        pull(metric_score),
      NA_character_
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blankvoorn", sample_id) |
                grepl("_blankvoorn", index_cluster),
              index_cluster %in%
                c("canals_blankvoorn", "forel_blankvoorn",
                  "vlagzalm_blankvoorn", "estuarien_IJzer_blankvoorn",
                  "lakes_blankvoorn"),
              !is.na(metric_score_name)
            ) |>
            pull(metric_score)
        )
      )
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_blankvoorn", sample_id) |
            grepl("_blankvoorn", index_cluster),
          !index_cluster %in%
            c("canals_blankvoorn", "forel_blankvoorn", "vlagzalm_blankvoorn",
              "estuarien_IJzer_blankvoorn", "lakes_blankvoorn"),
          !metric_name == "MnsTot",
          !(metric_name == "MniInd" &
              indextypology %in% c("brabeel", "upstream")),
          !(metric_name == "MnsInd" & indextypology == "bron")
        ) |>
        pull(metric_value),
      rep(NA_character_, 50)
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blankvoorn", sample_id) |
                grepl("_blankvoorn", index_cluster),
              index_cluster %in%
                c("canals_blankvoorn", "forel_blankvoorn",
                  "vlagzalm_blankvoorn", "estuarien_IJzer_blankvoorn",
                  "lakes_blankvoorn"),
              !metric_name == "MniYzer"
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blankvoorn", sample_id) |
                grepl("_blankvoorn", index_cluster),
              metric_name == "MnsTot"
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blankvoorn", sample_id) |
                grepl("_blankvoorn", index_cluster),
              metric_name == "MniInd" &
                indextypology %in% c("brabeel", "upstream")
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blankvoorn", sample_id) |
                grepl("_blankvoorn", index_cluster),
              metric_name == "MnsInd" & indextypology == "bron"
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_blauwbandgrondel", sample_id) |
            grepl("_blauwbandgrondel", index_cluster),
          !index_cluster %in%
            c("canals_blauwbandgrondel", "forel_blauwbandgrondel",
              "vlagzalm_blauwbandgrondel", "estuarien_IJzer_blauwbandgrondel")
        ) |>
        pull(metric_score),
      rep(NA_character_, 69)
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_blauwbandgrondel", sample_id) |
            grepl("_blauwbandgrondel", index_cluster),
          index_cluster == "estuarien_IJzer_blauwbandgrondel",
          metric_name == "MniYzer"
        ) |>
        pull(metric_score),
      NA_character_
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blauwbandgrondel", sample_id) |
                grepl("_blauwbandgrondel", index_cluster),
              index_cluster %in%
                c("canals_blauwbandgrondel", "forel_blauwbandgrondel",
                  "vlagzalm_blauwbandgrondel",
                  "estuarien_IJzer_blauwbandgrondel"),
              !is.na(metric_score_name)
            ) |>
            pull(metric_score)
        )
      )
    )
    expect_equal(
      results_eqr[["metric"]] |>
        filter(
          grepl("_blauwbandgrondel", sample_id) |
            grepl("_blauwbandgrondel", index_cluster),
          !index_cluster %in%
            c("canals_blauwbandgrondel", "forel_blauwbandgrondel",
              "vlagzalm_blauwbandgrondel", "estuarien_IJzer_blauwbandgrondel"),
          !grepl("estuarien_Schelde_.*_blauwbandgrondel", index_cluster),
          !metric_name == "MnsTot",
          !(metric_name == "MniInd" &
              indextypology %in% c("upstream", "lakes")),
          !(metric_name == "MnsInd" & indextypology == "bron")
        ) |>
        pull(metric_value),
      rep(NA_character_, 41)
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blauwbandgrondel", sample_id) |
                grepl("_blauwbandgrondel", index_cluster),
              index_cluster %in%
                c("canals_blauwbandgrondel", "forel_blauwbandgrondel",
                  "vlagzalm_blauwbandgrondel",
                  "estuarien_IJzer_blauwbandgrondel"),
              !grepl("estuarien_Schelde_.*_blauwbandgrondel", index_cluster)
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blauwbandgrondel", sample_id) |
                grepl("_blauwbandgrondel", index_cluster),
              metric_name == "MnsTot"
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blauwbandgrondel", sample_id) |
                grepl("_blauwbandgrondel", index_cluster),
              metric_name == "MniInd" &
                indextypology %in% c("upstream", "lakes")
            ) |>
            pull(metric_value)
        )
      )
    )
    expect_true(
      all(
        !is.na(
          results_eqr[["metric"]] |>
            filter(
              grepl("_blauwbandgrondel", sample_id) |
                grepl("_blauwbandgrondel", index_cluster),
              metric_name == "MnsInd" & indextypology == "bron"
            ) |>
            pull(metric_value)
        )
      )
    )
})
describe("mix of clustered and not clustered: correct presentation", {
    data_sample <- data_sample |>
      filter(grepl("bron", sample_id) | grepl("lakes", sample_id))
    data_fish <- data_fish |>
      filter(grepl("bron", sample_id) | grepl("lakes", sample_id))
    cluster1 <- cluster |>
      filter(grepl("bron", sample_id) | grepl("lakes", sample_id))
    cluster2 <- cluster |>
      filter(grepl("lakes", sample_id))
    expect_warning(
      results_eqr1 <- calculate_eqr(
        data_sample,
        data_fish,
        cluster = cluster1,
        output = "metric"
      ),
      "No fishdata for one of the records, metric gets value 0"
    )
    expect_warning(
      results_eqr2 <- calculate_eqr(
        data_sample,
        data_fish,
        cluster = cluster2,
        output = "metric"
      ),
      "No fishdata for one of the records, metric gets value 0"
    )
    expect_equal(
      results_eqr1[["eqr"]] |>
        mutate(
          index_cluster =
            ifelse(grepl("bron", index_cluster), NA, index_cluster)
        ),
      results_eqr2[["eqr"]]
    )
})
