library(readr)
library(dplyr)
library(tidyr)

data_sample <- read.csv2(
  system.file("testdata/freshwater_sample.csv", package = "EQRfishes")
) %>%
  mutate(
    index_type_code =
      ifelse(
        index_type_code %in% c("brabeel", "brasem", "barbeel", "forel",
                               "vlagzalm", "upstream", "bron"),
        "ZTWA",
        index_type_code
      )
  )

test_that("indextypology is determined correctly for brasem en barbeel", {
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8681, ]
      ))$indextypology_short,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8681, ], version = "old"
      ))$indextypology_short,
      "barbeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11652, ]
      ))$indextypology_short,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11652, ], version = "old"
      ))$indextypology_short,
      "brasem"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13384, ]
      ))$indextypology,
      "fish-based lowland IBI"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13384, ], version = "old"
      ))$indextypology,
      "fish-based bream IBI"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13282, ]
      ))$indextypology,
      "fish-based lowland IBI"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13282, ], version = "old"
      ))$indextypology,
      "fish-based barbel IBI"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13561, ]
      ))$indextypology_short,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13561, ], version = "old"
      ))$indextypology_short,
      "barbeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8434, ]
      ))$indextypology_short,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8434, ], version = "old"
      ))$indextypology_short,
      "barbeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13512, ]
      ))$indextypology_short,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13512, ], version = "old"
      ))$indextypology_short,
      "brasem"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8507, ]
      ))$indextypology_short,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8507, ], version = "old"
      ))$indextypology_short,
      "brasem"
    )
})
test_that("indextypology is determined correctly for upstream", {
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 4550, ]
      ))$indextypology_short,
      "upstream"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 4550, ], version = "old"
      ))$indextypology_short,
      "upstream"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11611, ]
      ))$indextypology,
      "fish-based upstream IBI"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11611, ], version = "old"
      ))$indextypology,
      "fish-based upstream IBI"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13534, ]
      ))$indextypology_short,
      "vlagzalm"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13534, ], version = "old"
      ))$indextypology,
      "fish-based grayling IBI"
    )
})
test_that("indextypology is determined correctly for stilstaand", {
    data_sample <- data.frame(
      width_river = 5,
      slope = 0,
      index_type_code = "ZTWA",
      version = c("old", "new")
    )
    expect_warning(
      typology <- determine_indextypology(data_sample),
      regexp = "If the indextypology mentions 'fish-based canals or lakes IBI',"
    )
    expect_equal(
      typology,
      data_sample |>
        bind_cols(
          data.frame(
            indextypology_short = rep("stilstaand", 2),
            indextypology = rep("fish-based canals or lakes IBI", 2),
            indextypology_dutch = "stilstaand index"
          )
        ) |>
        as_tibble()
    )
})
