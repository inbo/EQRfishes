context("test determine_indextypology")

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

describe("indextypology is determined correctly", {

  it("brasem en barbeel", {
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8681, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8681, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11652, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11652, ], version = "old"
      ))$indextypology,
      "brasem"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13384, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13384, ], version = "old"
      ))$indextypology,
      "brasem"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13282, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13282, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13561, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13561, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8434, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8434, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13512, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13512, ], version = "old"
      ))$indextypology,
      "brasem"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8507, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 8507, ], version = "old"
      ))$indextypology,
      "brasem"
    )
  })
  it("upstream", {
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 4550, ]
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 4550, ], version = "old"
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11611, ]
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 11611, ], version = "old"
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13534, ]
      ))$indextypology,
      "vlagzalm"
    )
    expect_equal(
      (determine_indextypology(
        data_sample[data_sample$sample_id == 13534, ], version = "old"
      ))$indextypology,
      "vlagzalm"
    )
  })
})
