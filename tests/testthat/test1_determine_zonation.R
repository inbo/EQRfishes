context("test determine_zonation")

library(readr)
library(dplyr)
library(tidyr)

data_sample <- read.csv2(
  system.file("testdata/freshwater_sample.csv", package = "EQRfishes")
) %>%
  mutate(
    IndexTypeCode =
      ifelse(
        IndexTypeCode %in% c("brabeel", "brasem", "barbeel", "forel",
                             "vlagzalm", "upstream", "bron"),
        "ZTWA",
        IndexTypeCode
      )
  )

describe("indextypology is determined correctly", {

  it("brasem en barbeel", {
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8681, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8681, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11652, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11652, ], version = "old"
      ))$indextypology,
      "brasem"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13384, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13384, ], version = "old"
      ))$indextypology,
      "brasem"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13282, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13282, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13561, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13561, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8434, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8434, ], version = "old"
      ))$indextypology,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13512, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13512, ], version = "old"
      ))$indextypology,
      "brasem"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8507, ]
      ))$indextypology,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8507, ], version = "old"
      ))$indextypology,
      "brasem"
    )
  })
  it("upstream", {
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 4550, ]
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 4550, ], version = "old"
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11611, ]
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11611, ], version = "old"
      ))$indextypology,
      "upstream"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13534, ]
      ))$indextypology,
      "vlagzalm"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13534, ], version = "old"
      ))$indextypology,
      "vlagzalm"
    )
  })
})
