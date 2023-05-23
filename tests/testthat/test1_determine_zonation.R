context("test determine_zonation")

library(readr)
library(dplyr)
library(tidyr)

load(system.file("extrafiles/visdata.Rdata", package = "EQRfishes"))
data_sample <- data_sample %>%
  mutate(
    IndexTypeCode =
      ifelse(
        IndexTypeCode %in% c("brabeel", "brasem", "barbeel", "forel",
                             "vlagzalm", "upstream", "bron"),
        "ZTWA",
        IndexTypeCode
      )
  )

describe("zonation is determined correctly", {

  it("brasem en barbeel", {
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8681, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8681, ], version = "old"
      ))$zonation,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11652, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11652, ], version = "old"
      ))$zonation,
      "brasem"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13384, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13384, ], version = "old"
      ))$zonation,
      "brasem"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13282, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13282, ], version = "old"
      ))$zonation,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13561, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13561, ], version = "old"
      ))$zonation,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8434, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8434, ], version = "old"
      ))$zonation,
      "barbeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13512, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13512, ], version = "old"
      ))$zonation,
      "brasem"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8507, ]
      ))$zonation,
      "brabeel"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 8507, ], version = "old"
      ))$zonation,
      "brasem"
    )
  })
  it("upstream", {
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 4550, ]
      ))$zonation,
      "bron"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 4550, ], version = "old"
      ))$zonation,
      "upstream"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11611, ]
      ))$zonation,
      "upstream"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 11611, ], version = "old"
      ))$zonation,
      "upstream"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13534, ]
      ))$zonation,
      "(undetermined)"
    )
    expect_equal(
      (determine_zonation(
        data_sample[data_sample$sample_key == 13534, ], version = "old"
      ))$zonation,
      "(undetermined)"
    )
  })
})
