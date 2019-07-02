context("test csv calculation rules")

library(readr)
library(dplyr)
library(tidyr)

data_guild <-
  suppressMessages(
    read_csv2(system.file("extdata/data_guild.csv", package = "EQRfishes"))
  )

guild_metric <-
  suppressMessages(
    read_csv2(system.file("extdata/guild_metric.csv", package = "EQRfishes"))
  )

calculate_metric_formula <-
  suppressMessages(
    read_csv2(
      system.file("extdata/calculate_metric_formula.csv", package = "EQRfishes")
    )
  )

calculate_metric_measures <-
  suppressMessages(
    read_csv2(
      system.file(
        "extdata/calculate_metric_measures.csv",
        package = "EQRfishes"
      )
    )
  )

calculate_metric_score <-
  suppressMessages(
    read_csv2(
      system.file("extdata/calculate_metric_score.csv", package = "EQRfishes")
    )
  )

# data_taxonmetrics <-
#   suppressMessages(
#     read_csv2(
#       system.file("extdata/data_taxonmetrics.csv", package = "EQRfishes")
#     )
#   )

# data_classes <-
#   suppressMessages(
#     read_csv2(system.file("extdata/data_classes.csv", package = "EQRfishes"))
#   )

calculate_IBI_EQR <-
  suppressMessages(
    read_csv2(
      system.file("extdata/calculate_IBI_EQR.csv", package = "EQRfishes")
    )
  )

# score <-
#   suppressMessages(
#     read_csv2(system.file("extdata/score.csv", package = "EQRfishes"))
#   )

describe("variables exist in dependent tables", {
  it("data_guild.guild -> guild_metric.guild", {
    lacking_vars <-
      unique(data_guild$guild)[
        !unique(data_guild$guild) %in% unique(guild_metric$guild)
      ]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          paste(lacking_vars, collapse = ", "),
          "should be added to column guild in table guild_metric.csv" #nolint
        )
    )
  })
  it("guild_metric.metric_formula_name -> calculate_metric_formula.metric_formula_name", {#nolint
    lacking_vars <-
      unique(guild_metric$metric_formula_name)[
        !unique(guild_metric$metric_formula_name) %in%
          unique(calculate_metric_formula$metric_formula_name)
        ]
    lacking_vars <- lacking_vars[!is.na(lacking_vars)]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          paste(lacking_vars, collapse = ", "),
          "should be added to column metric_formula_name in table calculate_metric_formula.csv" #nolint
        )
    )
  })
  it("guild_metric.metric_measures_name -> calculate_metric_measures.metric_measures_name", {#nolint
    lacking_vars <-
      unique(guild_metric$metric_measures_name)[
        !unique(guild_metric$metric_measures_name) %in%
          unique(calculate_metric_measures$metric_measures_name)
        ]
    lacking_vars <- lacking_vars[!is.na(lacking_vars)]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          paste(lacking_vars, collapse = ", "),
          "should be added to column metric_measures_name in table calculate_metric_measures.csv" #nolint
        )
    )
  })
  it("guild_metric.metric_score_name -> calculate_metric_score.metric_score", {
    lacking_vars <-
      unique(guild_metric$metric_score_name)[
        !unique(guild_metric$metric_score_name) %in%
          unique(calculate_metric_score$metric_score)
        ]
    lacking_vars <- lacking_vars[!is.na(lacking_vars)]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          paste(lacking_vars, collapse = ", "),
          "should be added to column metric_score in table calculate_metric_score.csv" #nolint
        )
    )
  })
  it("calculate_metric_formula.submetric_formula_name -> calculate_metric_formula.metric_formula_name", {#nolint
    lacking_vars <-
      unique(calculate_metric_formula$submetric_formula_name)[
        !unique(calculate_metric_formula$submetric_formula_name) %in%
          unique(calculate_metric_formula$metric_formula_name)
        ]
    lacking_vars <- lacking_vars[!is.na(lacking_vars)]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          paste(lacking_vars, collapse = ", "),
          "should be added to column metric_formula_name in table calculate_metric_formula.csv" #nolint
        )
    )
  })
  it("calculate_metric_formula.submetric_measures_name -> calculate_metric_measures.metric_measures_name", {#nolint
    lacking_vars <-
      unique(calculate_metric_formula$submetric_measures_name)[
        !unique(calculate_metric_formula$submetric_measures_name) %in%
          unique(calculate_metric_measures$metric_measures_name)
        ]
    lacking_vars <- lacking_vars[!is.na(lacking_vars)]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          paste(lacking_vars, collapse = ", "),
          "should be added to column metric_measures_name in table calculate_metric_measures.csv" #nolint
        )
    )
  })
  it("calculate_metric_formula.submetric_score_name -> calculate_metric_score.metric_score", {#nolint
    lacking_vars <-
      unique(calculate_metric_formula$submetric_score_name)[
        !unique(calculate_metric_formula$submetric_score_name) %in%
          unique(calculate_metric_score$metric_score)
        ]
    lacking_vars <- lacking_vars[!is.na(lacking_vars)]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          paste(lacking_vars, collapse = ", "),
          "should be added to column metric_score in table calculate_metric_score.csv" #nolint
        )
    )
  })
  it("calculate_metric_score.metric -> ...", {
    lacking_vars <- calculate_metric_score %>%
      select(.data$metric_score, .data$metric, .data$add_category) %>%
      gather(
        key = "magweg", value = "metric", -.data$metric_score, na.rm = TRUE
      ) %>%
      select(-.data$magweg) %>%
      distinct() %>%
      left_join(
        guild_metric %>%
          transmute(
            metric_score = .data$metric_score_name,
            metric_source =
              ifelse(
                is.na(.data$metric_formula_name),
                .data$metric_measures_name,
                .data$metric_formula_name
              )
          ) %>%
          bind_rows(
            calculate_metric_formula %>%
              transmute(
                metric_score = .data$submetric_score_name,
                metric_source =
                  ifelse(
                    is.na(.data$submetric_formula_name),
                    .data$submetric_measures_name,
                    .data$submetric_formula_name
                  )
              )
          ) %>%
          distinct(),
        by = c("metric_score")
      ) %>%
      filter(is.na(.data$metric_source)) %>%
      transmute(
        metric_for_score =
          paste0(.data$metric, " (for ", .data$metric_score, ")")
      )
    expect_equal(
      nrow(lacking_vars), 0,
      info =
        paste(
          "To calculate the scores in parentheses, the following metrics should be added to columns metric_formula_name or metric_measures_name of table guild_metric.csv or to columns submetric_formula_name or submetric_measures_name of table calculate_metric_formula.csv: ", #nolint
          paste(lacking_vars$metric_for_score, collapse = ", ")
        )
    )
  })
  it("calculate_IBI_EQR.guild <-> guild_metric.guild", {
    lacking_vars <-
      unique(guild_metric$guild)[
        !unique(guild_metric$guild) %in% unique(calculate_IBI_EQR$guild)
        ]
    expect_equal(
      nrow(lacking_vars), 0,
      info =
        paste(
          "Info on calculation of IBI and EQR of guild(s)",
          paste(lacking_vars, collapse = ", "),
          "should be added to column guild in table calculate_IBI_EQR.csv"
        )
    )
    lacking_vars <-
      unique(calculate_IBI_EQR$guild)[
        !unique(calculate_IBI_EQR$guild) %in% unique(guild_metric$guild)
        ]
    expect_equal(
      length(lacking_vars), 0,
      info =
        paste(
          "calculate_IBI_EQR.csv contains information on guild(s)",
          paste(lacking_vars, collapse = ", "),
          ", but there is no information on how to calculate the guild(s) in table guild_metric.csv" #nolint
        )
    )
  })
  it("calculate_IBI_EQR.calculated -> guild_metric.metric_name", {
    lacking_vars <- calculate_IBI_EQR %>%
      select(.data$guild, .data$to_calculate, .data$calculated) %>%
      filter(!(.data$to_calculate == "EQR" & .data$calculated == "IBI")) %>%
      distinct() %>%
      left_join(
        guild_metric %>%
          select(.data$guild, .data$metric_name, .data$metric_score_name) %>%
          distinct(),
        by = c("guild", "calculated" = "metric_name")
      ) %>%
      filter(is.na(.data$metric_score_name))
    expect_equal(
      nrow(lacking_vars), 0,
      info =
        paste0(
          "To calculate the IBI of ",
          lacking_vars$guild,
          ", the metric ",
          lacking_vars$calculated,
          " should be added to the column metric_name of table guild_metric.csv (and calculation rules should be provided in other columns)" #nolint
        )
    )
  })
})

describe("all variables in formulas exist in tables", {
  it("items in formula are added as submetric variable in table calculate_metric_formula.csv", {#nolint

  })
  it("items in calculate_metric_measures.csv exists in table data_taxonmetrics.csv", {#nolint

  })
})

describe("items in calculate_metric_measures.csv have valable names", {
  it("the calculation of metric_type is added to calculate_metric_measures.R", {
    problems <-
      unique(calculate_metric_measures$metric_type)[
        !unique(calculate_metric_measures$metric_type) %in%
          c(
            NA, "number_of_species", "number_of_individuals",
            "number_of_length_classes",
            "sum_of_scored_length_classes",
            "total_weight", "sum_values_column", "shannon_wiener_index",
            "no_metric"
          )
      ]
    expect_equal(
      nrow(problems), 0,
      info =
        paste(
          "No code for calculation is written for:",
          paste(problems, collapse = ", "),
          "(these names are added to column metric_type in table calculate_metric_score.csv)" #nolint
        )
    )
  })
  it("NULL_to_0 only contains 0 and 1", {
    stopifnot(
      all(calculate_metric_measures$NULL_to_0 %in% c(0, 1, NA))
    )
  })
  it("only_individual_measures only contains 0 and 1", {
    stopifnot(
      all(calculate_metric_measures$only_individual_measures %in% c(0, 1, NA))
    )
  })
})

describe("intervals are correct", {
  it("string is correctly noted", {
    wrong_interval <- calculate_metric_score %>%
      select(.data$value_metric) %>%
      distinct() %>%
      mutate(
        operator_min =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\1", .data$value_metric
          ),
        value_min =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\2", .data$value_metric
          ),
        value_max =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\6", .data$value_metric
          ),
        operator_max =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\9", .data$value_metric
          )
      ) %>%
      filter(
        !(.data$operator_min == "]" & .data$value_min == " " &
            is.numeric(as.numeric(.data$value_max)) &
            .data$operator_max %in% c("[", "]")),
        !(.data$operator_min %in% c("[", "]") &
            is.numeric(as.numeric(.data$value_min)) &
            .data$value_max == " " & .data$operator_max == "["),
        !(.data$operator_min %in% c("[", "]") &
            is.numeric(as.numeric(.data$value_min)) &
            is.numeric(as.numeric(.data$value_max)) &
            .data$operator_max %in% c("[", "]") &
            as.numeric(.data$value_min) <= as.numeric(.data$value_max))
      )
    expect_equal(
      nrow(wrong_interval), 0,
      info =
        paste(
          "Column value_metric from calculate_metric_score.csv has invalable interval(s):", #nolint
          paste(wrong_interval$value_metric, collapse = ", ")
        )
    )
    wrong_interval <- calculate_metric_score %>%
      select(.data$value_add_category) %>%
      filter(!is.na(.data$value_add_category)) %>%
      distinct() %>%
      mutate(
        operator_min =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\1", .data$value_add_category
          ),
        value_min =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\2", .data$value_add_category
          ),
        value_max =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\6", .data$value_add_category
          ),
        operator_max =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\9", .data$value_add_category
          )
      ) %>%
      filter(
        !(.data$operator_min == "]" & .data$value_min == " " &
            is.numeric(as.numeric(.data$value_max)) &
            .data$operator_max %in% c("[", "]")),
        !(.data$operator_min %in% c("[", "]") &
            is.numeric(as.numeric(.data$value_min)) &
            .data$value_max == " " & .data$operator_max == "["),
        !(.data$operator_min %in% c("[", "]") &
            is.numeric(as.numeric(.data$value_min)) &
            is.numeric(as.numeric(.data$value_max)) &
            .data$operator_max %in% c("[", "]") &
            as.numeric(.data$value_min) <= as.numeric(.data$value_max))
      )
    expect_equal(
      nrow(wrong_interval), 0,
      info =
        paste(
          "Column value_add_category from calculate_metric_score.csv has invalable interval(s):", #nolint
          paste(wrong_interval$value_add_category, collapse = ", ")
        )
    )
    wrong_interval <- calculate_IBI_EQR %>%
      select(.data$interval) %>%
      distinct() %>%
      mutate(
        operator_min =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\1", .data$interval
          ),
        value_min =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\2", .data$interval
          ),
        value_max =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\6", .data$interval
          ),
        operator_max =
          gsub(
            "^(\\W)((\\d+(.\\d+)?)|\\s)(,)((\\d+(.\\d+)?)|\\s)(\\W)$",
            "\\9", .data$interval
          )
      ) %>%
      filter(
        !(.data$operator_min == "]" & .data$value_min == " " &
            is.numeric(as.numeric(.data$value_max)) &
            .data$operator_max %in% c("[", "]")),
        !(.data$operator_min %in% c("[", "]") &
            is.numeric(as.numeric(.data$value_min)) &
            .data$value_max == " " & .data$operator_max == "["),
        !(.data$operator_min %in% c("[", "]") &
            is.numeric(as.numeric(.data$value_min)) &
            is.numeric(as.numeric(.data$value_max)) &
            .data$operator_max %in% c("[", "]") &
            as.numeric(.data$value_min) <= as.numeric(.data$value_max))
      )
    expect_equal(
      nrow(wrong_interval), 0,
      info =
        paste(
          "Column interval from calculate_IBI_EQR.csv has invalable interval(s):", #nolint
          paste(wrong_interval$interval, collapse = ", ")
        )
    )
  })
  it("intervals cover all possible values", {

  })
})
