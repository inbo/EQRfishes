library(readr)
library(dplyr)
library(tidyr)

data_indextypology <- suppressMessages(
  read_csv2(
    system.file("extdata/data_indextypology.csv", package = "EQRfishes")
  )
)

indextypology_metric <- suppressMessages(
  read_csv2(
    system.file("extdata/indextypology_metric.csv", package = "EQRfishes")
  )
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

calculate_ibi_eqr <-
  suppressMessages(
    read_csv2(
      system.file("extdata/calculate_ibi_eqr.csv", package = "EQRfishes")
    )
  )

# score <-
#   suppressMessages(
#     read_csv2(system.file("extdata/score.csv", package = "EQRfishes"))
#   )

test_that("variables exist in dependent tables: data_indextypology.indextypology -> indextypology_metric.indextypology", {  # nolint: line_length_linter
  lacking_vars <-
    unique(data_indextypology$indextypology)[
      !unique(data_indextypology$indextypology) %in%
        unique(indextypology_metric$indextypology)
    ]
  lacking_vars <- lacking_vars[lacking_vars != "stilstaand"]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        paste(lacking_vars, collapse = ", "),
        "should be added to column indextypology in table indextypology_metric.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: indextypology_metric.metric_formula_name -> calculate_metric_formula.metric_formula_name", { # nolint: line_length_linter
  lacking_vars <- unique(indextypology_metric$metric_formula_name)[
    !unique(indextypology_metric$metric_formula_name) %in%
      unique(calculate_metric_formula$metric_formula_name)
  ]
  lacking_vars <- lacking_vars[!is.na(lacking_vars)]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        paste(lacking_vars, collapse = ", "),
        "should be added to column metric_formula_name in table calculate_metric_formula.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: indextypology_metric.metric_measures_name -> calculate_metric_measures.metric_measures_name", { # nolint: line_length_linter
  lacking_vars <- unique(indextypology_metric$metric_measures_name)[
    !unique(indextypology_metric$metric_measures_name) %in%
      unique(calculate_metric_measures$metric_measures_name)
  ]
  lacking_vars <- lacking_vars[!is.na(lacking_vars)]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        paste(lacking_vars, collapse = ", "),
        "should be added to column metric_measures_name in table calculate_metric_measures.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: indextypology_metric.metric_score_name -> calculate_metric_score.metric_score", { # nolint: line_length_linter
  lacking_vars <- unique(indextypology_metric$metric_score_name)[
    !unique(indextypology_metric$metric_score_name) %in%
      unique(calculate_metric_score$metric_score)
  ]
  lacking_vars <- lacking_vars[!is.na(lacking_vars)]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        paste(lacking_vars, collapse = ", "),
        "should be added to column metric_score in table calculate_metric_score.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: calculate_metric_formula.submetric_formula_name -> calculate_metric_formula.metric_formula_name", { # nolint: line_length_linter
  lacking_vars <- unique(calculate_metric_formula$submetric_formula_name)[
    !unique(calculate_metric_formula$submetric_formula_name) %in%
      unique(calculate_metric_formula$metric_formula_name)
  ]
  lacking_vars <- lacking_vars[!is.na(lacking_vars)]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        paste(lacking_vars, collapse = ", "),
        "should be added to column metric_formula_name in table calculate_metric_formula.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: calculate_metric_formula.submetric_measures_name -> calculate_metric_measures.metric_measures_name", { # nolint: line_length_linter
  lacking_vars <- unique(calculate_metric_formula$submetric_measures_name)[
    !unique(calculate_metric_formula$submetric_measures_name) %in%
      unique(calculate_metric_measures$metric_measures_name)
  ]
  lacking_vars <- lacking_vars[!is.na(lacking_vars)]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        paste(lacking_vars, collapse = ", "),
        "should be added to column metric_measures_name in table calculate_metric_measures.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: calculate_metric_formula.submetric_score_name -> calculate_metric_score.metric_score", { # nolint: line_length_linter
  lacking_vars <- unique(calculate_metric_formula$submetric_score_name)[
    !unique(calculate_metric_formula$submetric_score_name) %in%
      unique(calculate_metric_score$metric_score)
  ]
  lacking_vars <- lacking_vars[!is.na(lacking_vars)]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        paste(lacking_vars, collapse = ", "),
        "should be added to column metric_score in table calculate_metric_score.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: calculate_metric_score.metric -> ...", {  # nolint: line_length_linter
  lacking_vars <- calculate_metric_score %>%
    select("metric_score", "metric", "add_category") %>%
    gather(
      key = "magweg", value = "metric", -"metric_score", na.rm = TRUE
    ) %>%
    select(-"magweg") %>%
    distinct() %>%
    left_join(
      indextypology_metric %>%
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
      by = c("metric_score"),
      relationship = "many-to-many"
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
        "To calculate the scores in parentheses, the following metrics should be added to columns metric_formula_name or metric_measures_name of table indextypology_metric.csv or to columns submetric_formula_name or submetric_measures_name of table calculate_metric_formula.csv: ", # nolint: line_length_linter
        paste(lacking_vars$metric_for_score, collapse = ", ")
      )
  )
})
test_that("variables exist in dependent tables: calculate_ibi_eqr.indextypology <-> indextypology_metric.indextypology", {  # nolint: line_length_linter
  lacking_vars <- unique(calculate_ibi_eqr$indextypology)[
    !unique(calculate_ibi_eqr$indextypology) %in%
      unique(indextypology_metric$indextypology)
  ]
  expect_equal(
    length(lacking_vars), 0,
    info =
      paste(
        "calculate_ibi_eqr.csv contains information on indextypology(s)",
        paste(lacking_vars, collapse = ", "),
        ", but there is no information on how to calculate the indextypology(s) in table indextypology_metric.csv" # nolint: line_length_linter
      )
  )
})
test_that("variables exist in dependent tables: calculate_ibi_eqr.calculated -> indextypology_metric.metric_name", {  # nolint: line_length_linter
  lacking_vars <- calculate_ibi_eqr %>%
    select("indextypology", "to_calculate", "calculated") %>%
    filter(!(.data$to_calculate == "EQR" & .data$calculated == "IBI")) %>%
    distinct() %>%
    left_join(
      indextypology_metric %>%
        select("indextypology", "metric_name", "metric_measures_name") %>%
        distinct(),
      by = c("indextypology", "calculated" = "metric_name")
    ) %>%
    filter(
      is.na(.data$metric_measures_name) &
        .data$indextypology != "(undetermined)"
    )
  expect_equal(
    nrow(lacking_vars), 0,
    info =
      paste0(
        "To calculate the IBI of ",
        lacking_vars$indextypology,
        ", the metric ",
        lacking_vars$calculated,
        " should be added to the column metric_name of table indextypology_metric.csv (and calculation rules should be provided in other columns)" # nolint: line_length_linter
      )
  )
})

test_that("all variables in formulas exist in tables: items in formula are added as submetric variable in table calculate_metric_formula.csv", { # nolint: line_length_linter

})

test_that("A denominator of zero in formulas in table calculate_metric_formula.csv will not cause problems", { # nolint: line_length_linter
  test <- calculate_metric_formula |>
    distinct(formula) |>
    mutate(
      denominator = gsub("^.* \\/ (\\w*)(, 0)?\\)?$", "\\1", formula),
      zerocheck1 =
        gsub("^ifelse\\((\\w*)\\s[=!]\\=\\s0[, ].*\\)$", "\\1", formula),
      zerocheck2 = gsub(
        "^ifelse\\(.*[|&]\\s(\\w*)\\s[=!]\\=\\s0\\,\\s.*\\)$", "\\1", formula
      )
    ) |>
    filter(
      denominator != zerocheck1, denominator != zerocheck2,
      denominator != "3",
      # the following formulas cause no problems because the validation drops
      # an error if n_fyke_nets and n_days are not positive integers
      !grepl("MniInd_\\w* \\/ n_fyke_days", formula),
      # the following gives a warning and a high score for ManBio in canals:
      formula != "TotWeight_canals / surface"
    )
  stopifnot(nrow(test) == 0)
})

test_that("all variables in formulas exist in tables: items in calculate_metric_measures.csv exists in table data_taxonmetrics.csv", { # nolint: line_length_linter

})

test_that("items in calculate_metric_measures.csv have valable names: the calculation of metric_type is added to calculate_metric_measures.R", {  # nolint: line_length_linter
  problems <-
    unique(calculate_metric_measures$metric_type)[
      !unique(calculate_metric_measures$metric_type) %in% c(
        NA, "number_of_species", "number_of_individuals",
        "number_of_length_classes",
        "sum_of_scored_length_classes",
        "total_weight", "sum_values_column", "shannon_wiener_index",
        "no_metric"
      )
    ]
  expect_equal(
    nrow(problems), NULL,
    info =
      paste(
        "No code for calculation is written for:",
        paste(problems, collapse = ", "),
        "(these names are added to column metric_type in table calculate_metric_score.csv)" # nolint: line_length_linter
      )
  )
})
test_that("items in calculate_metric_measures.csv have valable names: null_to_0 only contains 0 and 1", {  #nolint: line_length_linter
  stopifnot(
    all(calculate_metric_measures$null_to_0 %in% c(0, 1, NA))
  )
})
test_that("items in calculate_metric_measures.csv have valable names: only_individual_measures only contains 0 and 1", {  # nolint: line_length_linter
  stopifnot(
    all(calculate_metric_measures$only_individual_measures %in% c(0, 1, NA))
  )
})

test_that("intervals are correct: string is correctly noted", {
  wrong_interval <- calculate_metric_score %>%
    select("value_metric") %>%
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
        "Column value_metric from calculate_metric_score.csv has invalable interval(s):", # nolint: line_length_linter
        paste(wrong_interval$value_metric, collapse = ", ")
      )
  )
  wrong_interval <- calculate_metric_score %>%
    select("value_add_category") %>%
    filter(
      !is.na(.data$value_add_category),
      !.data$value_add_category %in% c("spring", "summer", "autumn")
    ) %>%
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
        "Column value_add_category from calculate_metric_score.csv has invalable interval(s):", # nolint: line_length_linter
        paste(wrong_interval$value_add_category, collapse = ", ")
      )
  )
  wrong_interval <- calculate_ibi_eqr %>%
    select("interval") %>%
    distinct() %>%
    filter(!is.na(.data$interval) & !.data$interval %in% c("-1")) %>%
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
        "Column interval from calculate_ibi_eqr.csv has invalable interval(s):", # nolint: line_length_linter
        paste(wrong_interval$interval, collapse = ", ")
      )
  )
})
test_that("intervals are correct: intervals cover all possible values", {

})
