#### Preamble ####
# Purpose: Tests the structure and validity of the cleaned Toronto
#   traffic tickets analysis dataset, including the correctness of the
#   variables constructed in 03-clean_data.R, using the `testthat`
#   package.
# Author: Bolong Tang
# Date: 28 September 2026
# License: MIT
# Pre-requisites:
  # - The `tidyverse` and `testthat` packages must be installed and loaded
  # - 03-clean_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
library(tidyverse)
library(testthat)

analysis_data <- read_csv("data/03-analysis_data/analysis_data.csv")


#### Test data ####

test_that("dataset loads and has the expected dimensions", {
  expect_true(exists("analysis_data"))
  # Row count must be unchanged from the raw download -- cleaning
  # should never drop or duplicate rows
  expect_equal(nrow(analysis_data), 51352)
  # 10 original columns + 3 constructed
  expect_equal(ncol(analysis_data), 13)
})

test_that("column names match the schema produced by 03-clean_data.R", {
  expected_cols <- c(
    "row_id", "ticket_index", "offence_year", "division", "ticket_type",
    "offence_category", "age_group", "hood_158", "neighbourhood_158",
    "ticket_count", "offence_group", "period", "has_neighbourhood"
  )
  expect_setequal(colnames(analysis_data), expected_cols)
})

test_that("'row_id' and 'ticket_index' are unique row identifiers", {
  expect_equal(n_distinct(analysis_data$row_id), nrow(analysis_data))
  expect_equal(n_distinct(analysis_data$ticket_index), nrow(analysis_data))
})

test_that("'offence_year' falls within the observed range 2014-2025", {
  expect_true(all(analysis_data$offence_year %in% 2014:2025))
})

test_that("'division' contains only valid division codes", {
  valid_divisions <- c(
    "D11", "D12", "D13", "D14", "D22", "D23", "D31", "D32",
    "D33", "D41", "D42", "D43", "D51", "D52", "D53", "D55", "NSA"
  )
  expect_true(all(analysis_data$division %in% valid_divisions))
})

test_that("'ticket_type' was successfully trimmed and recoded", {
  expect_true(all(analysis_data$ticket_type %in% c("Part I notice", "Part III summons")))
  expect_true(all(analysis_data$ticket_type == str_trim(analysis_data$ticket_type)))
})

test_that("'offence_category' contains only the eight valid categories", {
  valid_categories <- c(
    "Aggressive Driving", "All CAIA", "Distracted Driving",
    "Document Violations", "Equipment Violations", "Moving Violations",
    "Other HTA", "Speeding"
  )
  expect_true(all(analysis_data$offence_category %in% valid_categories))
})

test_that("'age_group' contains only valid age groups", {
  expect_true(all(analysis_data$age_group %in% c("Adult", "Youth", "Unknown")))
})

test_that("'ticket_count' is strictly positive and integer-valued", {
  expect_true(all(analysis_data$ticket_count > 0))
  expect_true(all(analysis_data$ticket_count == round(analysis_data$ticket_count)))
})

test_that("the dataset has no missing values or empty strings", {
  expect_true(all(!is.na(analysis_data)))

  char_cols <- c(
    "division", "ticket_type", "offence_category", "age_group",
    "hood_158", "neighbourhood_158", "offence_group", "period"
  )
  expect_true(all(sapply(analysis_data[char_cols], function(x) all(x != ""))))
})


#### Test the constructed variables ####
# These checks confirm 'offence_group', 'period', and
# 'has_neighbourhood' were derived correctly, not just that they exist.

test_that("'offence_group' is derived correctly from 'offence_category'", {
  behavioural_categories <- c(
    "Aggressive Driving", "Speeding", "Distracted Driving", "Moving Violations"
  )

  offence_group_correct <- analysis_data |>
    mutate(
      expected_group = if_else(
        offence_category %in% behavioural_categories, "Behavioural", "Administrative"
      )
    ) |>
    summarise(all_correct = all(offence_group == expected_group)) |>
    pull(all_correct)

  expect_true(offence_group_correct)
})

test_that("'period' is derived correctly from 'offence_year'", {
  period_correct <- analysis_data |>
    mutate(
      expected_period = case_when(
        offence_year <= 2019 ~ "pre_2020",
        offence_year >= 2022 ~ "post_2021",
        TRUE ~ "2020_2021"
      )
    ) |>
    summarise(all_correct = all(period == expected_period)) |>
    pull(all_correct)

  expect_true(period_correct)
})

test_that("'has_neighbourhood' correctly flags NSA rows", {
  has_neighbourhood_correct <- analysis_data |>
    summarise(all_correct = all(has_neighbourhood == (hood_158 != "NSA"))) |>
    pull(all_correct)

  expect_true(has_neighbourhood_correct)
})

test_that("'hood_158' and 'neighbourhood_158' map 1-to-1", {
  hood_map <- analysis_data |> distinct(hood_158, neighbourhood_158)
  expect_equal(n_distinct(hood_map$hood_158), nrow(hood_map))
  expect_equal(n_distinct(hood_map$neighbourhood_158), nrow(hood_map))
})

