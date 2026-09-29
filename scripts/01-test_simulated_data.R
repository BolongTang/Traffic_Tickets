#### Preamble ####
# Purpose: Tests the structure and validity of the simulated Toronto
#   traffic tickets dataset, using the `testthat` package.
# Author: Bolong Tang
# Date: 28 September 2026
# License: MIT
# Pre-requisites:
  # - The `tidyverse` and `testthat` packages must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
library(tidyverse)
library(testthat)

analysis_data <- read_csv("data/00-simulated_data/simulated_data.csv")


#### Test data ####

test_that("dataset loads and has the expected dimensions", {
  expect_true(exists("analysis_data"))
  expect_equal(ncol(analysis_data), 10)
  # 12 years x 4,200 rows/year, as set in 00-simulate_data.R
  expect_equal(nrow(analysis_data), 12 * 4200)
})

test_that("column names match the expected schema", {
  expected_cols <- c(
    "X_id", "INDEX_", "OFFENCE_YEAR", "DIVISION", "TICKET_TYPE",
    "OFFENCE_CATEGORY", "AGE_GROUP", "HOOD_158", "NEIGHBOURHOOD_158",
    "TICKET_COUNT"
  )
  expect_setequal(colnames(analysis_data), expected_cols)
})

test_that("'X_id' and 'INDEX_' are unique row identifiers", {
  expect_equal(n_distinct(analysis_data$X_id), nrow(analysis_data))
  expect_equal(n_distinct(analysis_data$INDEX_), nrow(analysis_data))
})

test_that("'OFFENCE_YEAR' falls within the simulated range 2014-2025", {
  expect_true(all(analysis_data$OFFENCE_YEAR %in% 2014:2025))
})

test_that("'DIVISION' contains only valid division codes", {
  valid_divisions <- c(
    "D11", "D12", "D13", "D14", "D22", "D23", "D31", "D32",
    "D33", "D41", "D42", "D43", "D51", "D52", "D53", "D55", "NSA"
  )
  expect_true(all(analysis_data$DIVISION %in% valid_divisions))
})

test_that("'TICKET_TYPE' contains only the two valid ticket types", {
  valid_ticket_types <- c(
    "Prov Offence Notice - Part I (Pot)",
    "Prov Offence Summons Part III Form 104"
  )
  expect_true(all(analysis_data$TICKET_TYPE %in% valid_ticket_types))
})

test_that("'OFFENCE_CATEGORY' contains only the eight valid categories", {
  valid_categories <- c(
    "Aggressive Driving", "All CAIA", "Distracted Driving",
    "Document Violations", "Equipment Violations", "Moving Violations",
    "Other HTA", "Speeding"
  )
  expect_true(all(analysis_data$OFFENCE_CATEGORY %in% valid_categories))
  # not degenerate -- at least two categories actually occur
  expect_gte(n_distinct(analysis_data$OFFENCE_CATEGORY), 2)
})

test_that("'AGE_GROUP' contains only valid age groups", {
  expect_true(all(analysis_data$AGE_GROUP %in% c("Adult", "Youth", "Unknown")))
})

test_that("'TICKET_COUNT' is strictly positive and integer-valued", {
  expect_true(all(analysis_data$TICKET_COUNT > 0))
  expect_true(all(analysis_data$TICKET_COUNT == round(analysis_data$TICKET_COUNT)))
})

test_that("the dataset has no missing values or empty strings", {
  expect_true(all(!is.na(analysis_data)))

  char_cols <- c(
    "DIVISION", "TICKET_TYPE", "OFFENCE_CATEGORY", "AGE_GROUP",
    "HOOD_158", "NEIGHBOURHOOD_158"
  )
  expect_true(all(sapply(analysis_data[char_cols], function(x) all(x != ""))))
})

test_that("'HOOD_158' and 'NEIGHBOURHOOD_158' map 1-to-1", {
  hood_map <- analysis_data |> distinct(HOOD_158, NEIGHBOURHOOD_158)
  expect_equal(n_distinct(hood_map$HOOD_158), nrow(hood_map))
  expect_equal(n_distinct(hood_map$NEIGHBOURHOOD_158), nrow(hood_map))
})


#### Test the simulated interactions ####
# These checks confirm the interactions built into 00-simulate_data.R
# actually show up in the output, rather than testing structure alone.

test_that("Aggressive Driving's share rises in later years, as simulated", {
  early_share <- analysis_data |>
    filter(OFFENCE_YEAR %in% 2014:2016) |>
    summarise(share = mean(OFFENCE_CATEGORY == "Aggressive Driving")) |>
    pull(share)

  late_share <- analysis_data |>
    filter(OFFENCE_YEAR %in% 2023:2025) |>
    summarise(share = mean(OFFENCE_CATEGORY == "Aggressive Driving")) |>
    pull(share)

  expect_gt(late_share, early_share)
})

test_that("NSA has a higher Speeding share than other divisions, as simulated", {
  nsa_share <- analysis_data |>
    filter(DIVISION == "NSA") |>
    summarise(share = mean(OFFENCE_CATEGORY == "Speeding")) |>
    pull(share)

  other_share <- analysis_data |>
    filter(DIVISION != "NSA") |>
    summarise(share = mean(OFFENCE_CATEGORY == "Speeding")) |>
    pull(share)

  expect_gt(nsa_share, other_share)
})

