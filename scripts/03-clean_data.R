#### Preamble ####
# Purpose: Clean the raw Toronto traffic tickets dataset into an
#   analysis-ready table: tidy column names, trimmed strings, and two
#   constructed variables (a behavioural/administrative offence grouping
#   and a pre-/post-pandemic period flag) that are used throughout the
#   paper's data and results sections.
# Author: Bolong Tang
# Date: 28 September 2026
# License: MIT
# Pre-requisites:
  # - The `tidyverse` package must be installed and loaded
  # - 02-download_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj

#### Workspace setup ####
library(tidyverse)

raw_data <- read_csv("data/01-raw_data/unedited_traffic_tickets.csv")

#### Clean data ####

cleaned_data <- raw_data |>
  # Tidy column names: lower_snake_case, and drop the trailing
  # underscore that Open Data Toronto's export leaves on a couple of
  # fields (e.g. INDEX_).
  rename(
    row_id             = X_id,
    ticket_index       = INDEX_,
    offence_year       = OFFENCE_YEAR,
    division           = DIVISION,
    ticket_type        = TICKET_TYPE,
    offence_category   = OFFENCE_CATEGORY,
    age_group          = AGE_GROUP,
    hood_158           = HOOD_158,
    neighbourhood_158  = NEIGHBOURHOOD_158,
    ticket_count       = TICKET_COUNT
  ) |>
  mutate(
    # The raw TICKET_TYPE field carries long trailing whitespace on
    # every value (a quirk of the source export) -- trim it so the two
    # ticket types are compared and grouped correctly downstream.
    ticket_type = str_trim(ticket_type),
    # Recode to short, analysis-friendly labels while preserving the
    # original meaning.
    ticket_type = case_when(
      str_detect(ticket_type, "Part I \\(Pot\\)") ~ "Part I notice",
      str_detect(ticket_type, "Part III")         ~ "Part III summons",
      TRUE ~ ticket_type
    ),
    # Constructed variable: behavioural offences (those directly about
    # how a vehicle was driven) vs. administrative offences (documents,
    # equipment, insurance, other HTA). This split is used throughout
    # the data and results sections to separate genuine driving-
    # behaviour signal from enforcement-intensity effects.
    offence_group = if_else(
      offence_category %in% c(
        "Aggressive Driving", "Speeding",
        "Distracted Driving", "Moving Violations"
      ),
      "Behavioural",
      "Administrative"
    ),
    # Constructed variable: a coarse period flag used to compare
    # pre-pandemic and post-pandemic years while excluding 2020-2021,
    # which are affected by lockdown-era traffic volumes and are not
    # comparable to either period.
    period = case_when(
      offence_year <= 2019 ~ "pre_2020",
      offence_year >= 2022 ~ "post_2021",
      TRUE ~ "2020_2021"
    ),
    # HOOD_158 == "NSA" marks tickets that cannot be tied to a specific
    # neighbourhood (e.g. some highway/automated enforcement). Keep
    # these rows -- excluding them would understate total tickets --
    # but flag them so neighbourhood-level analysis can exclude them
    # explicitly and deliberately.
    has_neighbourhood = hood_158 != "NSA"
  )

#### Validate cleaned data ####
# A lightweight sanity check here (not a substitute for
# 04-test_analysis_data.R) -- catches an obviously broken cleaning
# step before the file is even written.
stopifnot(
  nrow(cleaned_data) == nrow(raw_data),
  all(!is.na(cleaned_data$offence_group)),
  all(!is.na(cleaned_data$period)),
  all(cleaned_data$ticket_type %in% c("Part I notice", "Part III summons"))
)

#### Save cleaned data ####
dir.create("data/03-analysis_data", recursive = TRUE, showWarnings = FALSE)
write_csv(cleaned_data, "data/03-analysis_data/analysis_data.csv")

glimpse(cleaned_data)

