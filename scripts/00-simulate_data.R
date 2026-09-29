#### Preamble ####
# Purpose: Simulate a dataset resembling the City of Toronto's Traffic
#   Tickets Issued open dataset, with realistic dependence between year,
#   neighbourhood type, offence category, ticket type, and ticket count,
#   so the download/cleaning pipeline can be developed and tested before
#   the real data is pulled.
# Author: Bolong Tang
# Date: 28 September 2026
# License: MIT
# Pre-requisites: none
# Any other information needed? None.

#### Workspace setup ####
library(tidyverse)

set.seed(853) # arbitrary, fixed seed for reproducibility

#### Define the structural "world" the simulation draws from ####

n_years <- 2014:2025

# Divisions: 16 real Toronto Police divisions + NSA ("no specific address",
# used for offences that cannot be tied to a division, e.g. some highway/
# automated enforcement).
divisions <- c(
  "D11", "D12", "D13", "D14", "D22", "D23", "D31", "D32",
  "D33", "D41", "D42", "D43", "D51", "D52", "D53", "D55", "NSA"
)

# A small set of simulated neighbourhoods, each tagged with a "profile"
# that drives the offence-category mix below. This mirrors the real
# pattern in which wide, high-speed arterial/suburban roads see more
# speeding, while dense, congested downtown neighbourhoods see more
# aggressive-driving incidents.
neighbourhoods <- tibble(
  neighbourhood_158 = c(
    "Simulated Heights", "Simulated Clairville", "Simulated Malvern",
    "Simulated Morningside", "Simulated Bendale", "Simulated Hill",
    "Simulated Wellington Place", "Simulated Bay Corridor",
    "Simulated Moss Park", "Simulated Liberty Village",
    "Simulated Parkdale", "Simulated Junction", "Simulated Annex",
    "Simulated Rosedale", "Simulated Weston", "Simulated Oakwood"
  ),
  hood_158 = sprintf("%03d", 1:16),
  profile = c(
    rep("suburban_arterial", 6),
    rep("urban_core", 4),
    rep("mixed", 6)
  )
)

offence_categories <- c(
  "Aggressive Driving", "All CAIA", "Distracted Driving",
  "Document Violations", "Equipment Violations", "Moving Violations",
  "Other HTA", "Speeding"
)

ticket_types <- c(
  "Prov Offence Notice - Part I (Pot)",
  "Prov Offence Summons Part III Form 104"
)

age_groups <- c("Adult", "Youth", "Unknown")

#### Simulate one row per (year, division, neighbourhood, category,
#### ticket_type, age_group) combination that plausibly occurs, each with
#### its own TICKET_COUNT ####

# Number of "event rows" to simulate per year -- mirrors the real data
# having roughly 3,600-4,900 rows per year.
rows_per_year <- 4200

sim_base <- expand_grid(offence_year = n_years) |>
  slice(rep(1:n(), each = rows_per_year)) |>
  mutate(
    neighbourhood_draw = sample(1:nrow(neighbourhoods), n(), replace = TRUE)
  ) |>
  mutate(neighbourhoods[neighbourhood_draw, ])

# --- Interaction 1: category probabilities depend on neighbourhood profile
# and drift over time (aggressive driving and distracted driving trend up
# in later years; speeding spikes in 2020-2021, then reverts). ---
category_probs <- function(profile, year) {
  base <- c(
    "Aggressive Driving"   = 0.16,
    "All CAIA"              = 0.09,
    "Distracted Driving"    = 0.06,
    "Document Violations"   = 0.20,
    "Equipment Violations"  = 0.07,
    "Moving Violations"     = 0.06,
    "Other HTA"             = 0.06,
    "Speeding"              = 0.30
  )

  if (profile == "suburban_arterial") {
    base["Speeding"] <- base["Speeding"] + 0.15
    base["Aggressive Driving"] <- base["Aggressive Driving"] - 0.06
  } else if (profile == "urban_core") {
    base["Aggressive Driving"] <- base["Aggressive Driving"] + 0.18
    base["Speeding"] <- base["Speeding"] - 0.15
  }

  # Time trend: aggressive + distracted driving share rises after 2022;
  # speeding gets a pandemic-year bump in 2020-2021.
  drift <- max(0, year - 2022) * 0.02
  base["Aggressive Driving"] <- base["Aggressive Driving"] + drift
  base["Distracted Driving"] <- base["Distracted Driving"] + drift * 0.5

  if (year %in% c(2020, 2021)) {
    base["Speeding"] <- base["Speeding"] + 0.20
  }

  base <- pmax(base, 0.01) # keep all categories possible
  base / sum(base)
}

sim_base <- sim_base |>
  rowwise() |>
  mutate(
    offence_category = sample(
      offence_categories,
      size = 1,
      prob = category_probs(profile, offence_year)
    )
  ) |>
  ungroup()

# --- Interaction 2: ticket type depends on category. Administrative /
# insurance categories (Other HTA, CAIA, Document Violations) are more
# likely to be escalated to a Part III summons than moving offences. ---
sim_base <- sim_base |>
  rowwise() |>
  mutate(
    ticket_type = sample(
      ticket_types,
      size = 1,
      prob = case_when(
        offence_category == "Other HTA" ~ c(0.35, 0.65),
        offence_category == "All CAIA" ~ c(0.85, 0.15),
        offence_category == "Document Violations" ~ c(0.89, 0.11),
        TRUE ~ c(0.97, 0.03)
      )
    )
  ) |>
  ungroup()

# --- Interaction 3: division is drawn conditionally on neighbourhood
# profile (rough approximation -- real division boundaries are more
# complex, but the direction is what matters for testing downstream code):
# NSA is more likely for Speeding (automated/highway enforcement), other
# divisions drawn uniformly otherwise. ---
sim_base <- sim_base |>
  rowwise() |>
  mutate(
    division = if (offence_category == "Speeding" && runif(1) < 0.25) {
      "NSA"
    } else {
      sample(setdiff(divisions, "NSA"), 1)
    }
  ) |>
  ungroup()

# --- Interaction 4: age group, overwhelmingly Adult, as in the real data. ---
sim_base <- sim_base |>
  mutate(
    age_group = sample(
      age_groups,
      size = n(),
      replace = TRUE,
      prob = c(0.992, 0.006, 0.002)
    )
  )

# --- Interaction 5: TICKET_COUNT, right-skewed with a heavier tail for
# Speeding (automated enforcement can generate very large single-row
# counts) and a mild upward drift by year (more proactive enforcement in
# later years). ---
sim_base <- sim_base |>
  rowwise() |>
  mutate(
    count_mu = case_when(
      offence_category == "Speeding" & division == "NSA" ~ 120,
      offence_category == "Speeding" ~ 40,
      TRUE ~ 15
    ) * (1 + 0.01 * max(0, offence_year - 2022)),
    ticket_count = rnbinom(1, mu = count_mu, size = 1.1) + 1
  ) |>
  ungroup() |>
  select(-count_mu, -neighbourhood_draw, -profile)

#### Assemble final simulated dataset, matching the real column names ####
simulated_data <- sim_base |>
  mutate(
    x_id = row_number(),
    index_ = sample(1:n(), n())
  ) |>
  select(
    x_id, index_,
    offence_year, division, ticket_type, offence_category,
    age_group, hood_158, neighbourhood_158, ticket_count
  ) |>
  rename(
    X_id = x_id,
    INDEX_ = index_,
    OFFENCE_YEAR = offence_year,
    DIVISION = division,
    TICKET_TYPE = ticket_type,
    OFFENCE_CATEGORY = offence_category,
    AGE_GROUP = age_group,
    HOOD_158 = hood_158,
    NEIGHBOURHOOD_158 = neighbourhood_158,
    TICKET_COUNT = ticket_count
  )

#### Save simulated data ####
write_csv(simulated_data, "simulated_tickets.csv")
