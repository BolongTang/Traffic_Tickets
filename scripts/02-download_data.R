#### Preamble ####
# Purpose: Download the Traffic Tickets Issued dataset from the City of
#   Toronto's Open Data Portal, using opendatatoronto (Gelfand 2022), and
#   save an unedited copy for downstream cleaning.
# Author: Bolong Tang
# Date: 28 September 2026
# License: MIT
# Pre-requisites: install.packages("opendatatoronto"); install.packages("tidyverse")
# Any other information needed? Requires an internet connection.

#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)

#### Locate the package ####
# Package ID confirmed directly from the dataset's page on
# open.toronto.ca ("Police Annual Statistical Report - Tickets Issued").
# Hard-coding the confirmed ID (rather than searching by title) makes
# this reproducible even if the dataset's title changes later.
package_id <- "2dc66a14-7ea0-494c-8ea3-2680332cc7cb"

#### Locate and download the resource ####
# Pass the ID string directly rather than a show_package() object:
# list_package_resources() expects a 1-row data frame or a length-1
# character vector, and show_package()'s return type has varied across
# opendatatoronto versions, so the ID string is the more robust input.
resources <- list_package_resources(package_id)
resources |> select(name, format)

# Confirmed resource list has exactly one CSV-format row
# ("Tickets Issued.csv"), alongside JSON/XML/JSON duplicates -- filter
# to it directly rather than string-matching on the name.
resource_to_get <- resources |>
  filter(format == "CSV") |>
  slice(1)

raw_tickets <- get_resource(resource_to_get)

# get_resource() can return a list (one element per sheet/table) for
# some formats; take the first table if so.
if (is.list(raw_tickets) && !is.data.frame(raw_tickets)) {
  raw_tickets <- raw_tickets[[1]]
}

#### Save unedited copy ####
dir.create("data/raw_data", recursive = TRUE, showWarnings = FALSE)
write_csv(raw_tickets, "data/01-raw_data/unedited_traffic_tickets.csv")

# Quick sanity check on what was saved
glimpse(raw_tickets)

