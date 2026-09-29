#### Preamble ####
# Purpose: Explore the cleaned Toronto traffic tickets dataset and
#   produce the ggplot2 figures and tinytable tables used in the
#   paper's data section. Figures are saved as PNG to
#   outputs/figures/, and the underlying summarised tables are saved
#   as CSV to outputs/tables/ so paper.qmd can rebuild the tinytable
#   objects without re-running this whole script. Styling is
#   deliberately minimal and grayscale throughout, matching paper.qmd:
#   the multi-category line plot distinguishes categories by shape and
#   linetype rather than colour, and both stacked/segmented bar plots
#   use a grayscale gradient fill rather than arbitrary hues.
# Author: Bolong Tang
# Date: 28 September 2026
# License: MIT
# Pre-requisites:
  # - The `tidyverse`, `tinytable`, and `scales` packages must be
  #   installed and loaded
  # - 03-clean_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj

#### Workspace setup ####
library(tidyverse)
library(tinytable)
library(scales)

analysis_data <- read_csv("data/03-analysis_data/analysis_data.csv")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Shared theme so every figure looks consistent
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    )
)


#### Figure: distribution of TICKET_COUNT (actual row-level data) ####
# Each row already represents an aggregated count of tickets for one
# combination of year/division/category/etc.; this plots that
# row-level distribution directly (not a further summary of it),
# showing how right-skewed and heavy-tailed it is.

fig_ticket_count_dist <- analysis_data |>
  ggplot(aes(x = ticket_count)) +
  geom_histogram(bins = 50, fill = "grey35", colour = "white") +
  scale_x_log10(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    x = "Tickets recorded in a single row (log scale)",
    y = "Number of rows",
    title = NULL
  )

ggsave(
  "outputs/figures/fig-ticket-count-distribution.png",
  fig_ticket_count_dist,
  width = 7, height = 4, dpi = 300
)


#### Table: summary statistics for TICKET_COUNT ####

table_ticket_count_summary <- analysis_data |>
  summarise(
    Minimum = min(ticket_count),
    Median  = median(ticket_count),
    Mean    = mean(ticket_count),
    Maximum = max(ticket_count),
    `Std. dev.` = sd(ticket_count)
  ) |>
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") |>
  mutate(Value = comma(Value, accuracy = 0.1))

write_csv(table_ticket_count_summary, "outputs/tables/table-ticket-count-summary.csv")

tt(table_ticket_count_summary)


#### Overview figures: raw distribution across categorical variables ####

fig_category_overview <- analysis_data |>
  group_by(offence_category) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  ggplot(aes(x = tickets, y = fct_reorder(offence_category, tickets))) +
  geom_col(fill = "grey35") +
  scale_x_continuous(labels = comma) +
  labs(x = "Total tickets, 2014-2025", y = NULL)

ggsave(
  "outputs/figures/fig-category-overview.png",
  fig_category_overview,
  width = 7, height = 4, dpi = 300
)

fig_division_overview <- analysis_data |>
  group_by(division) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  ggplot(aes(x = tickets, y = fct_reorder(division, tickets))) +
  geom_col(fill = "grey35") +
  scale_x_continuous(labels = comma) +
  labs(x = "Total tickets, 2014-2025", y = NULL)

ggsave(
  "outputs/figures/fig-division-overview.png",
  fig_division_overview,
  width = 7, height = 4, dpi = 300
)

fig_age_overview <- analysis_data |>
  group_by(age_group) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  ggplot(aes(x = tickets, y = fct_reorder(age_group, tickets))) +
  geom_col(fill = "grey35") +
  scale_x_continuous(labels = comma) +
  labs(x = "Total tickets, 2014-2025", y = NULL)

ggsave(
  "outputs/figures/fig-age-overview.png",
  fig_age_overview,
  width = 7, height = 4, dpi = 300
)

fig_tickettype_overview <- analysis_data |>
  group_by(ticket_type) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  ggplot(aes(x = tickets, y = fct_reorder(ticket_type, tickets))) +
  geom_col(fill = "grey35") +
  scale_x_continuous(labels = comma) +
  labs(x = "Total tickets, 2014-2025", y = NULL)

ggsave(
  "outputs/figures/fig-tickettype-overview.png",
  fig_tickettype_overview,
  width = 7, height = 4, dpi = 300
)


#### Table: ticket type composition by division ####

table_tickettype_by_division <- analysis_data |>
  group_by(division) |>
  summarise(
    summons_share = sum(ticket_count[ticket_type == "Part III summons"]) / sum(ticket_count),
    .groups = "drop"
  ) |>
  arrange(desc(summons_share)) |>
  transmute(
    Division = division,
    `Share issued as Part III summons` = percent(summons_share, accuracy = 0.1)
  )

write_csv(table_tickettype_by_division, "outputs/tables/table-tickettype-by-division.csv")

tt(table_tickettype_by_division)


#### Figure: total tickets by year ####

yearly_totals <- analysis_data |>
  group_by(offence_year) |>
  summarise(total_tickets = sum(ticket_count), .groups = "drop")

fig_yearly_totals <- yearly_totals |>
  ggplot(aes(x = offence_year, y = total_tickets)) +
  geom_line(colour = "grey20", linewidth = 1) +
  geom_point(colour = "grey20", size = 2) +
  scale_x_continuous(breaks = 2014:2025) +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Total tickets issued", title = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "outputs/figures/fig-yearly-totals.png",
  fig_yearly_totals,
  width = 7, height = 4, dpi = 300
)


#### Figure: behavioural share of tickets by year ####
# Grayscale gradient fill (not arbitrary colors) distinguishes the two
# offence groups.

yearly_group_share <- analysis_data |>
  group_by(offence_year, offence_group) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  group_by(offence_year) |>
  mutate(share = tickets / sum(tickets)) |>
  ungroup()

fig_group_share <- yearly_group_share |>
  ggplot(aes(x = offence_year, y = share, fill = offence_group)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 2014:2025) +
  scale_y_continuous(labels = percent) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  labs(x = "Year", y = "Share of tickets", fill = "Offence group", title = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "outputs/figures/fig-group-share.png",
  fig_group_share,
  width = 7, height = 4, dpi = 300
)


#### Figure: category trends by year ####
# Shape and linetype distinguish the eight categories, rather than
# colour (single grayscale color throughout).

category_trends <- analysis_data |>
  group_by(offence_year, offence_category) |>
  summarise(tickets = sum(ticket_count), .groups = "drop")

fig_category_trends <- category_trends |>
  ggplot(aes(x = offence_year, y = tickets, linetype = offence_category, shape = offence_category)) +
  geom_line(colour = "grey20", linewidth = 0.6) +
  geom_point(colour = "grey20", size = 1.8) +
  scale_x_continuous(breaks = 2014:2025) +
  scale_y_continuous(labels = comma) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 8)) +
  scale_linetype_manual(values = c(
    "solid", "22", "42", "44", "13", "1343", "73", "2262"
  )) +
  labs(x = "Year", y = "Tickets issued", linetype = "Offence category", shape = "Offence category", title = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(
    linetype = guide_legend(nrow = 3),
    shape = guide_legend(nrow = 3)
  )

ggsave(
  "outputs/figures/fig-category-trends.png",
  fig_category_trends,
  width = 7, height = 5, dpi = 300
)


#### Figure: NSA (unresolved location) share by year ####

nsa_share <- analysis_data |>
  group_by(offence_year) |>
  summarise(
    nsa_share = sum(ticket_count[!has_neighbourhood]) / sum(ticket_count),
    .groups = "drop"
  )

fig_nsa_share <- nsa_share |>
  ggplot(aes(x = offence_year, y = nsa_share)) +
  geom_line(colour = "grey20", linewidth = 1) +
  geom_point(colour = "grey20", size = 2) +
  scale_x_continuous(breaks = 2014:2025) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  labs(x = "Year", y = "Share of tickets with no resolved neighbourhood", title = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "outputs/figures/fig-nsa-share.png",
  fig_nsa_share,
  width = 7, height = 4, dpi = 300
)


#### Table: division-level change, pre- vs. post-pandemic (behavioural only) ####

division_change <- analysis_data |>
  filter(offence_group == "Behavioural", period != "2020_2021") |>
  group_by(division, period, offence_year) |>
  summarise(yearly_tickets = sum(ticket_count), .groups = "drop") |>
  group_by(division, period) |>
  summarise(avg_annual_tickets = mean(yearly_tickets), .groups = "drop") |>
  pivot_wider(names_from = period, values_from = avg_annual_tickets) |>
  mutate(pct_change = (post_2021 / pre_2020 - 1) * 100) |>
  arrange(desc(pct_change))

table_division_change <- division_change |>
  transmute(
    Division = division,
    `Avg. annual tickets, 2014-19` = comma(pre_2020, accuracy = 1),
    `Avg. annual tickets, 2022-25` = comma(post_2021, accuracy = 1),
    `Change` = paste0(if_else(pct_change >= 0, "+", ""), number(pct_change, accuracy = 0.1), "%")
  )

write_csv(division_change, "outputs/tables/table-division-change.csv")

tt(table_division_change)


#### Figure: category mix in the top 10 neighbourhoods, 2022-25 ####
# Grayscale gradient fill (not arbitrary colors) distinguishes the
# four behavioural offence categories.

top10_neighbourhoods <- analysis_data |>
  filter(period == "post_2021", offence_group == "Behavioural", has_neighbourhood) |>
  group_by(neighbourhood_158) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  slice_max(tickets, n = 10) |>
  pull(neighbourhood_158)

fig_top10_mix <- analysis_data |>
  filter(
    period == "post_2021", offence_group == "Behavioural", has_neighbourhood,
    neighbourhood_158 %in% top10_neighbourhoods
  ) |>
  group_by(neighbourhood_158, offence_category) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  ggplot(aes(
    x = tickets,
    y = fct_reorder(neighbourhood_158, tickets, .fun = sum),
    fill = offence_category
  )) +
  geom_col() +
  scale_x_continuous(labels = comma) +
  scale_fill_grey(start = 0.15, end = 0.85) +
  labs(
    x = "Tickets issued, 2022-2025", y = NULL,
    fill = "Offence category", title = NULL
  ) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

ggsave(
  "outputs/figures/fig-top10-neighbourhood-mix.png",
  fig_top10_mix,
  width = 7, height = 5, dpi = 300
)


#### Figure: age group share by year (near-constant, included for completeness) ####

age_share <- analysis_data |>
  group_by(offence_year, age_group) |>
  summarise(tickets = sum(ticket_count), .groups = "drop") |>
  group_by(offence_year) |>
  mutate(share = tickets / sum(tickets)) |>
  ungroup()

fig_age_share <- age_share |>
  filter(age_group != "Adult") |>
  ggplot(aes(x = offence_year, y = share, linetype = age_group, shape = age_group)) +
  geom_line(colour = "grey20", linewidth = 0.8) +
  geom_point(colour = "grey20", size = 2) +
  scale_x_continuous(breaks = 2014:2025) +
  scale_y_continuous(labels = percent) +
  labs(x = "Year", y = "Share of tickets", linetype = "Age group", shape = "Age group", title = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "outputs/figures/fig-age-share.png",
  fig_age_share,
  width = 7, height = 4, dpi = 300
)

