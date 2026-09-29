# Speeding Down, Aggression Up: Toronto Traffic Tickets, 2014-2025

This repo contains the data, code, and paper for an analysis of twelve years
of traffic tickets issued by the Toronto Police Service, examining whether
driving behaviour in Toronto has become more or less risky over time, and
where in the city specific behaviours are concentrated.

The paper is at `paper/paper.qmd` (renders to PDF).

## File structure

```
.
├── data
│   ├── 00-simulated_data/     Simulated dataset (00-simulate_data.R output)
│   ├── 02-raw_data/           Unedited download from Open Data Toronto
│   └── 03-analysis_data/      Cleaned, analysis-ready dataset
├── outputs
│   ├── figures/                PNG figures from 05-exploratory_data_analysis.R
│   └── tables/                 CSV tables from 05-exploratory_data_analysis.R
├── paper
│   ├── paper.qmd                The paper itself
│   └── references.bib           BibTeX references
├── scripts
│   ├── 00-simulate_data.R              Simulates the dataset structure, with tests
│   ├── 01-test_simulated_data.R        Tests for the simulated dataset (testthat)
│   ├── 02-download_data.R              Downloads the real dataset via opendatatoronto
│   ├── 03-clean_data.R                 Cleans the raw data into analysis_data.csv
│   ├── 04-test_analysis_data.R         Tests for the cleaned real dataset (testthat)
│   └── 05-exploratory_data_analysis.R  Builds the figures/tables used in the paper
├── sketches/                    Pre-analysis sketches of the planned figures
└── other/
    └── llm/
        └── usage.txt             Statement on LLM usage (see below)
```

## Reproducing this analysis

1. Open this folder as an RStudio project (`.Rproj`).
2. Install dependencies (see "Dependencies" below).
3. Run the scripts in numeric order from the project root:
   - `scripts/00-simulate_data.R`
   - `scripts/01-test_simulated_data.R`
   - `scripts/02-download_data.R`
   - `scripts/03-clean_data.R`
   - `scripts/04-test_analysis_data.R`
   - `scripts/05-exploratory_data_analysis.R`
4. Render `paper/paper.qmd` to PDF (e.g. the "Render" button in RStudio, or
   `quarto::quarto_render("paper/paper.qmd")`).

Each script reads only locally saved files (never re-downloading or
re-simulating data it doesn't need), so steps 3-4 can be re-run
independently once the earlier steps have produced their output files.

## Dependencies

```r
install.packages(c(
  "tidyverse", "opendatatoronto", "tinytable", "scales", "testthat"
))
```

A working Quarto installation (bundled with recent RStudio) is required to
render `paper.qmd` to PDF.

## Data source

Toronto Police Service, *Police Annual Statistical Report -- Tickets
Issued*, City of Toronto Open Data Portal, accessed via the
`opendatatoronto` package.

## Statement on LLM usage

**TODO -- fill in before submitting.** The rubric requires this be filled in
truthfully and specifically: whether autocomplete-style tools (e.g.
Copilot) were used in the code, and whether a chat-based LLM (e.g. this
conversation) was used -- and if so, the full chat history must be included
in `other/llm/usage.txt`. A starting point, to edit so it accurately
reflects what was and wasn't used:

> Aspects of the code and this paper were developed with the help of
> Claude (Anthropic). The full chat history is available in
> `other/llm/usage.txt`.

To produce `other/llm/usage.txt`, export this conversation (check Claude's
export/download options for the chat) and save the full text there. I
can't generate that export file myself from within this session -- it
needs to come from your own copy of the conversation.
