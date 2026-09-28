# GMICP Explorer

A public R/Shiny dashboard for exploring the Global Media and Internet Concentration Project
(GMICP) dataset: revenue trends, top companies, and market concentration (CR4/HHI)
across media and telecom sectors, in countries around the world.

## Project layout

```
app.R                 # entrypoint: header, filter bar, nav tabs; loads the workbook once at startup
R/data.R              # reads and cleans the GMICP Excel workbook
R/geo.R                # country name <-> ISO-3166-1 alpha-2 lookups, for joining data onto the map
R/gaps.R               # +/-2 year gap-filling primitives, with disclosure notes
R/agg_helpers.R         # shared aggregation logic built on R/gaps.R, used by both pages below
R/charts.R             # Highcharts (highcharter) theme + chart builders
R/filters.R             # apply_filters() + currency helpers
R/mod_countries.R       # Countries page: map/search, with a Single country / Compare countries mode toggle
R/mod_companies.R       # Companies page: worldwide leaderboard + single-company profile
R/mod_about.R           # Data & Methodology page
www/styles.css           # app styling
data/GMICP_unified_workbook.xlsx
install.R              # installs required R packages
deploy.R               # deploys to shinyapps.io
```

## Run locally

1. Install [R](https://cran.r-project.org/) (4.2+) and, optionally, [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install dependencies:
   ```r
   source("install.R")
   ```
3. Run the app:
   ```r
   shiny::runApp()
   ```
   Or open `app.R` in RStudio and click **Run App**.

## Deploy online (shinyapps.io)

GitHub itself only serves static files — it can't run a live R/Shiny process. [shinyapps.io](https://www.shinyapps.io)
is Posit's free-tier Shiny hosting: push and get a public URL, no server management required.

1. Create a free account at shinyapps.io.
2. In the shinyapps.io dashboard, go to **Account > Tokens**, and copy the `rsconnect::setAccountInfo(...)`
   snippet it gives you. Run that once in your local R console (only needed the first time, per machine).
3. Deploy:
   ```r
   source("deploy.R")
   ```
4. Re-run `source("deploy.R")` any time you want to push updates.

## Design

- **Countries page, one map, two modes.** A "Single country / Compare countries" toggle switches what
  clicking the map (or the search box) does — replace the current selection (single country: revenue by
  sector, top companies, concentration) vs. add/remove a country from a comparison set (multiple countries:
  revenue over time, a snapshot-year breakdown, and concentration, all side by side). Switching modes clears
  the current selection. Concentration is shared code either way (`build_concentration_series()` in
  `R/agg_helpers.R` already handles one country or several).
- **Companies page** answers "who's biggest, period" rather than "who's biggest in this one country" —
  a worldwide top-N leaderboard (stacked by country) plus a searchable single-company profile (revenue by
  country over time, and a footprint chart of which countries/sectors it operates in).
- **Sector, Year range, and Currency are the only global filters** — Country selection happens on the
  Countries page's own map/search, not as a filter dropdown, since it's the primary interaction there,
  not a filter on top of it.
- **Missing data is filled from nearby years, and it says so.** Any (country/sector, or company) × year gap is
  filled from the nearest available year within ±2 years — see `R/gaps.R` and `R/agg_helpers.R`. Every
  affected chart shows a caption disclosing how many points were filled and where from, and chart tooltips
  mark individual filled points. Downloaded CSVs keep `filled`/`source_year` columns so a reader can
  identify or exclude estimated data. Full policy on the **Data & Methods** page.
- **Direct data labels, not legends,** on multi-line charts (`gmicp_line_chart()` in `R/charts.R`) — each line is
  labeled at its last point, in that line's color, so there's nothing to cross-reference against a legend.
- Charts are built with [`highcharter`](https://jkunst.com/highcharter/) (an R wrapper around Highcharts,
  including its `hcmap()` choropleth map), themed in `R/charts.R` (`hc_gmicp_theme()`) with muted gridlines,
  a single axis line, and a vivid indigo/pastel palette that matches the rest of the dashboard.
  Typefaces are **Inter** and **Manrope**, both Google Fonts, loaded in `app.R`.
  **Highcharts licensing:** free for personal/non-commercial and nonprofit use (CC BY-NC) and for non-funded
  academic research at an educational institution — a paid license is required for commercial/internally-funded
  use. See [highcharts.com/faq](https://shop.highcharts.com/faq) to confirm which tier applies to your use case.
- Every chart has a **Download chart data (CSV)** button next to it.
