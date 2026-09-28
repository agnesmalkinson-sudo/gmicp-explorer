library(dplyr)

# Provisional ceiling on how recent a year the app treats as "the latest year"
# for chart ranges, KPI defaults, and year-picker defaults -- most countries'
# 2024/2025 coverage is currently too sparse for those automatic choices to be
# representative (a handful of sectors/companies reporting looks like a
# collapse rather than incomplete data collection). Bump this forward as later
# years fill in; it doesn't restrict the Years filter slider itself, which
# still covers the dataset's full range for anyone who wants to look anyway.
DEFAULT_SNAPSHOT_YEAR <- 2023

# Year cadence for revenue-over-time charts: sparse pre-2000 coverage means fewer
# points are shown there, more as coverage gets denser toward the present. Capped
# at `cap` (DEFAULT_SNAPSHOT_YEAR unless overridden) so trailing, too-thin-to-be-
# filled years don't tail off the chart looking like a real collapse.
data_include_years <- function(max_year, cap = DEFAULT_SNAPSHOT_YEAR) {
  max_year <- min(max_year, cap)
  years <- c(seq(1984, 2000, by = 4), seq(2002, 2010, by = 2), seq(2011, max_year))
  sort(unique(years))
}

# Single-country override for the DEFAULT_SNAPSHOT_YEAR cap above: that global
# cap is a blunt, dataset-wide guard, but a specific country can have solid
# 2024/2025 coverage even while most others don't. Walks back from this
# country's latest raw data year to the most recent year whose sector count is
# still at least min_coverage_ratio of this country's own recent coverage, so a
# thin trailing year (a handful of sectors reporting so far) doesn't get shown
# as if revenue collapsed. Never returns less than the old global-cap result,
# so no country's chart regresses relative to today's behavior, and never more
# than the country's actual latest data year.
latest_country_chart_year <- function(df_raw, min_coverage_ratio = 0.7, lookback_years = 10) {
  max_yr <- max(df_raw$Year)
  floor_yr <- min(max_yr, DEFAULT_SNAPSHOT_YEAR)
  cov <- df_raw %>% group_by(Year) %>% summarise(n_sectors = n_distinct(Sector), .groups = "drop") %>% arrange(Year)
  recent <- cov[cov$Year > max_yr - lookback_years & cov$Year <= max_yr, , drop = FALSE]
  reference <- max(recent$n_sectors)
  threshold <- reference * min_coverage_ratio
  candidates <- cov$Year[cov$n_sectors >= threshold]
  best <- if (length(candidates) == 0) floor_yr else max(candidates)
  min(max(best, floor_yr), max_yr)
}

# Country-level revenue totals (summed across sectors), gap-filled at the
# (Country,Sector) level before totaling so a year with only partial sector coverage
# doesn't look like a real revenue collapse (see fill_pair_gaps()), then again at the
# Country level for any Year/Country combos still missing entirely.
build_revenue_totals <- function(df_revenue, countries, sectors, years, rc) {
  d <- apply_filters(df_revenue, countries, sectors, years)
  if (nrow(d) == 0) return(list(df = d, notes = list(), n_sector_level_fills = 0))

  sector_raw <- d %>% group_by(Year, Country, Sector) %>%
    summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop")
  names(sector_raw)[names(sector_raw) == "value"] <- rc
  sector_filled <- fill_pair_gaps(as.data.frame(sector_raw), "Country", "Sector", rc)

  country_totals <- sector_filled %>% group_by(Year, Country) %>%
    summarise(value = sum(.data[[rc]], na.rm = TRUE), sector_level_fill = any(filled), .groups = "drop")
  names(country_totals)[names(country_totals) == "value"] <- rc

  res <- fill_year_gaps(as.data.frame(country_totals[, c("Year", "Country", rc)]), "Country", rc)
  flags <- country_totals[country_totals$sector_level_fill, c("Year", "Country")]
  flags$sector_level_fill <- TRUE
  out <- merge(res$df, flags, by = c("Year", "Country"), all.x = TRUE)
  out$filled <- out$filled | !is.na(out$sector_level_fill)
  out$sector_level_fill <- NULL

  list(df = out, notes = res$notes, n_sector_level_fills = sum(sector_filled$filled))
}

revenue_fill_caption <- function(res) {
  parts <- c()
  if (res$n_sector_level_fills > 0) {
    parts <- c(parts, paste0(format(res$n_sector_level_fills, big.mark = ","),
                              " country-sector combinations estimated from adjacent years before totaling"))
  }
  if (length(res$notes) > 0) {
    parts <- c(parts, paste0(length(res$notes), " year", if (length(res$notes) != 1) "s" else "",
                              " with no sector data at all, filled from a nearby year's total"))
  }
  if (length(parts) == 0) return(NULL)
  paste0("Data note: ", paste(parts, collapse = "; "), ". Points marked with an asterisk (*) in the tooltip are estimated.")
}

# Per (Country,Sector) CR4/HHI series, gap-filled. `series` is labeled by whichever
# of Country/Sector actually varies in the current selection (just Sector for one
# country and several sectors, just Country for one sector and several countries,
# "Country — Sector" once both vary).
# `collapse_countries`: for the Countries page's "Worldwide" option -- averaging
# every country's CR4/HHI for a sector into one global line per sector, rather
# than a separate line per country (which, across dozens of countries, would be
# an unreadable legend). An unweighted mean, consistent with how this function
# already averages multiple rows within a single (Country,Sector,Year).
build_concentration_series <- function(df_conc, countries, sectors, years, mc, collapse_countries = FALSE) {
  d <- apply_filters(df_conc, countries, sectors, years)
  if (nrow(d) == 0) return(d)

  if (collapse_countries) {
    raw <- d %>% group_by(Year, Sector) %>% summarise(value = mean(.data[[mc]], na.rm = TRUE), .groups = "drop")
    names(raw)[names(raw) == "value"] <- mc
    filled <- fill_year_gaps(as.data.frame(raw), "Sector", mc)$df
    filled <- filled[!is.na(filled[[mc]]), , drop = FALSE]
    filled$series <- filled$Sector
    return(filled)
  }

  raw <- d %>% group_by(Year, Country, Sector) %>% summarise(value = mean(.data[[mc]], na.rm = TRUE), .groups = "drop")
  names(raw)[names(raw) == "value"] <- mc
  filled <- fill_pair_gaps(as.data.frame(raw), "Country", "Sector", mc)
  filled <- filled[!is.na(filled[[mc]]), , drop = FALSE]

  n_countries <- length(unique(filled$Country))
  n_sectors <- length(unique(filled$Sector))
  filled$series <- if (n_countries > 1 && n_sectors > 1) paste(filled$Country, filled$Sector, sep = " — ")
                    else if (n_countries > 1) filled$Country
                    else filled$Sector
  filled
}

conc_fill_caption <- function(d) {
  n <- sum(d$filled)
  if (n == 0) return(NULL)
  paste0("Data note: ", format(n, big.mark = ","), " point", if (n != 1) "s" else "",
         " on this chart ", if (n != 1) "were" else "was",
         " missing and filled from the nearest year within ±2 years. Points marked with an asterisk (*) in the tooltip are estimated.")
}

stat_card <- function(value, label, delta = NULL, detail = NULL, sparkline = NULL, icon = NULL, boxed = TRUE) {
  delta_tag <- NULL
  if (!is.null(delta) && !is.na(delta)) {
    cls <- if (delta >= 0) "positive" else "negative"
    delta_tag <- span(class = paste("stat-delta", cls), paste0(if (delta >= 0) "+" else "", round(delta, 1), "%"))
  }
  card_class <- if (boxed) "stat-card" else "stat-card stat-card-plain"
  div(class = card_class,
      div(class = "stat-card-header",
          if (!is.null(icon)) div(class = "stat-card-icon", bsicons::bs_icon(icon, size = "0.85rem")),
          div(class = "stat-card-text",
              div(class = "stat-label", label),
              div(class = "stat-value", value, delta_tag))),
      if (!is.null(detail)) div(class = "stat-card-detail", detail),
      if (!is.null(sparkline)) div(class = "stat-card-spark", sparkline))
}
