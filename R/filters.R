library(shiny)

apply_filters <- function(df, countries, sectors, year_range) {
  mask <- rep(TRUE, nrow(df))
  if (length(countries) > 0) mask <- mask & df$Country %in% countries
  if (length(sectors) > 0) mask <- mask & df$Sector %in% sectors
  mask <- mask & df$Year >= year_range[1] & df$Year <= year_range[2]
  df[mask, , drop = FALSE]
}

currency_col <- function(currency) if (identical(currency, "usd")) "rev_usd" else "rev_local"
currency_label <- function(currency) if (identical(currency, "usd")) "Revenue (US$ millions)" else "Revenue (local currency, millions)"

# "Worldwide" is a synthetic choice in the Countries page's single-country picker
# (see mod_countries.R) meaning "no country filter" -- every country aggregated
# together. These two helpers translate that sentinel into what apply_filters()
# (an empty vector) and direct `df$Country == country` row-masking actually need.
is_worldwide <- function(country) identical(country, "Worldwide")
country_mask <- function(col, country) if (is_worldwide(country)) rep(TRUE, length(col)) else col == country
