library(shiny)
library(dplyr)
library(highcharter)

# Maps GMICP country names to the lowercase ISO-3166-1 alpha-2 codes used as
# `hc-key` in Highcharts' "custom/world" map, so revenue/concentration data can be
# joined onto map shapes. Countries not in this table simply render with no data
# on the map (grey) -- every other part of the app still works for them via search.
COUNTRY_ISO2 <- c(
  "Argentina" = "ar", "Australia" = "au", "Austria" = "at", "Belgium" = "be",
  "Brazil" = "br", "Canada" = "ca", "Chile" = "cl", "China" = "cn",
  "Czech Republic" = "cz", "Denmark" = "dk", "Finland" = "fi", "France" = "fr",
  "Germany" = "de", "India" = "in", "Israel" = "il", "Italy" = "it", "Japan" = "jp",
  "Kenya" = "ke", "Mexico" = "mx", "Netherlands" = "nl", "New Zealand" = "nz",
  "Nigeria" = "ng", "Norway" = "no", "Poland" = "pl", "Portugal" = "pt",
  "Russia" = "ru", "Slovakia" = "sk", "South Africa" = "za", "South Korea" = "kr",
  "Spain" = "es", "Sweden" = "se", "Switzerland" = "ch", "Turkey" = "tr",
  "United Kingdom" = "gb", "United States" = "us"
)

ISO2_TO_COUNTRY <- setNames(names(COUNTRY_ISO2), unname(COUNTRY_ISO2))

MAP_METRICS <- c(
  "Total revenue (US$)" = "revenue",
  "Market concentration — HHI" = "hhi",
  "Market concentration — CR4 (%)" = "cr4"
)

# Builds the world choropleth used on both the Countries and Companies pages.
# `click_input_id` must be the fully-namespaced Shiny input id (i.e. already run
# through that module's ns()) that the map's click handler will set; the caller
# wires up its own observeEvent(input$map_click, ...) since single-select
# (replace) vs multi-select (toggle) behavior differs by page.
# `categorical`: the Countries page map is a data graphic (color encodes revenue/
# concentration magnitude, hence the gradient + legend scale), but the Companies
# page map is just a country filter picker -- a magnitude gradient there implies
# a comparison that page isn't making. When TRUE, each present country gets its
# own flat qualitative color instead, and the color-axis/legend scale is dropped.
build_country_map <- function(data, metric, sector_sel, year, click_input_id, categorical = FALSE) {
  ss <- sector_sel

  if (metric == "revenue") {
    d <- data$revenue[data$revenue$Year == year, , drop = FALSE]
    if (length(ss) > 0) d <- d[d$Sector %in% ss, , drop = FALSE]
    agg <- d %>% group_by(Country) %>% summarise(value = sum(rev_usd, na.rm = TRUE), .groups = "drop")
    legend_name <- "Revenue (US$ millions)"
    is_revenue <- TRUE
  } else {
    mc <- metric
    d <- data$concentration[data$concentration$Year == year & !is.na(data$concentration[[mc]]), , drop = FALSE]
    if (length(ss) > 0) {
      d <- d[d$Sector %in% ss, , drop = FALSE]
    } else if (nrow(d) > 0) {
      best_sector <- d %>% count(Sector, sort = TRUE) %>% slice_head(n = 1) %>% pull(Sector)
      d <- d[d$Sector == best_sector, , drop = FALSE]
    }
    agg <- d %>% group_by(Country) %>% summarise(value = mean(.data[[mc]], na.rm = TRUE), .groups = "drop")
    legend_name <- if (mc == "hhi") "HHI" else "CR4 (%)"
    is_revenue <- FALSE
  }

  agg$iso2 <- unname(COUNTRY_ISO2[agg$Country])
  agg <- agg[!is.na(agg$iso2) & !is.na(agg$value) & agg$value > 0, , drop = FALSE]
  validate(need(nrow(agg) > 0, "No data available for this metric/year/sector combination."))

  if (categorical) agg$color <- distinct_hues(nrow(agg))

  click_js <- JS(sprintf(
    "function() { Shiny.setInputValue('%s', {key: this['hc-key'] || this.properties['hc-key'], t: Date.now()}, {priority: 'event'}); }",
    click_input_id
  ))

  hc <- hcmap(map = "custom/world", data = as.data.frame(agg), value = "value", joinBy = c("hc-key", "iso2"),
              name = legend_name, nullColor = "#eceade", borderColor = "#ffffff", borderWidth = 0.4) %>%
    hc_mapNavigation(enabled = TRUE, buttonOptions = list(verticalAlign = "bottom")) %>%
    hc_tooltip(pointFormat = "<b>{point.name}</b><br/>{point.value:,.1f}") %>%
    hc_plotOptions(series = list(cursor = "pointer", point = list(events = list(click = click_js)))) %>%
    apply_gmicp_theme() %>%
    hc_chart(height = 500)

  if (categorical) {
    hc %>% hc_colorAxis(enabled = FALSE) %>% hc_legend(enabled = FALSE)
  } else {
    # Revenue is heavily right-skewed (a handful of huge economies, many small
    # ones), so a linear color scale leaves most countries in the palest,
    # hardest-to-see sliver of the gradient -- a log scale spreads them out instead.
    hc %>%
      hc_colorAxis(type = if (is_revenue) "logarithmic" else "linear",
                   stops = color_stops(colors = c("#ddd8fb", "#b3a9f4", "#8b7fe8", "#6c5ce7", "#4f3fc9"))) %>%
      hc_legend(enabled = TRUE, layout = "horizontal", align = "left")
  }
}
