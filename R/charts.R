library(highcharter)

# Inter is loaded via a <link> tag in the document head (see app.R) and applied to
# charts through plain CSS on .highcharts-container (www/styles.css), NOT via a
# chart$style$fontFamily entry in the theme below: highcharter's JS binding scans
# every "fontFamily" value in the chart options and tries to build a Google Fonts
# request URL from it, and a multi-fallback CSS stack like "'Inter', Arial, ..."
# breaks that URL and throws a jQuery selector syntax error at render time.
options(highcharter.google_fonts = FALSE)

CHART_TEXT <- "#101014"
CHART_MUTED <- "#9a9aa2"
CHART_GRID <- "#f0f0f3"
CHART_BG <- "#ffffff"

# Vivid-pastel qualitative palette matching the app shell's product-dashboard look
# (indigo accent, soft pink/amber/teal), extended with a larger rotation so
# high-cardinality charts (dozens of companies) still get a distinct color each.
CHART_PALETTE <- c(
  "#6C5CE7", "#F2618C", "#F5A623", "#17B8A6", "#3E9EEA", "#7CC24B",
  "#D264C2", "#FB923C", "#22D3EE", "#EAB308", "#FB7185", "#6366F1"
)
.legacy_palette <- c(
  "#A78BFA", "#F472B6", "#FBBF24", "#2DD4BF", "#60A5FA", "#4ADE80",
  "#E879F9", "#FDBA74", "#67E8F9", "#FACC15", "#FCA5A5", "#818CF8",
  "#C4B5FD", "#F9A8D4", "#FDE68A", "#99F6E4", "#93C5FD", "#BEF264",
  "#F0ABFC", "#FED7AA", "#A5F3FC", "#FEF08A", "#FECACA", "#C7D2FE"
)
BIG_PALETTE <- unique(c(CHART_PALETTE, .legacy_palette))

# Minimal trend sparkline for embedding inside a stat card: no axes, no legend, no
# tooltip chrome -- just a thin colored line over a soft fill, sized by its container.
gmicp_sparkline <- function(values, color = "#6C5CE7") {
  values <- values[!is.na(values)]
  hc <- highchart() %>%
    hc_chart(type = "area", backgroundColor = "transparent",
              margin = c(2, 0, 2, 0), spacing = c(0, 0, 0, 0)) %>%
    hc_xAxis(visible = FALSE) %>%
    hc_yAxis(visible = FALSE, startOnTick = FALSE, endOnTick = FALSE) %>%
    hc_legend(enabled = FALSE) %>%
    hc_tooltip(enabled = FALSE) %>%
    hc_credits(enabled = FALSE) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_plotOptions(area = list(
      color = color, fillOpacity = 0.16, lineWidth = 2,
      marker = list(enabled = FALSE), enableMouseTracking = FALSE
    ))
  if (length(values) >= 2) hc <- hc %>% hc_add_series(data = values)
  hc
}

fmt_money <- function(v, currency_symbol = "$") {
  if (is.na(v)) return("N/A")
  if (abs(v) >= 1e6) return(paste0(currency_symbol, formatC(v / 1e6, digits = 1, format = "f"), "T"))
  if (abs(v) >= 1e3) return(paste0(currency_symbol, formatC(v / 1e3, digits = 1, format = "f"), "B"))
  paste0(currency_symbol, round(v), "M")
}

build_color_map <- function(categories) {
  categories <- unique(categories)
  n <- length(BIG_PALETTE)
  setNames(BIG_PALETTE[(seq_along(categories) - 1) %% n + 1], categories)
}

# Evenly-spaced hues (not a fixed hand-picked palette) so any two categories
# stay visually distinct regardless of how many there are or which positions
# they land on. BIG_PALETTE works fine for the handful of series a typical
# chart shows at once, but it has more than one color from the same "green"
# family scattered at different positions -- fine normally, but the Companies
# page's country map can have 20-30+ countries on screen together, where two
# unrelated countries (e.g. Canada, Russia) landing on different-but-similar
# greens reads as a mistake.
distinct_hues <- function(n) {
  if (n <= 0) return(character(0))
  grDevices::hcl(h = seq(15, 375, length.out = n + 1)[seq_len(n)], c = 80, l = 60)
}

hc_gmicp_theme <- function() {
  hc_theme(
    colors = BIG_PALETTE,
    chart = list(backgroundColor = CHART_BG),
    title = list(style = list(color = CHART_TEXT, fontWeight = "800")),
    subtitle = list(style = list(color = CHART_MUTED)),
    xAxis = list(
      gridLineWidth = 0, lineWidth = 0, tickLength = 0,
      labels = list(style = list(color = CHART_MUTED, fontSize = "13px")),
      title = list(style = list(color = CHART_MUTED, fontSize = "13px"))
    ),
    yAxis = list(
      gridLineWidth = 1, gridLineColor = CHART_GRID, gridLineDashStyle = "Solid",
      lineWidth = 0, tickLength = 0, minorGridLineWidth = 0,
      labels = list(style = list(color = CHART_MUTED, fontSize = "13px")),
      title = list(style = list(color = CHART_MUTED, fontSize = "13px"))
    ),
    legend = list(
      itemStyle = list(color = CHART_TEXT, fontWeight = "normal", fontSize = "13px"),
      itemHoverStyle = list(color = "#6C5CE7"),
      align = "left", verticalAlign = "top", layout = "horizontal"
    ),
    plotOptions = list(
      column = list(borderWidth = 0),
      bar = list(borderWidth = 0)
    ),
    # Highcharts' own tooltip default (~12px) doesn't scale with the page's
    # base font-size the way regular HTML text does (SVG/HTML tooltip text is
    # sized independently) -- set explicitly, and a notch larger than body
    # text since a tooltip is usually read at a glance, not settled into.
    tooltip = list(backgroundColor = "#ffffff", borderColor = "#ececee", borderRadius = 12,
                   style = list(color = CHART_TEXT, fontSize = "14px"))
  )
}

apply_gmicp_theme <- function(hc) {
  hc %>% hc_add_theme(hc_gmicp_theme()) %>% hc_credits(enabled = FALSE) %>% hc_exporting(enabled = FALSE)
}

# Shared-tooltip formatter for both gmicp_line_chart() and gmicp_stacked_chart():
# one row per series at the hovered x, with a trailing asterisk on the value for
# points flagged via a "note" field (see fill_tooltip_note() in gaps.R) -- the
# caption below each chart explains what the asterisk means. show_total appends
# a summed-across-series line (opt-in -- e.g. the sector-time stacked chart,
# where "total revenue that year" is meaningful; not turned on for charts like
# compare-countries or compare-sectors, where summing series together isn't).
gmicp_shared_tooltip_js <- function(sort_desc = FALSE, show_total = FALSE) {
  sort_snippet <- if (sort_desc) "pts.sort(function(a, b) { return b.y - a.y; });" else ""
  total_snippet <- if (show_total) r"(
    var total = pts.reduce(function(sum, p) { return sum + p.y; }, 0);
    s += '<br/><b>Total: ' + Highcharts.numberFormat(total, 1) + '</b>';
  )" else ""
  JS(paste0(r"(function() {
    var pts = this.points.slice();
    )", sort_snippet, r"(
    var s = '<b>' + this.x + '</b>';
    pts.forEach(function(p) {
      s += '<br/><span style="color:' + p.color + '">●</span> ' + p.series.name +
        ': <b>' + Highcharts.numberFormat(p.y, 1) + (p.point.note || '') + '</b>';
    });
    )", total_snippet, r"(
    return s;
  })"))
}

# Stacked (or, with stacked = FALSE, grouped/clustered) bar/column, built
# series-by-series so axis-category order and stacking order can be controlled
# independently of each other.
gmicp_stacked_chart <- function(df, category_col, value_col, series_col, category_order, series_order,
                               orientation = "v", extra_col = NULL, color_map = NULL, stacked = TRUE) {
  type <- if (orientation == "h") "bar" else "column"
  if (is.null(color_map)) color_map <- build_color_map(series_order)
  category_order_chr <- as.character(category_order)

  # as.list(), not the bare character vector: jsonlite auto-unboxes a length-1
  # character vector into a plain JSON string ("China") rather than a one-element
  # array (["China"]) when serializing the widget. Highcharts then reads that
  # string as an array of characters and shows just categories[0] -- a single-
  # country chart's x-axis label rendered as the single letter "C". Wrapping in
  # as.list() forces array serialization regardless of length.
  hc <- highchart() %>% hc_chart(type = type) %>% hc_xAxis(categories = as.list(category_order_chr))
  for (s in series_order) {
    sub <- df[df[[series_col]] == s, , drop = FALSE]
    idx <- match(as.character(sub[[category_col]]), category_order_chr)
    pts <- lapply(seq_along(category_order_chr), function(i) list(y = NA))
    for (j in seq_along(idx)) {
      if (is.na(idx[j])) next
      pt <- list(y = sub[[value_col]][j])
      if (!is.null(extra_col)) pt[[extra_col]] <- sub[[extra_col]][j]
      pts[[idx[j]]] <- pt
    }
    hc <- hc %>% hc_add_series(name = s, data = pts, color = unname(color_map[[s]]))
  }
  hc %>% hc_plotOptions(series = list(stacking = if (stacked) "normal" else NULL))
}

# Regulatory concentration bands, in the app's traffic-light order (low -> high).
# HHI thresholds are the DOJ/FTC 2010 Horizontal Merger Guidelines (<1,500
# unconcentrated, 1,500-2,500 moderately concentrated, >2,500 highly
# concentrated); CR4 reuses the two figures the app's own "Concentration Ratio"
# glossary entry cites (CR4 >50%, CR8 >75%) as the closest sourced equivalent,
# since there's no separate three-band CR4 standard as widely cited as the HHI one.
CONCENTRATION_BAND_COLORS <- c("#17B8A6", "#F5A623", "#F2618C")
CONCENTRATION_BAND_LABELS <- c("Unconcentrated", "Moderately concentrated", "Highly concentrated")
concentration_band_breaks <- function(mc) if (mc == "hhi") c(1500, 2500) else c(50, 75)

# Discrete (not gradient) band color for a single value -- values below the
# first break get the first color, at/above the last break get the last color,
# same cutoffs concentration_band_breaks() uses everywhere else (heatmap cells,
# the legend key, and the CR4/HHI glossary entries above).
concentration_band_color <- function(value, mc) {
  CONCENTRATION_BAND_COLORS[findInterval(value, concentration_band_breaks(mc)) + 1]
}

# Human-readable "<1,500 (Unconcentrated)" / "50-75% (Moderately concentrated)"
# style labels for each band, for the heatmap's legend key (see
# conc_heatmap_legend_ui in mod_countries.R).
concentration_band_ranges <- function(mc) {
  b <- concentration_band_breaks(mc)
  fmt <- function(x) if (mc == "hhi") format(x, big.mark = ",") else paste0(x, "%")
  c(paste0("< ", fmt(b[1])), paste0(fmt(b[1]), " – ", fmt(b[2])), paste0("> ", fmt(b[2])))
}

# Series x Year grid, colored by concentration level -- a heat-mapped alternative
# to the Market concentration line chart, for scanning many sectors/years at once
# instead of following individual lines. Each cell is colored by which of the
# three regulatory concentration bands it falls into (see concentration_band_color()),
# not a continuous gradient across the metric's full range -- a value just over
# the "highly concentrated" cutoff reads as red immediately, not as a barely-
# tinted mid-gradient color. `df` is conc_agg()'s output: one row per (series,
# Year), with `filled` flagging estimated points (see
# build_concentration_series()/fill_pair_gaps()).
gmicp_concentration_heatmap <- function(df, mc) {
  df <- df[!is.na(df[[mc]]), , drop = FALSE]
  y_cats <- sort(unique(df$series))
  x_cats <- sort(unique(df$Year))
  yi <- match(df$series, y_cats) - 1
  xi <- match(df$Year, x_cats) - 1
  metric_label <- if (mc == "hhi") "HHI" else "CR4 (%)"
  pts <- lapply(seq_len(nrow(df)), function(i) {
    list(x = xi[i], y = yi[i], value = df[[mc]][i], series_name = df$series[i],
         color = concentration_band_color(df[[mc]][i], mc),
         note = if (isTRUE(df$filled[i])) " (estimated)" else "")
  })

  highchart() %>%
    hc_chart(type = "heatmap") %>%
    hc_xAxis(categories = as.list(as.character(x_cats)), title = list(text = ""),
             labels = list(rotation = -60, style = list(fontSize = "11px"))) %>%
    hc_yAxis(categories = as.list(y_cats), title = list(text = ""), reversed = TRUE,
             labels = list(style = list(fontSize = "11px"))) %>%
    hc_colorAxis(min = 0, max = if (mc == "hhi") 10000 else 100) %>%
    hc_add_series(name = metric_label, data = pts, borderWidth = 0.5, borderColor = "#ffffff") %>%
    hc_tooltip(useHTML = TRUE, formatter = JS(r"(function() {
      return '<b>' + this.point.series_name + '</b><br/>' + this.series.xAxis.categories[this.point.x] +
        ': <b>' + Highcharts.numberFormat(this.point.value, 1) + '</b>' + (this.point.note || '');
    })")) %>%
    hc_legend(enabled = FALSE) %>%
    apply_gmicp_theme()
}

# Plain multi-series line chart -- smooth (spline) curve, but otherwise default
# Highcharts markers/legend rendering (no area fill, no custom hover/label
# plugins), just the app's color palette, axis/tooltip formatting, and shared
# theme on top.
# `sort_tooltip_desc`: when TRUE, the shared tooltip lists series largest-value-first
# instead of series/insertion order.
# `show_markers`: FALSE hides the point markers at rest (still shown on hover),
# for a cleaner line-only look where individual points aren't the focus. Applies
# per-series, though: a series with exactly one data point has no line to draw,
# so with markers off it would be entirely invisible at rest -- those series
# keep a marker regardless.
# `note_col`: optional column of pre-built per-point tooltip HTML (see
# fill_tooltip_note() in gaps.R) -- e.g. flagging a filled/estimated point.
gmicp_line_chart <- function(df, x, y, color, y_label = y, sort_tooltip_desc = FALSE, show_markers = TRUE,
                              note_col = NULL) {
  cats <- sort(unique(df[[color]]))
  cmap <- build_color_map(cats)
  single_point_cats <- names(which(table(df[[color]]) == 1))

  mapping <- if (!is.null(note_col)) {
    hcaes_string(x = x, y = y, group = color, note = note_col)
  } else {
    hcaes_string(x = x, y = y, group = color)
  }

  hc <- hchart(df, "spline", mapping) %>%
    hc_colors(unname(cmap[levels(factor(df[[color]]))])) %>%
    hc_yAxis(title = list(text = y_label), labels = list(format = "{value:,.0f}"), min = 0) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE, useHTML = TRUE,
               formatter = gmicp_shared_tooltip_js(sort_tooltip_desc)) %>%
    hc_legend(enabled = length(cats) > 1) %>%
    hc_plotOptions(spline = list(
      marker = list(enabled = show_markers, states = list(hover = list(enabled = TRUE)))
    ))

  hc <- hc %>% apply_gmicp_theme()

  if (!show_markers && length(single_point_cats) > 0) {
    for (i in seq_along(hc$x$hc_opts$series)) {
      if (hc$x$hc_opts$series[[i]]$name %in% single_point_cats) {
        hc$x$hc_opts$series[[i]]$marker <- list(enabled = TRUE, radius = 4)
      }
    }
  }
  hc
}
