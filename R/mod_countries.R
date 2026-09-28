library(shiny)
library(highcharter)
library(dplyr)
library(bsicons)

mod_countries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("compare_banner")),
    div(class = "panel-card panel-card-wide map-card",
      h3(class = "panel-title", "Select countries"),
      div(class = "mode-toggle",
        radioButtons(ns("mode"), NULL, c("Single country" = "single", "Compare countries" = "compare"),
                     selected = "single", inline = TRUE)
      ),
      p(class = "panel-note", uiOutput(ns("mode_hint"))),
      div(class = "panel-toolbar",
        selectInput(ns("map_metric"), "Colour map by", choices = MAP_METRICS, width = "260px"),
        uiOutput(ns("map_year_ui"))
      ),
      uiOutput(ns("map_sector_note")),
      highchartOutput(ns("map"), height = "500px"),
      div(class = "focus-row",
        div(class = "focus-search",
          div(class = "focus-search-label-row",
            tags$label(class = "filter-label", "Search for a country"),
            uiOutput(ns("select_all_ui"))
          ),
          uiOutput(ns("country_pick_ui"))
        ),
        uiOutput(ns("focus_hint"))
      )
    ),
    uiOutput(ns("detail_sections"))
  )
}

mod_countries_server <- function(id, data, all_countries, all_sectors, all_years, gf, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rev_col <- reactive(currency_col(gf$currency()))
    rev_label <- reactive(currency_label(gf$currency()))
    sector_sel <- reactive({
      s <- gf$sectors()
      if (is.null(s) || identical(s, "")) character(0) else s
    })
    is_compare <- reactive(identical(input$mode, "compare"))

    # The KPI stat row mirrors whichever year is selected in the map's own
    # "Snapshot year" picker (falling back to DEFAULT_SNAPSHOT_YEAR before that
    # control has rendered) -- so choosing e.g. 2024 there also updates Total
    # Revenue/Sectors Reported/Leading Company below, instead of those staying
    # stuck on the default regardless of what the visitor picks.
    target_yr <- reactive({
      y <- suppressWarnings(as.numeric(input$map_year))
      if (length(y) == 0 || is.na(y)) DEFAULT_SNAPSHOT_YEAR else y
    })

    # Comparing revenue/concentration across countries in each one's own local
    # currency mixes units on the same axis -- switch to USD the moment Compare
    # mode is turned on so the chart is meaningful by default. A one-time nudge,
    # not a lock: the user can still switch back to local currency afterward.
    observeEvent(input$mode, {
      if (identical(input$mode, "compare")) updateRadioButtons(parent_session, "currency", selected = "usd")
    })

    output$compare_banner <- renderUI({
      req(is_compare())
      n <- length(selection())
      status <- if (n == 0) "Search or click the map to add countries."
                else if (n == 1) paste0("1 selected (", selection(), ") — add at least one more to compare.")
                else paste0("Comparing ", n, " countries: ", paste(selection(), collapse = ", "))
      tagList(
        div(class = "compare-banner-spacer"),
        div(class = "compare-banner",
          bsicons::bs_icon("bar-chart-steps", size = "0.95rem"),
          span(class = "compare-banner-text", tags$strong("Compare mode"), " — ", status),
          actionLink(ns("exit_compare"), "Exit", class = "compare-banner-exit")
        )
      )
    })

    observeEvent(input$exit_compare, updateRadioButtons(session, "mode", selected = "single"))

    output$mode_hint <- renderUI({
      if (is_compare()) {
        paste0("Pick two or more countries to compare their revenue and market concentration side by side. ",
               "Click a country on the map to add or remove it. Switching modes clears the current selection.")
      } else {
        paste0("Click a country on the map, or search below, to see its revenue, top companies, and market ",
               "concentration. Picking a new country replaces the current one.")
      }
    })

    # ---------------- Map ----------------

    output$map_year_ui <- renderUI({
      selectInput(ns("map_year"), "Snapshot year", choices = rev(seq(all_years[1], all_years[2])),
                  selected = min(DEFAULT_SNAPSHOT_YEAR, all_years[2]), width = "160px")
    })

    output$map_sector_note <- renderUI({
      if (length(sector_sel()) == 0) {
        div(class = "panel-scope-note", "Using the Sectors filter above. Currently: all sectors (revenue is summed; concentration metrics show the sector with the most coverage for the selected year).")
      } else {
        div(class = "panel-scope-note", paste0("Using the Sectors filter above. Currently: ", paste(sector_sel(), collapse = ", "), "."))
      }
    })

    output$map <- renderHighchart({
      req(input$map_metric, input$map_year)
      build_country_map(data, input$map_metric, sector_sel(), as.numeric(input$map_year), ns("map_click"))
    })

    observeEvent(input$map_click, {
      key <- tolower(input$map_click$key %||% "")
      matched <- ISO2_TO_COUNTRY[[key]]
      if (is.null(matched)) return(invisible(NULL))
      if (is_compare()) {
        current <- input$country_pick %||% character(0)
        new_sel <- if (matched %in% current) setdiff(current, matched) else c(current, matched)
        updateSelectizeInput(session, "country_pick", selected = new_sel)
      } else {
        updateSelectizeInput(session, "country_pick", selected = matched)
      }
    })

    # ---------------- Country picker (rebuilt on mode change: single vs multi select) ----------------

    output$country_pick_ui <- renderUI({
      if (is_compare()) {
        selectizeInput(ns("country_pick"), NULL, choices = all_countries, selected = character(0), multiple = TRUE,
                        options = list(plugins = list("remove_button"), placeholder = "Type a country name..."))
      } else {
        # selected = character(0) alone isn't enough to keep a single (non-multiple)
        # selectize empty -- Shiny falls back to selecting the first choice ("Worldwide")
        # whenever `selected` has length 0, same as if it were never passed. onInitialize
        # force-clears it after render so the field genuinely starts empty.
        selectizeInput(ns("country_pick"), NULL, choices = c("Worldwide", all_countries), selected = character(0),
                        multiple = FALSE, options = list(placeholder = "Type a country name...",
                                                          onInitialize = I('function() { this.setValue(""); }')))
      }
    })

    output$select_all_ui <- renderUI({
      req(is_compare())
      tagList(actionLink(ns("select_all_countries"), "Select all"), actionLink(ns("clear_focus"), "Clear all"))
    })
    observeEvent(input$clear_focus, updateSelectizeInput(session, "country_pick", selected = character(0)))
    observeEvent(input$select_all_countries, updateSelectizeInput(session, "country_pick", selected = all_countries))

    selection <- reactive({
      v <- input$country_pick
      if (is.null(v) || (length(v) == 1 && !nzchar(v))) character(0) else v
    })

    # What to actually pass as the country filter: "Worldwide" (single mode only)
    # means no country restriction at all, not a literal country named "Worldwide".
    selection_countries <- reactive({
      if (!is_compare() && is_worldwide(selection())) character(0) else selection()
    })

    output$focus_hint <- renderUI({
      n <- length(selection())
      if (is_compare()) {
        if (n == 0) div(class = "focus-hint", "Nothing selected yet — click the map or search above to get started.")
        else if (n == 1) div(class = "focus-hint", paste0("Selected: ", selection(), ". Add at least one more to compare."))
        else div(class = "focus-hint", paste0("Comparing ", n, " countries: ", paste(selection(), collapse = ", ")))
      } else {
        if (n == 0) div(class = "focus-hint", "Nothing selected yet — click the map or search above to get started.")
        else div(class = "focus-hint", paste0("Showing: ", selection()))
      }
    })

    # ---------------- Detail sections ----------------

    output$detail_sections <- renderUI({
      if (is_compare()) {
        n <- length(selection())
        if (n == 0) return(NULL)
        if (n == 1) {
          return(div(class = "panel-card panel-card-wide",
            p(class = "panel-note", "Add at least one more country above to compare.")))
        }
        tagList(
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Revenue over time"),
            p(class = "panel-note",
              "Total revenue for the countries you've selected — see the legend for which line is which. Data points ",
              "are shown every 4 years through 2000, every 2 years from 2002–2010, and every year from 2011 on. ",
              "Where a shown year is missing, the nearest year within ±2 years is used and marked with an asterisk (*) in the tooltip."),
            highchartOutput(ns("chart_compare_revenue"), height = "440px"),
            uiOutput(ns("compare_revenue_fill_caption_ui")),
            downloadButton(ns("dl_compare_revenue"), "Download chart data (CSV)", class = "dl-btn")
          ),
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Revenue comparison — snapshot year"),
            p(class = "panel-note", "Same countries, one year at a time, broken down by sector."),
            div(class = "panel-toolbar", uiOutput(ns("snap_year_ui"))),
            uiOutput(ns("chart_snapshot_ui")),
            downloadButton(ns("dl_snapshot"), "Download chart data (CSV)", class = "dl-btn")
          ),
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Market concentration"),
            p(class = "panel-note",
              "CR4 and HHI over time. Pick one or more sectors to compare — each line is a country/sector ",
              "combination. Where a year is missing, the nearest year within ±2 years is used and marked with an asterisk (*) in the tooltip."),
            div(class = "panel-toolbar",
              selectInput(ns("conc_metric"), "Metric", choices = c(
                "CR4 (4-firm concentration ratio, %)" = "cr4", "HHI (Herfindahl-Hirschman Index)" = "hhi"
              ), width = "300px"),
              selectInput(ns("conc_view"), "View as", choices = c("Line chart" = "line", "Heatmap" = "heatmap"),
                          selected = "line", width = "160px"),
              uiOutput(ns("conc_sector_ui"))
            ),
            uiOutput(ns("conc_chart_ui")),
            uiOutput(ns("conc_heatmap_legend_ui")),
            uiOutput(ns("conc_fill_caption_ui")),
            downloadButton(ns("dl_concentration"), "Download chart data (CSV)", class = "dl-btn")
          )
        )
      } else {
        req(length(selection()) == 1)
        tagList(
          uiOutput(ns("stats")),
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Revenue by sector, over time"),
            p(class = "panel-note",
              "Stacked by sector, so you can see both the overall trend and its composition. Data points are ",
              "shown every 4 years through 2000, every 2 years from 2002–2010, and every year from 2011 on. Where ",
              "a sector is missing a shown year, the nearest year within ±2 years is used and marked with an asterisk (*) in the tooltip."),
            highchartOutput(ns("chart_sector_time"), height = "460px"),
            uiOutput(ns("sector_time_fill_caption")),
            downloadButton(ns("dl_sector_time"), "Download chart data (CSV)", class = "dl-btn")
          ),
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Compare sectors"),
            p(class = "panel-note",
              "Pick two or more sectors or sector groups to compare against each other for this country — ",
              "independent of the Sectors filter above, which scopes every other chart on this page. Data points ",
              "are shown every 4 years through 2000, every 2 years from 2002–2010, and every year from 2011 on. ",
              "Where a pick is missing a shown year, the nearest year within ±2 years is used and marked with an asterisk (*) in the tooltip."),
            div(class = "panel-toolbar", uiOutput(ns("sector_compare_pick_ui"))),
            highchartOutput(ns("chart_sector_compare_time"), height = "420px"),
            uiOutput(ns("sector_compare_fill_caption")),
            downloadButton(ns("dl_sector_compare_time"), "Download chart data (CSV)", class = "dl-btn")
          ),
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Compare sectors — snapshot year"),
            p(class = "panel-note", "Same picks as above, one year at a time, side by side."),
            div(class = "panel-toolbar", uiOutput(ns("sector_compare_snap_year_ui"))),
            uiOutput(ns("chart_sector_compare_snap_ui")),
            downloadButton(ns("dl_sector_compare_snap"), "Download chart data (CSV)", class = "dl-btn")
          ),
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Top companies"),
            div(class = "panel-toolbar",
              selectInput(ns("top_n"), "Show top", choices = c(5, 10, 20, 50), selected = 20, width = "110px"),
              uiOutput(ns("companies_year_ui"))
            ),
            uiOutput(ns("chart_companies_ui")),
            downloadButton(ns("dl_companies"), "Download chart data (CSV)", class = "dl-btn")
          ),
          div(class = "panel-card panel-card-wide",
            h3(class = "panel-title", "Market concentration"),
            p(class = "panel-note",
              "CR4 and HHI over time. Pick one or more sectors to compare — each line is a sector. Where a year is ",
              "missing, the nearest year within ±2 years is used and marked with an asterisk (*) in the tooltip."),
            div(class = "panel-toolbar",
              selectInput(ns("conc_metric"), "Metric", choices = c(
                "CR4 (4-firm concentration ratio, %)" = "cr4", "HHI (Herfindahl-Hirschman Index)" = "hhi"
              ), width = "300px"),
              selectInput(ns("conc_view"), "View as", choices = c("Line chart" = "line", "Heatmap" = "heatmap"),
                          selected = "line", width = "160px"),
              uiOutput(ns("conc_sector_ui"))
            ),
            uiOutput(ns("conc_chart_ui")),
            uiOutput(ns("conc_heatmap_legend_ui")),
            uiOutput(ns("conc_fill_caption_ui")),
            downloadButton(ns("dl_concentration"), "Download chart data (CSV)", class = "dl-btn")
          )
        )
      }
    })

    # --- KPI stats (single mode) ---

    country_stats <- reactive({
      req(!is_compare(), length(selection()) == 1)
      country <- selection()
      rc <- rev_col()

      # Gap-filled totals (not a raw sum) so the year-over-year change -- and the
      # sparkline trend -- reflect real growth rather than an artifact of different
      # years having different sector coverage.
      rev_totals <- build_revenue_totals(data$revenue, selection_countries(), sector_sel(), gf$years(), rc)$df
      validate(need(nrow(rev_totals) > 0, "No revenue data for this country."))
      # One row per (Year, Country) coming out of build_revenue_totals() -- collapse
      # to one row per Year (a no-op sum for a single selected country, a real sum
      # across countries for Worldwide) so every Year lookup below is a scalar.
      rev_totals <- rev_totals %>% group_by(Year) %>% summarise(!!rc := sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>% as.data.frame()
      # Capped at the selected snapshot year (see target_yr above) -- falls
      # back to the nearest earlier year with data if the exact one is sparse.
      rev_totals <- rev_totals[rev_totals$Year <= target_yr(), , drop = FALSE]
      validate(need(nrow(rev_totals) > 0, "No revenue data for this country."))
      rev_totals <- rev_totals[order(rev_totals$Year), ]
      latest_yr <- max(rev_totals$Year)
      latest_total <- rev_totals[[rc]][rev_totals$Year == latest_yr]
      prior_row <- rev_totals[rev_totals$Year == latest_yr - 1, ]
      yoy_delta <- if (nrow(prior_row) == 1 && !is.na(prior_row[[rc]]) && prior_row[[rc]] > 0) {
        (latest_total - prior_row[[rc]]) / prior_row[[rc]] * 100
      } else NA_real_

      first_yr <- min(rev_totals$Year)
      first_total <- rev_totals[[rc]][rev_totals$Year == first_yr]
      rev_detail <- if (first_yr < latest_yr) paste0("Since ", first_yr, ": ", fmt_money(first_total)) else NULL

      rev_d <- data$revenue[country_mask(data$revenue$Country, country) & !is.na(data$revenue[[rc]]) &
                               data$revenue$Year <= target_yr(), , drop = FALSE]
      sector_counts <- rev_d %>% group_by(Year) %>% summarise(n = n_distinct(Sector), .groups = "drop") %>% arrange(Year)
      n_sectors <- sector_counts$n[sector_counts$Year == latest_yr]
      peak_row <- sector_counts[which.max(sector_counts$n), ]
      sectors_detail <- paste0("Peak: ", peak_row$n, " sectors in ", peak_row$Year)

      comp_d <- data$unified[country_mask(data$unified$Country, country) & !is.na(data$unified[[rc]]) &
                                !data$unified$is_aggregate & data$unified$Year <= target_yr(), , drop = FALSE]
      top_company <- "N/A"
      company_detail <- NULL
      company_trend <- NULL
      if (nrow(comp_d) > 0) {
        comp_latest <- comp_d[comp_d$Year == max(comp_d$Year), , drop = FALSE]
        if (nrow(comp_latest) > 0) {
          agg <- comp_latest %>% group_by(parent) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>% arrange(desc(t))
          if (nrow(agg) > 0) {
            top_company <- agg$parent[1]
            company_detail <- paste0(fmt_money(agg$t[1]), " in ", max(comp_d$Year))
            # The sparkline can look like it's falling even when the company's own
            # reporting-currency revenue is flat/growing, purely because of USD/local
            # exchange-rate movement -- flag that so it isn't read as a real decline.
            if (identical(gf$currency(), "usd") && !is_worldwide(country)) {
              company_detail <- paste0(company_detail, " · in USD — exchange-rate moves can shift this trend")
            }
            company_trend <- comp_d[comp_d$parent == top_company, ] %>%
              group_by(Year) %>% summarise(v = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>% arrange(Year)
          }
        }
      }

      conc_d <- data$concentration[country_mask(data$concentration$Country, country) & !is.na(data$concentration$cr4) &
                                      data$concentration$Year <= target_yr(), , drop = FALSE]
      top_conc <- "N/A"
      conc_detail <- NULL
      sector_trend <- NULL
      if (nrow(conc_d) > 0) {
        conc_latest <- conc_d[conc_d$Year == max(conc_d$Year), , drop = FALSE]
        if (nrow(conc_latest) > 0) {
          conc_latest <- conc_latest[order(-conc_latest$cr4), ]
          top_conc_sector <- conc_latest$Sector[1]
          top_conc <- paste0(top_conc_sector, " (", round(conc_latest$cr4[1]), "%)")
          if (!is.na(conc_latest$hhi[1])) conc_detail <- paste0("HHI: ", round(conc_latest$hhi[1]), " in ", max(conc_d$Year))
          sector_trend <- conc_d[conc_d$Sector == top_conc_sector, ] %>%
            group_by(Year) %>% summarise(v = mean(cr4, na.rm = TRUE), .groups = "drop") %>% arrange(Year)
        }
      }

      list(
        latest_yr = latest_yr, latest_total = latest_total, yoy_delta = yoy_delta, rev_detail = rev_detail,
        n_sectors = n_sectors, sectors_detail = sectors_detail,
        top_company = top_company, company_detail = company_detail,
        top_conc = top_conc, conc_detail = conc_detail,
        rev_trend = rev_totals[[rc]], sector_count_trend = sector_counts$n,
        company_trend = company_trend$v, sector_trend = sector_trend$v
      )
    })

    output$stats <- renderUI({
      s <- country_stats()
      div(class = "stat-strip",
        stat_card(fmt_money(s$latest_total), paste0("Total revenue, ", s$latest_yr), delta = s$yoy_delta,
                   detail = s$rev_detail, icon = "cash-stack",
                   sparkline = highchartOutput(ns("spark_revenue"), height = "100%")),
        stat_card(s$n_sectors, paste0("Sectors reported, ", s$latest_yr),
                   detail = s$sectors_detail, icon = "grid-3x3-gap",
                   sparkline = highchartOutput(ns("spark_sectors"), height = "100%")),
        stat_card(s$top_company, "Leading company",
                   detail = s$company_detail, icon = "building",
                   sparkline = if (!is.null(s$company_trend)) highchartOutput(ns("spark_company"), height = "100%")),
        stat_card(s$top_conc, "Most concentrated sector (CR4)",
                   detail = s$conc_detail, icon = "pie-chart",
                   sparkline = if (!is.null(s$sector_trend)) highchartOutput(ns("spark_sector_conc"), height = "100%"))
      )
    })

    output$spark_revenue <- renderHighchart(gmicp_sparkline(country_stats()$rev_trend, "#6C5CE7"))
    output$spark_sectors <- renderHighchart(gmicp_sparkline(country_stats()$sector_count_trend, "#17B8A6"))
    output$spark_company <- renderHighchart({
      req(country_stats()$company_trend)
      gmicp_sparkline(country_stats()$company_trend, "#F5A623")
    })
    output$spark_sector_conc <- renderHighchart({
      req(country_stats()$sector_trend)
      gmicp_sparkline(country_stats()$sector_trend, "#F2618C")
    })

    # --- Single mode: revenue by sector over time ---

    sector_time_agg <- reactive({
      req(!is_compare(), length(selection()) == 1)
      rc <- rev_col()
      d <- apply_filters(data$revenue, selection_countries(), sector_sel(), gf$years())
      req(nrow(d) > 0)
      raw <- d %>% group_by(Year, Sector) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop")
      names(raw)[names(raw) == "value"] <- rc
      res <- fill_year_gaps(as.data.frame(raw), "Sector", rc)
      res$cap_year <- latest_country_chart_year(d)
      res
    })

    output$chart_sector_time <- renderHighchart({
      res <- sector_time_agg()
      d <- res$df
      validate(need(nrow(d) > 0, "No revenue data for this country."))
      rc <- rev_col()
      ticks <- data_include_years(max(d$Year), cap = res$cap_year)
      d <- d[d$Year %in% ticks, , drop = FALSE]
      # Ascending (smallest first): the first series added sits at the top of a
      # Highcharts stacked column, so this puts the smallest sector at the top of
      # each bar and the biggest at the bottom, against the x-axis -- legend
      # order is flipped back to biggest-first with hc_legend(reversed = TRUE)
      # below, so it doesn't read top-to-bottom smallest-first too.
      sector_order <- d %>% group_by(Sector) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>%
        arrange(t) %>% pull(Sector)
      years_chr <- as.character(sort(unique(d$Year)))
      d$Year <- as.character(d$Year)
      d$note <- fill_tooltip_note(d$filled)

      gmicp_stacked_chart(d, "Year", rc, "Sector", years_chr, sector_order, extra_col = "note") %>%
        hc_xAxis(title = list(text = ""), labels = list(rotation = -45)) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_legend(reversed = TRUE) %>%
        hc_tooltip(shared = TRUE, useHTML = TRUE, formatter = gmicp_shared_tooltip_js(FALSE, show_total = TRUE)) %>%
        apply_gmicp_theme()
    })

    output$sector_time_fill_caption <- renderUI({
      n <- sum(sector_time_agg()$df$filled)
      if (n == 0) return(NULL)
      div(class = "fill-note", bs_icon("exclamation-circle", size = "0.85rem"), " ",
          paste0("Data note: ", format(n, big.mark = ","), " sector/year point", if (n != 1) "s" else "",
                 " on this chart ", if (n != 1) "were" else "was",
                 " missing and filled from the nearest year within ±2 years. Points marked with an asterisk (*) in the tooltip are estimated."))
    })

    output$dl_sector_time <- downloadHandler(
      filename = function() paste0("gmicp_", gsub(" ", "_", selection()), "_revenue_by_sector.csv"),
      content = function(file) write.csv(sector_time_agg()$df %>% select(-source_year), file, row.names = FALSE)
    )

    # --- Single mode: compare sectors (independent of the global Sectors filter) ---

    output$sector_compare_pick_ui <- renderUI({
      grp <- sector_group_choices()
      choices <- c(setNames(all_sectors, all_sectors), setNames(paste0("grp:", grp), names(grp)))
      selectizeInput(ns("sector_compare_pick"), NULL, choices = choices, multiple = TRUE,
                      options = list(placeholder = "e.g. Broadcast TV, Online Video Services, Traditional Media Services..."))
    })

    # Each pick resolves to a label + the flat sector vector behind it -- a
    # plain sector is its own one-item vector, a group id expands via
    # sector_group_sectors() (same helper the global Sector Groups filter uses).
    sector_compare_items <- reactive({
      picks <- input$sector_compare_pick
      req(length(picks) > 0)
      lapply(picks, function(p) {
        if (startsWith(p, "grp:")) {
          id <- sub("^grp:", "", p)
          list(label = sector_group_label(id), sectors = sector_group_sectors(id))
        } else {
          list(label = p, sectors = p)
        }
      })
    })

    sector_compare_agg <- reactive({
      req(!is_compare(), length(selection()) == 1)
      items <- sector_compare_items()
      rc <- rev_col()
      raw_list <- lapply(items, function(it) {
        d <- apply_filters(data$revenue, selection_countries(), it$sectors, gf$years())
        if (nrow(d) == 0) return(NULL)
        d %>% group_by(Year) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
          mutate(label = it$label)
      })
      raw <- bind_rows(raw_list)
      req(nrow(raw) > 0)
      names(raw)[names(raw) == "value"] <- rc
      fill_year_gaps(as.data.frame(raw), "label", rc)
    })

    output$chart_sector_compare_time <- renderHighchart({
      res <- sector_compare_agg()
      d <- res$df
      validate(need(nrow(d) > 0, "No revenue data for the selected sectors."))
      rc <- rev_col()
      ticks <- data_include_years(max(d$Year))
      d <- d[d$Year %in% ticks, , drop = FALSE]
      d$note <- fill_tooltip_note(d$filled)
      gmicp_line_chart(d, "Year", rc, "label", rev_label(), sort_tooltip_desc = TRUE, show_markers = FALSE, note_col = "note")
    })

    output$sector_compare_fill_caption <- renderUI({
      n <- sum(sector_compare_agg()$df$filled)
      if (n == 0) return(NULL)
      div(class = "fill-note", bs_icon("exclamation-circle", size = "0.85rem"), " ",
          paste0("Data note: ", format(n, big.mark = ","), " point", if (n != 1) "s" else "",
                 " on this chart ", if (n != 1) "were" else "was",
                 " missing and filled from the nearest year within ±2 years. Points marked with an asterisk (*) in the tooltip are estimated."))
    })

    output$dl_sector_compare_time <- downloadHandler(
      filename = function() paste0("gmicp_", gsub(" ", "_", selection()), "_sector_comparison_over_time.csv"),
      content = function(file) write.csv(sector_compare_agg()$df %>% select(-source_year), file, row.names = FALSE)
    )

    output$sector_compare_snap_year_ui <- renderUI({
      items <- sector_compare_items()
      all_sec <- unique(unlist(lapply(items, function(it) it$sectors)))
      yrs <- sort(unique(apply_filters(data$revenue, selection_countries(), all_sec, gf$years())$Year), decreasing = TRUE)
      req(length(yrs) > 0)
      default <- if (DEFAULT_SNAPSHOT_YEAR %in% yrs) DEFAULT_SNAPSHOT_YEAR else yrs[1]
      selectInput(ns("sector_compare_snap_year"), "Snapshot year", choices = yrs, selected = default, width = "160px")
    })

    sector_compare_snap_data <- reactive({
      req(input$sector_compare_snap_year)
      items <- sector_compare_items()
      rc <- rev_col()
      rows <- lapply(items, function(it) {
        d <- apply_filters(data$revenue, selection_countries(), it$sectors, gf$years())
        d <- d[d$Year == as.numeric(input$sector_compare_snap_year), , drop = FALSE]
        data.frame(label = it$label, value = if (nrow(d) == 0) 0 else sum(d[[rc]], na.rm = TRUE))
      })
      out <- bind_rows(rows)
      names(out)[names(out) == "value"] <- rc
      out
    })

    output$chart_sector_compare_snap_ui <- renderUI({
      n <- length(unique(sector_compare_snap_data()$label))
      highchartOutput(ns("chart_sector_compare_snap"), height = paste0(max(300, n * 40 + 60), "px"))
    })

    output$chart_sector_compare_snap <- renderHighchart({
      d <- sector_compare_snap_data()
      validate(need(nrow(d) > 0, "No data for the current selection."))
      rc <- rev_col()
      d <- d %>% arrange(.data[[rc]])
      cmap <- build_color_map(d$label)
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_xAxis(categories = as.list(d$label), title = list(text = "")) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_add_series(name = rev_label(), data = as.numeric(d[[rc]]), colorByPoint = TRUE,
                       colors = unname(cmap[d$label])) %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.1f}</b>") %>%
        hc_legend(enabled = FALSE) %>%
        apply_gmicp_theme()
    })

    output$dl_sector_compare_snap <- downloadHandler(
      filename = function() paste0("gmicp_", gsub(" ", "_", selection()), "_sector_comparison_snapshot_", input$sector_compare_snap_year, ".csv"),
      content = function(file) write.csv(sector_compare_snap_data(), file, row.names = FALSE)
    )

    # --- Single mode: top companies ---

    companies_filtered <- reactive({
      req(!is_compare(), length(selection()) == 1)
      d <- apply_filters(data$unified, selection_countries(), sector_sel(), gf$years())
      d[!is.na(d$parent) & d$parent != "nan" & d$parent != "" & !d$is_aggregate, , drop = FALSE]
    })

    output$companies_year_ui <- renderUI({
      yrs <- sort(unique(companies_filtered()$Year), decreasing = TRUE)
      req(length(yrs) > 0)
      default <- if (DEFAULT_SNAPSHOT_YEAR %in% yrs) DEFAULT_SNAPSHOT_YEAR else yrs[1]
      selectInput(ns("companies_year"), "Year", choices = yrs, selected = default, width = "140px")
    })

    companies_snap <- reactive({
      req(input$companies_year)
      rc <- rev_col()
      s <- companies_filtered()[companies_filtered()$Year == as.numeric(input$companies_year), , drop = FALSE]
      s <- s[!is.na(s[[rc]]), , drop = FALSE]
      validate(need(nrow(s) > 0, paste0("No company revenue data for ", selection(), " in ", input$companies_year, ".")))
      top_n <- as.numeric(input$top_n)
      top_parents <- s %>% group_by(parent) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>%
        arrange(desc(t)) %>% slice_head(n = top_n) %>% pull(parent)
      s %>% filter(parent %in% top_parents) %>%
        group_by(parent, Sector) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
        rename(!!rc := value) -> agg
      list(agg = as.data.frame(agg), top_parents = top_parents)
    })

    output$chart_companies_ui <- renderUI({
      n <- tryCatch(as.numeric(input$top_n), error = function(e) 20)
      highchartOutput(ns("chart_companies"), height = paste0(max(400, n * 32), "px"))
    })

    output$chart_companies <- renderHighchart({
      res <- companies_snap()
      rc <- rev_col()
      sectors <- sort(unique(res$agg$Sector))
      parent_order <- rev(res$top_parents)
      gmicp_stacked_chart(res$agg, "parent", rc, "Sector", parent_order, sectors, orientation = "h") %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_tooltip(shared = FALSE, valueDecimals = 1) %>%
        apply_gmicp_theme()
    })

    output$dl_companies <- downloadHandler(
      filename = function() paste0("gmicp_top_companies_", gsub(" ", "_", selection()), "_", input$companies_year, ".csv"),
      content = function(file) write.csv(companies_snap()$agg, file, row.names = FALSE)
    )

    # --- Compare mode: revenue over time ---

    compare_revenue_agg <- reactive({
      req(is_compare(), length(selection()) > 1, gf$years())
      build_revenue_totals(data$revenue, selection(), sector_sel(), gf$years(), rev_col())
    })

    output$chart_compare_revenue <- renderHighchart({
      res <- compare_revenue_agg()
      d <- res$df
      validate(need(nrow(d) > 0, "No revenue data for this selection."))
      rc <- rev_col()
      ticks <- data_include_years(max(d$Year))
      d <- d[d$Year %in% ticks, , drop = FALSE]
      d$note <- fill_tooltip_note(d$filled)
      gmicp_line_chart(d, "Year", rc, "Country", rev_label(),
                        sort_tooltip_desc = TRUE, show_markers = FALSE, note_col = "note")
    })

    output$compare_revenue_fill_caption_ui <- renderUI({
      cap <- revenue_fill_caption(compare_revenue_agg())
      if (is.null(cap)) return(NULL)
      div(class = "fill-note", bs_icon("exclamation-circle", size = "0.85rem"), " ", cap)
    })

    output$dl_compare_revenue <- downloadHandler(
      filename = function() "gmicp_revenue_over_time_comparison.csv",
      content = function(file) write.csv(compare_revenue_agg()$df %>% select(-filled, -source_year), file, row.names = FALSE)
    )

    # --- Compare mode: snapshot year ---

    output$snap_year_ui <- renderUI({
      yrs <- sort(unique(apply_filters(data$revenue, selection(), sector_sel(), gf$years())$Year), decreasing = TRUE)
      req(length(yrs) > 0)
      default <- if (DEFAULT_SNAPSHOT_YEAR %in% yrs) DEFAULT_SNAPSHOT_YEAR else yrs[1]
      selectInput(ns("snap_year"), "Snapshot year", choices = yrs, selected = default, width = "160px")
    })

    snap_data <- reactive({
      req(input$snap_year, is_compare(), length(selection()) > 1)
      rc <- rev_col()
      d <- apply_filters(data$revenue, selection(), sector_sel(), gf$years())
      d <- d[d$Year == as.numeric(input$snap_year), , drop = FALSE]
      d %>% group_by(Country, Sector) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
        rename(!!rc := value)
    })

    output$chart_snapshot_ui <- renderUI({
      n <- length(unique(snap_data()$Country))
      highchartOutput(ns("chart_snapshot"), height = paste0(max(300, n * 32 + 60), "px"))
    })

    output$chart_snapshot <- renderHighchart({
      d <- snap_data()
      validate(need(nrow(d) > 0, "No data for the current filter selection."))
      rc <- rev_col()
      # Ascending, not descending -- gmicp_stacked_chart()'s horizontal ("h")
      # bars draw categories bottom-to-top, so the smallest-first order here is
      # what puts the biggest country at the top of the chart (same convention
      # as the Leading companies and company-footprint horizontal bars).
      country_order <- d %>% group_by(Country) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>%
        arrange(t) %>% pull(Country)
      sector_order <- d %>% group_by(Sector) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>%
        arrange(desc(t)) %>% pull(Sector)

      gmicp_stacked_chart(d, "Country", rc, "Sector", country_order, sector_order, orientation = "h") %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_tooltip(shared = FALSE, valueDecimals = 1) %>%
        apply_gmicp_theme()
    })

    output$dl_snapshot <- downloadHandler(
      filename = function() paste0("gmicp_revenue_snapshot_", input$snap_year, ".csv"),
      content = function(file) write.csv(snap_data(), file, row.names = FALSE)
    )

    # --- Concentration (shared between both modes -- build_concentration_series() already
    # handles n_countries==1 vs >1 via its own series-labeling logic) ---

    output$conc_sector_ui <- renderUI({
      default <- if (length(sector_sel()) > 0) {
        sector_sel()
      } else {
        data$concentration %>% filter(!is.na(cr4)) %>% count(Sector, sort = TRUE) %>%
          slice_head(n = 4) %>% pull(Sector)
      }
      selectizeInput(ns("conc_sectors"), "Sectors to compare", choices = all_sectors, selected = default,
                      multiple = TRUE, options = list(plugins = list("remove_button")), width = "420px")
    })

    conc_agg <- reactive({
      req(length(selection()) > 0, length(input$conc_sectors) > 0, input$conc_metric, gf$years())
      build_concentration_series(data$concentration, selection_countries(), input$conc_sectors, gf$years(), input$conc_metric,
                                  collapse_countries = !is_compare() && is_worldwide(selection()))
    })

    output$chart_concentration <- renderHighchart({
      d <- conc_agg()
      validate(need(nrow(d) > 0, "No data for this selection."))
      mc <- input$conc_metric
      metric_label <- if (mc == "hhi") "HHI" else "CR4 (%)"
      d$note <- fill_tooltip_note(d$filled)
      hc <- gmicp_line_chart(d, "Year", mc, "series", metric_label,
                              sort_tooltip_desc = TRUE, show_markers = FALSE, note_col = "note")
      if (mc == "cr4") hc <- hc %>% hc_yAxis(min = 0, max = 100)
      if (mc == "hhi") hc <- hc %>% hc_yAxis(min = 0, max = 10000)
      hc
    })

    output$chart_concentration_heatmap <- renderHighchart({
      d <- conc_agg()
      validate(need(nrow(d) > 0, "No data for this selection."))
      gmicp_concentration_heatmap(d, input$conc_metric)
    })

    output$conc_chart_ui <- renderUI({
      if (identical(input$conc_view, "heatmap")) {
        n <- length(unique(conc_agg()$series))
        highchartOutput(ns("chart_concentration_heatmap"), height = paste0(max(300, n * 26 + 90), "px"))
      } else {
        highchartOutput(ns("chart_concentration"), height = "460px")
      }
    })

    output$conc_heatmap_legend_ui <- renderUI({
      req(identical(input$conc_view, "heatmap"))
      ranges <- concentration_band_ranges(input$conc_metric)
      div(class = "concentration-legend",
        lapply(seq_along(CONCENTRATION_BAND_LABELS), function(i) {
          span(class = "concentration-legend-item",
            span(class = "concentration-legend-swatch", style = paste0("background:", CONCENTRATION_BAND_COLORS[i], ";")),
            paste0(CONCENTRATION_BAND_LABELS[i], " (", ranges[i], ")")
          )
        })
      )
    })

    output$conc_fill_caption_ui <- renderUI({
      cap <- conc_fill_caption(conc_agg())
      if (is.null(cap)) return(NULL)
      div(class = "fill-note", bs_icon("exclamation-circle", size = "0.85rem"), " ", cap)
    })

    output$dl_concentration <- downloadHandler(
      filename = function() paste0("gmicp_concentration_", input$conc_metric, ".csv"),
      content = function(file) write.csv(conc_agg(), file, row.names = FALSE)
    )

    # Exposed so the sticky header (app.R) can show the current country
    # selection alongside the global Sector/Years filters -- is_compare so it
    # can skip showing the (potentially long) compare-mode country list there.
    list(selection = selection, is_compare = is_compare)
  })
}
