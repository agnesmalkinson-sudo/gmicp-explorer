library(shiny)
library(highcharter)
library(dplyr)
library(bsicons)

mod_companies_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "panel-card panel-card-wide",
      uiOutput(ns("profile_title")),
      uiOutput(ns("profile_note")),
      div(class = "focus-row",
        div(class = "focus-search",
          tags$label(class = "filter-label", "Filter by country"),
          uiOutput(ns("profile_country_pick_ui"))
        ),
        uiOutput(ns("profile_focus_hint"))
      ),
      div(class = "filter-group", style = "max-width: 360px;",
        tags$label(class = "filter-label", "Search for a company"),
        uiOutput(ns("company_pick_ui"))
      ),
      uiOutput(ns("profile_sections"))
    ),
    div(class = "panel-card panel-card-wide map-card",
      h3(class = "panel-title", "Filter by country"),
      p(class = "panel-note",
        "Click a country on the map, or search below, to rank leading companies within that country instead of ",
        "worldwide. The map always shows figures in US dollars, for comparability."),
      div(class = "panel-toolbar",
        uiOutput(ns("map_year_ui"))
      ),
      uiOutput(ns("map_sector_note")),
      highchartOutput(ns("map"), height = "500px"),
      div(class = "focus-row",
        div(class = "focus-search",
          tags$label(class = "filter-label", "Search for a country"),
          uiOutput(ns("country_pick_ui"))
        ),
        uiOutput(ns("focus_hint"))
      )
    ),
    div(class = "panel-card panel-card-wide",
      uiOutput(ns("leaders_title")),
      uiOutput(ns("leaders_note")),
      div(class = "panel-toolbar",
        selectInput(ns("top_n"), "Show top", choices = c(5, 10, 20, 50), selected = 20, width = "110px"),
        checkboxInput(ns("exclude_others"), "Exclude 'Others' aggregate rows", value = TRUE),
        checkboxInput(ns("compare_years"), "Compare years", value = FALSE),
        uiOutput(ns("stack_by_ui")),
        uiOutput(ns("leader_year_ui"))
      ),
      uiOutput(ns("chart_leaders_ui")),
      downloadButton(ns("dl_leaders"), "Download chart data (CSV)", class = "dl-btn")
    )
  )
}

mod_companies_server <- function(id, data, all_countries, all_sectors, all_years, gf, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rev_col <- reactive(currency_col(gf$currency()))
    rev_label <- reactive(currency_label(gf$currency()))
    sector_sel <- reactive({
      s <- gf$sectors()
      if (is.null(s) || identical(s, "")) character(0) else s
    })

    # ---------------- Country map filter ----------------

    output$map_year_ui <- renderUI({
      selectInput(ns("map_year"), "Snapshot year", choices = rev(seq(all_years[1], all_years[2])),
                  selected = min(DEFAULT_SNAPSHOT_YEAR, all_years[2]), width = "160px")
    })

    output$map_sector_note <- renderUI({
      if (length(sector_sel()) == 0) {
        div(class = "panel-scope-note", "Using the Sectors filter above. Currently: all sectors (revenue is summed).")
      } else {
        div(class = "panel-scope-note", paste0("Using the Sectors filter above. Currently: ", paste(sector_sel(), collapse = ", "), "."))
      }
    })

    output$map <- renderHighchart({
      req(input$map_year)
      build_country_map(data, "revenue", sector_sel(), as.numeric(input$map_year), ns("map_click"), categorical = TRUE)
    })

    observeEvent(input$map_click, {
      key <- tolower(input$map_click$key %||% "")
      matched <- ISO2_TO_COUNTRY[[key]]
      if (is.null(matched)) return(invisible(NULL))
      updateSelectizeInput(session, "country_pick", selected = matched)
    })

    WORLDWIDE <- "__worldwide__"

    output$country_pick_ui <- renderUI({
      selectizeInput(ns("country_pick"), NULL, choices = c("Worldwide (all countries)" = WORLDWIDE, all_countries),
                      selected = WORLDWIDE, multiple = FALSE, options = list(placeholder = "Type a country name..."))
    })

    output$focus_hint <- renderUI({
      if (is.null(input$country_pick) || !nzchar(input$country_pick) || input$country_pick == WORLDWIDE) {
        div(class = "focus-hint", "Showing worldwide — click the map or search above to narrow to one country.")
      } else {
        div(class = "focus-hint", paste0("Showing: ", input$country_pick, " "),
            actionLink(ns("clear_country_filter"), "Back to worldwide", class = "focus-hint-clear"))
      }
    })

    observeEvent(input$clear_country_filter, updateSelectizeInput(session, "country_pick", selected = WORLDWIDE))

    leader_country <- reactive({
      c <- input$country_pick
      if (is.null(c) || !nzchar(c) || c == WORLDWIDE) character(0) else c
    })

    # ---------------- Company profile's own country filter ----------------
    # Independent of the "Filter by country" map below (which scopes the map
    # and "Leading companies" leaderboard) -- the profile card sits above that
    # map now, so it needs its own scope for the company search list and the
    # Rank chart rather than depending on a filter the user hasn't seen yet.
    # Same Worldwide-default pattern as the map's picker.

    output$profile_country_pick_ui <- renderUI({
      selectizeInput(ns("profile_country_pick"), NULL, choices = c("Worldwide (all countries)" = WORLDWIDE, all_countries),
                      selected = WORLDWIDE, multiple = FALSE, options = list(placeholder = "Type a country name..."))
    })

    output$profile_focus_hint <- renderUI({
      if (is.null(input$profile_country_pick) || !nzchar(input$profile_country_pick) || input$profile_country_pick == WORLDWIDE) {
        div(class = "focus-hint", "Showing worldwide — search above to narrow to one country.")
      } else {
        div(class = "focus-hint", paste0("Showing: ", input$profile_country_pick, " "),
            actionLink(ns("clear_profile_country_filter"), "Back to worldwide", class = "focus-hint-clear"))
      }
    })

    observeEvent(input$clear_profile_country_filter, updateSelectizeInput(session, "profile_country_pick", selected = WORLDWIDE))

    profile_country <- reactive({
      c <- input$profile_country_pick
      if (is.null(c) || !nzchar(c) || c == WORLDWIDE) character(0) else c
    })

    # ---------------- Leading companies ----------------

    output$leaders_title <- renderUI({
      txt <- if (length(leader_country()) == 0) "Leading companies worldwide" else paste0("Leading companies in ", leader_country())
      h3(class = "panel-title", txt)
    })

    output$leaders_note <- renderUI({
      if (isTRUE(input$compare_years)) {
        p(class = "panel-note",
          "Comparing the years picked below — each company's bar is grouped by year instead of stacked by country ",
          "or sector, ranked by its total revenue across those years combined.")
      } else if (length(leader_country()) == 0) {
        stack_by <- input$worldwide_stack_by %||% "Country"
        stack_desc <- if (stack_by == "Country") "the countries that company reports revenue in" else "sector"
        p(class = "panel-note",
          "Ranked across every country in the dataset (respecting the Sectors and Years filters above) — not just ",
          "one. Each bar is stacked by ", stack_desc, " -- switch with \"Stack bars by\" below.")
      } else {
        p(class = "panel-note",
          "Ranked within the selected country only (respecting the Sectors and Years filters above). Each bar is ",
          "stacked by sector.")
      }
    })

    output$stack_by_ui <- renderUI({
      req(length(leader_country()) == 0, !isTRUE(input$compare_years))
      radioButtons(ns("worldwide_stack_by"), "Stack bars by", c("Country" = "Country", "Sector" = "Sector"),
                   selected = "Country", inline = TRUE)
    })

    base_filtered <- reactive({
      d <- apply_filters(data$unified, leader_country(), sector_sel(), gf$years())
      if (isTRUE(input$exclude_others) && "is_aggregate" %in% names(d)) d <- d[!d$is_aggregate, , drop = FALSE]
      d[!is.na(d$parent) & d$parent != "nan" & d$parent != "", , drop = FALSE]
    })

    # Same shape as base_filtered() above, but scoped by the profile card's
    # own country filter instead of the map's -- used only by the profile's
    # Rank chart, which now sits in a card above the map/leaderboard. Always
    # excludes "Others" aggregate rows (no separate toggle for this card;
    # TRUE was base_filtered()'s original default too).
    profile_base_filtered <- reactive({
      d <- apply_filters(data$unified, profile_country(), sector_sel(), gf$years())
      if ("is_aggregate" %in% names(d)) d <- d[!d$is_aggregate, , drop = FALSE]
      d[!is.na(d$parent) & d$parent != "nan" & d$parent != "", , drop = FALSE]
    })

    output$leader_year_ui <- renderUI({
      yrs <- sort(unique(base_filtered()$Year), decreasing = TRUE)
      req(length(yrs) > 0)
      if (isTRUE(input$compare_years)) {
        default <- rev(utils::head(sort(yrs, decreasing = TRUE), 2))
        selectizeInput(ns("leader_years"), "Years to compare", choices = sort(yrs), selected = default,
                        multiple = TRUE, options = list(plugins = list("remove_button"), maxItems = 6), width = "280px")
      } else {
        default <- if (DEFAULT_SNAPSHOT_YEAR %in% yrs) DEFAULT_SNAPSHOT_YEAR else yrs[1]
        selectInput(ns("leader_year"), "Year", choices = yrs, selected = default, width = "140px")
      }
    })

    leaders_snap <- reactive({
      rc <- rev_col()
      top_n <- as.numeric(input$top_n)

      if (isTRUE(input$compare_years)) {
        req(length(input$leader_years) >= 2)
        yrs_sel <- sort(as.numeric(input$leader_years))
        s <- base_filtered()[base_filtered()$Year %in% yrs_sel, , drop = FALSE]
        s <- s[!is.na(s[[rc]]), , drop = FALSE]
        validate(need(nrow(s) > 0, "No company revenue data for the selected years."))
        top_parents <- s %>% group_by(parent) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>%
          arrange(desc(t)) %>% slice_head(n = top_n) %>% pull(parent)
        s %>% filter(parent %in% top_parents) %>% mutate(Year = as.character(Year)) %>%
          group_by(parent, Year) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
          rename(!!rc := value) -> agg
        list(agg = as.data.frame(agg), top_parents = top_parents, stack_col = "Year",
             stack_values = as.character(yrs_sel), stacked = FALSE)
      } else {
        req(input$leader_year)
        s <- base_filtered()[base_filtered()$Year == as.numeric(input$leader_year), , drop = FALSE]
        s <- s[!is.na(s[[rc]]), , drop = FALSE]
        validate(need(nrow(s) > 0, paste0("No company revenue data for ", input$leader_year, ".")))
        top_parents <- s %>% group_by(parent) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>%
          arrange(desc(t)) %>% slice_head(n = top_n) %>% pull(parent)
        # Worldwide, stack each company's bar by country (or sector, via the
        # "Stack bars by" toggle); narrowed to one country, stacking by country
        # would just be a single solid segment, so it's always sector there.
        stack_col <- if (length(leader_country()) == 0) (input$worldwide_stack_by %||% "Country") else "Sector"
        s %>% filter(parent %in% top_parents) %>%
          group_by(parent, .data[[stack_col]]) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
          rename(!!rc := value) -> agg
        # Ascending by total across the shown companies -- the first series added
        # sits at the top of a Highcharts stack, so this puts the biggest
        # country/sector at the bottom of each bar, smallest at top (same
        # convention as the "Revenue by sector, over time" chart).
        stack_totals <- agg %>% group_by(.data[[stack_col]]) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>% arrange(t)
        list(agg = as.data.frame(agg), top_parents = top_parents, stack_col = stack_col,
             stack_values = stack_totals[[stack_col]], stacked = TRUE)
      }
    })

    output$chart_leaders_ui <- renderUI({
      n <- tryCatch(as.numeric(input$top_n), error = function(e) 20)
      highchartOutput(ns("chart_leaders"), height = paste0(max(400, n * 32), "px"))
    })

    output$chart_leaders <- renderHighchart({
      res <- leaders_snap()
      rc <- rev_col()
      parent_order <- rev(res$top_parents)
      hc <- gmicp_stacked_chart(res$agg, "parent", rc, res$stack_col, parent_order, res$stack_values,
                           orientation = "h", stacked = res$stacked) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_tooltip(shared = FALSE, valueDecimals = 1) %>%
        apply_gmicp_theme()
      # Legend order flipped back to biggest-first to match the ascending
      # (smallest-at-top) stack order -- only when stacked by size (Country/
      # Sector); "Compare years" (stacked = FALSE) keeps chronological order.
      if (res$stacked) hc <- hc %>% hc_legend(reversed = TRUE)
      hc
    })

    output$dl_leaders <- downloadHandler(
      filename = function() {
        scope <- if (length(leader_country()) == 0) "worldwide" else gsub(" ", "_", leader_country())
        yr_part <- if (isTRUE(input$compare_years)) paste(input$leader_years, collapse = "-") else input$leader_year
        paste0("gmicp_leading_companies_", scope, "_", yr_part, ".csv")
      },
      content = function(file) write.csv(leaders_snap()$agg, file, row.names = FALSE)
    )

    # ---------------- Company profile ----------------

    # Scoped to the profile card's own country filter (and the global Sector/Years
    # filters), so the search box only offers companies actually present there. Once
    # a company is picked, its own profile below still shows its full worldwide
    # footprint -- narrowing this list is about *finding* the company, not
    # restricting its data. Excludes rows with no revenue figure at all (same
    # reasoning as co_data() below: a company shouldn't be offered here on the
    # strength of a non-revenue row alone, e.g. an audience/ownership record with no
    # $ figure) -- otherwise picking it just lands on "No revenue data for this
    # company." Filtered on the currently selected currency column, matching what
    # co_data() itself requires once a pick is made.
    all_parents <- reactive({
      rc <- rev_col()
      d <- apply_filters(data$unified, profile_country(), sector_sel(), gf$years())
      d <- d[!is.na(d[[rc]]) & !d$is_aggregate, , drop = FALSE]
      p <- d$parent
      sort(unique(p[!is.na(p) & p != "" & p != "nan"]))
    })

    output$profile_title <- renderUI({
      txt <- if (length(profile_country()) == 0) "Company profile" else paste0("Company profile — Firms operating in ", profile_country())
      h3(class = "panel-title", txt)
    })

    output$profile_note <- renderUI({
      if (length(profile_country()) == 0) {
        p(class = "panel-note", "Search for a company to see where it operates and how its revenue has changed over time.")
      } else {
        p(class = "panel-note",
          "Only showing companies with a presence in the country selected above (respecting the Sectors and Years ",
          "filters too). Once selected, the profile below still shows that company's full footprint across every ",
          "country it operates in — pick \"Back to worldwide\" above to search all companies again.")
      }
    })

    # Detects when the selected company's rows span a name change merged by
    # PARENT_NORMALIZE (R/data.R) -- e.g. "Google" -> "Alphabet" -- by comparing the
    # pre-normalization parent_original values against the current, normalized name.
    # Computed from the actual data rather than hardcoded, so it stays correct if the
    # mapping list changes.
    former_names_note <- reactive({
      req(company())
      if (!"parent_original" %in% names(data$unified)) return(NULL)
      u <- data$unified
      d <- u[!is.na(u$parent) & u$parent == company() & !u$is_aggregate, , drop = FALSE]
      former <- unique(d$parent_original[!is.na(d$parent_original) & d$parent_original != company()])
      if (length(former) == 0) return(NULL)
      ranges <- vapply(former, function(nm) {
        yrs <- d$Year[!is.na(d$parent_original) & d$parent_original == nm]
        if (length(yrs) == 0) return(nm)
        paste0(nm, " (", min(yrs), "–", max(yrs), ")")
      }, character(1))
      paste0("Includes data reported under a former name: ", paste(ranges, collapse = ", "), ".")
    })

    output$profile_former_name_note <- renderUI({
      note <- former_names_note()
      if (is.null(note)) return(NULL)
      div(class = "panel-scope-note", note)
    })

    # Rendered exactly once (no reactive reads in the body) and then kept updated via
    # updateSelectizeInput() below -- destroying and recreating the widget itself every
    # time the country filter changes left the underlying <select> with no explicit
    # "nothing selected" option, and the browser was defaulting to auto-selecting the
    # first alphabetical company instead of showing an empty search box.
    output$company_pick_ui <- renderUI({
      selectizeInput(ns("company_pick"), NULL, choices = character(0), selected = character(0), multiple = FALSE,
                      options = list(placeholder = "e.g. Comcast, Alphabet, News Corp..."))
    })

    observeEvent(all_parents(), {
      ph <- if (length(profile_country()) == 0) "e.g. Comcast, Alphabet, News Corp..." else paste0("Companies active in ", profile_country(), "...")
      updateSelectizeInput(session, "company_pick", choices = all_parents(), selected = character(0),
                            server = TRUE, options = list(placeholder = ph))
    }, ignoreNULL = FALSE)

    company <- reactive({
      c <- input$company_pick
      if (is.null(c) || !nzchar(c)) NULL else c
    })

    # Comparing a multi-country company's revenue across countries in local
    # currency mixes units on the same chart -- switch to USD the moment such a
    # company is picked. A one-time nudge, not a lock: the user can still switch
    # back to local currency afterward. Checked against both currency columns
    # (not just the one currently selected) so the decision doesn't depend on
    # whatever currency happened to be active already.
    observeEvent(company(), {
      req(company())
      d <- data$unified
      d <- d[!is.na(d$parent) & d$parent == company() & !d$is_aggregate, , drop = FALSE]
      d <- d[!is.na(d$rev_usd) | !is.na(d$rev_local), , drop = FALSE]
      if (length(unique(d$Country)) > 1) updateRadioButtons(parent_session, "currency", selected = "usd")
    })

    # Filters out rows with no revenue figure at all -- this dashboard is revenue-
    # focused, so a company shouldn't appear "present" in a country (in the KPI
    # stats, the country-comparison chart, or the footprint chart) on the strength of
    # a non-revenue row alone (e.g. an audience/ownership record with no $ figure).
    #
    # Also splices in each PARENT_LINEAGE segment's rows (see data.R -- e.g.
    # Time Warner, and the 2018-2021 AT&T ownership window, both feeding into
    # Warner Bros. Discovery) so a boundary that PARENT_NORMALIZE/the
    # ownership-window override deliberately don't collapse into one `parent`
    # value everywhere still reads as one continuous company across every
    # chart in this profile -- Revenue over time, Rank, and Footprint alike.
    # Every segment's `parent` stays a fully separate, independently
    # searchable/rankable company/attribution everywhere else in the app
    # (its own leaderboard entry, its own search result). Each spliced row is
    # tagged with `lineage_label` (the segment's label) so a chart can say
    # which one a given year/point belongs to.
    co_data <- reactive({
      req(company())
      rc <- rev_col()
      d <- apply_filters(data$unified, character(0), sector_sel(), gf$years())
      d <- d[!is.na(d$parent) & d$parent == company() & !d$is_aggregate, , drop = FALSE]
      d <- d[!is.na(d[[rc]]), , drop = FALSE]
      d$lineage_label <- NA_character_

      segments <- PARENT_LINEAGE[[company()]]
      if (!is.null(segments)) {
        for (seg in segments) {
          pd <- apply_filters(data$unified, character(0), sector_sel(), gf$years())
          pd <- pd[!is.na(pd$parent) & pd$parent == seg$parent & !pd$is_aggregate, , drop = FALSE]
          if (!is.null(seg$flag) && seg$flag %in% names(pd)) pd <- pd[pd[[seg$flag]] & !is.na(pd[[seg$flag]]), , drop = FALSE]
          pd <- pd[!is.na(pd[[rc]]), , drop = FALSE]
          if (nrow(pd) > 0) {
            pd$lineage_label <- seg$label
            d <- bind_rows(d, pd)
          }
        }
      }
      d
    })

    output$profile_lineage_note <- renderUI({
      req(company())
      segments <- PARENT_LINEAGE[[company()]]
      if (is.null(segments)) return(NULL)
      labels <- unique(vapply(segments, function(s) s$label, character(1)))
      div(class = "panel-scope-note",
          paste0("Includes revenue reported under a predecessor entity or prior ownership window (",
                 paste(labels, collapse = ", "), ") for continuity in this long-term trend (years shown under ",
                 "that name are marked in the tooltip). ", paste(labels, collapse = " and "),
                 " remain separate, independently searchable/rankable elsewhere in the app."))
    })

    output$profile_sections <- renderUI({
      if (is.null(company())) {
        return(div(class = "focus-hint", "Nothing selected yet — search above to get started."))
      }
      tagList(
        uiOutput(ns("profile_former_name_note")),
        div(class = "panel-card",
          h4(class = "panel-title", "Revenue over time, by country"),
          p(class = "panel-note", "Where a year is missing, the nearest year within ±2 years is used and marked with an asterisk (*) in the tooltip."),
          uiOutput(ns("profile_lineage_note")),
          highchartOutput(ns("chart_profile_time"), height = "380px"),
          uiOutput(ns("profile_time_fill_caption_ui")),
          downloadButton(ns("dl_profile_time"), "Download chart data (CSV)", class = "dl-btn")
        ),
        div(class = "panel-card",
          uiOutput(ns("profile_rank_title")),
          p(class = "panel-note",
            "Top 20 companies by revenue in the current scope, for the selected year. The searched company is ",
            "highlighted, and included even when it falls outside the top 20."),
          div(class = "panel-toolbar", uiOutput(ns("profile_rank_year_ui"))),
          uiOutput(ns("chart_profile_rank_ui")),
          downloadButton(ns("dl_profile_rank"), "Download chart data (CSV)", class = "dl-btn")
        ),
        div(class = "panel-card",
          h4(class = "panel-title", "Footprint by country and sector"),
          p(class = "panel-note", "Each bar is a country, stacked by sector, for the selected year. ",
            "When one country dominates, the rest can shrink to almost nothing on the shared scale — ",
            "click and drag across the revenue axis to zoom into the low end and see them (and their sector ",
            "breakdown) more clearly; use the \"Reset zoom\" button that appears to zoom back out."),
          div(class = "panel-toolbar", uiOutput(ns("profile_year_ui"))),
          uiOutput(ns("chart_profile_footprint_ui")),
          downloadButton(ns("dl_profile_footprint"), "Download chart data (CSV)", class = "dl-btn")
        )
      )
    })


    profile_time_agg <- reactive({
      req(company())
      rc <- rev_col()
      base <- co_data()
      year_labels <- unique(base[!is.na(base$lineage_label), c("Year", "lineage_label")])
      raw <- base %>% group_by(Year, Country, Sector) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop")
      names(raw)[names(raw) == "value"] <- rc
      # Same two-step trick as build_revenue_totals() (R/agg_helpers.R): fill gaps at
      # the (Country, Sector) level *before* summing to a per-country total. Without
      # this, a year where the company simply didn't report one of its sectors summed
      # to an artificially low total -- indistinguishable from a real revenue drop --
      # and the country-level fill below never caught it, because that low total
      # wasn't NA/missing, just wrong. extrapolate = FALSE throughout, same reason as
      # before: a country with a single real data point (e.g. one isolated year of
      # reported revenue) should show as just that one point, not several
      # manufactured years of "presence" projected around it.
      sector_filled <- fill_pair_gaps(as.data.frame(raw), "Country", "Sector", rc, extrapolate = FALSE)
      country_totals <- sector_filled %>% group_by(Year, Country) %>%
        summarise(value = sum(.data[[rc]], na.rm = TRUE), sector_level_fill = any(filled), .groups = "drop")
      names(country_totals)[names(country_totals) == "value"] <- rc

      res <- fill_year_gaps(as.data.frame(country_totals[, c("Year", "Country", rc)]), "Country", rc, extrapolate = FALSE)
      flags <- country_totals[country_totals$sector_level_fill, c("Year", "Country")]
      flags$sector_level_fill <- TRUE
      out <- merge(res$df, flags, by = c("Year", "Country"), all.x = TRUE)
      out$filled <- out$filled | !is.na(out$sector_level_fill)
      out$sector_level_fill <- NULL
      list(df = out, notes = res$notes, n_sector_level_fills = sum(sector_filled$filled), year_labels = year_labels)
    })

    output$chart_profile_time <- renderHighchart({
      res <- profile_time_agg()
      d <- res$df
      validate(need(nrow(d) > 0, "No data."))
      rc <- rev_col()
      d$note <- fill_tooltip_note(d$filled)
      if (nrow(res$year_labels) > 0) {
        idx <- match(d$Year, res$year_labels$Year)
        has_label <- !is.na(idx)
        d$note[has_label] <- paste0(d$note[has_label], " (as ", res$year_labels$lineage_label[idx[has_label]], ")")
      }
      gmicp_line_chart(d, "Year", rc, "Country", rev_label(),
                        sort_tooltip_desc = TRUE, show_markers = FALSE, note_col = "note")
    })

    output$profile_time_fill_caption_ui <- renderUI({
      cap <- revenue_fill_caption(profile_time_agg())
      if (is.null(cap)) return(NULL)
      div(class = "fill-note", bs_icon("exclamation-circle", size = "0.85rem"), " ", cap)
    })

    output$dl_profile_time <- downloadHandler(
      filename = function() paste0("gmicp_", gsub(" ", "_", company()), "_revenue_by_country.csv"),
      content = function(file) write.csv(profile_time_agg()$df %>% select(-filled, -source_year), file, row.names = FALSE)
    )

    output$profile_rank_title <- renderUI({
      txt <- if (length(profile_country()) == 0) "Rank — worldwide" else paste0("Rank — ", profile_country())
      h4(class = "panel-title", txt)
    })

    output$profile_rank_year_ui <- renderUI({
      req(company())
      yrs <- sort(unique(profile_base_filtered()$Year), decreasing = TRUE)
      req(length(yrs) > 0)
      default <- if (DEFAULT_SNAPSHOT_YEAR %in% yrs) DEFAULT_SNAPSHOT_YEAR else yrs[1]
      selectInput(ns("profile_rank_year"), "Year", choices = yrs, selected = default, width = "140px")
    })

    # Same shape as "Leading companies" (Sectors/Years filters, "Others" and
    # blank parents excluded), but scoped by the profile card's own country
    # filter so a company's rank here always means the same thing as its
    # position would on that leaderboard for the same country. Top 20 by
    # revenue for the selected year, with the searched company appended (if
    # it's not already in the top 20) so it's always visible regardless of
    # how far down it ranks.
    profile_rank_snap <- reactive({
      req(company(), input$profile_rank_year)
      rc <- rev_col()
      s <- profile_base_filtered()[profile_base_filtered()$Year == as.numeric(input$profile_rank_year), , drop = FALSE]
      s <- s[!is.na(s[[rc]]), , drop = FALSE]
      validate(need(nrow(s) > 0, paste0("No company revenue data for ", input$profile_rank_year, ".")))
      totals <- s %>% group_by(parent) %>% summarise(revenue = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(revenue)) %>% as.data.frame()
      totals$rank <- seq_len(nrow(totals))
      # Continuity: a historical year naturally shows this company under its
      # own name at the time (e.g. "Time Warner") -- lineage_names_in() (data.R)
      # recognizes that row as the searched company too, same as co_data() does
      # for the time-series charts. Computed here (against `s`, the row-level
      # data for this year, which still carries any ownership-window flag
      # columns) rather than against `totals`/`top`, since those are already
      # aggregated and lose the flag columns needed to confirm a segment like
      # AT&T's genuinely applies in this specific year.
      highlight_names <- lineage_names_in(s, company())
      totals$is_highlight <- totals$parent %in% highlight_names
      top <- totals[seq_len(min(20, nrow(totals))), ]
      if (!any(top$is_highlight) && any(totals$is_highlight)) {
        top <- bind_rows(top, totals[totals$is_highlight, ])
      }
      names(top)[names(top) == "revenue"] <- rc
      top
    })

    output$chart_profile_rank_ui <- renderUI({
      n <- nrow(profile_rank_snap())
      highchartOutput(ns("chart_profile_rank"), height = paste0(max(300, n * 32 + 40), "px"))
    })

    output$chart_profile_rank <- renderHighchart({
      d <- profile_rank_snap()
      rc <- rev_col()
      # Ascending, not descending -- same horizontal-bar convention as "Leading
      # companies"/"Top companies": the first category sits at the bottom of an
      # "h" bar chart, so ascending order puts the biggest company at the top.
      d <- d[order(d[[rc]]), ]
      is_highlighted <- d$is_highlight
      is_historical_name <- is_highlighted & d$parent != company()
      d$label <- paste0("#", d$rank, " ", d$parent, ifelse(is_historical_name, paste0(" (", company(), ")"), ""))
      cmap <- ifelse(is_highlighted, "#F2618C", "#6C5CE7")
      highchart() %>% hc_chart(type = "bar") %>%
        hc_xAxis(categories = as.list(d$label), title = list(text = "")) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_add_series(name = rev_label(), data = round(d[[rc]], 1), colorByPoint = TRUE, colors = cmap) %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.1f}</b>") %>%
        hc_legend(enabled = FALSE) %>%
        apply_gmicp_theme()
    })

    output$dl_profile_rank <- downloadHandler(
      filename = function() paste0("gmicp_", gsub(" ", "_", company()), "_rank_", input$profile_rank_year, ".csv"),
      content = function(file) write.csv(profile_rank_snap() %>% select(-is_highlight), file, row.names = FALSE)
    )

    output$profile_year_ui <- renderUI({
      yrs <- sort(unique(co_data()$Year), decreasing = TRUE)
      req(length(yrs) > 0)
      default <- if (DEFAULT_SNAPSHOT_YEAR %in% yrs) DEFAULT_SNAPSHOT_YEAR else yrs[1]
      selectInput(ns("profile_year"), "Year", choices = yrs, selected = default, width = "140px")
    })

    # Raw (unfilled) footprint for the selected year -- deliberately not
    # gap-filled, same reasoning as before: this chart is about where the
    # company genuinely reported revenue, not a smoothed trend.
    footprint_data <- reactive({
      req(input$profile_year)
      rc <- rev_col()
      d <- co_data()[co_data()$Year == as.numeric(input$profile_year), , drop = FALSE]
      d %>% group_by(Country, Sector) %>% summarise(value = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>%
        rename(!!rc := value)
    })

    output$chart_profile_footprint_ui <- renderUI({
      n <- length(unique(footprint_data()$Country))
      highchartOutput(ns("chart_profile_footprint"), height = paste0(max(220, n * 42 + 60), "px"))
    })

    output$chart_profile_footprint <- renderHighchart({
      d <- footprint_data()
      validate(need(nrow(d) > 0, paste0("No data for ", company(), " in ", input$profile_year, ".")))
      rc <- rev_col()
      country_order <- d %>% group_by(Country) %>% summarise(t = sum(.data[[rc]], na.rm = TRUE)) %>%
        arrange(t) %>% pull(Country)
      sectors <- sort(unique(d$Sector))
      # Zoomable value axis: with every country shown, one dominant market can
      # dwarf the rest down to invisible slivers -- drag-select along the
      # revenue axis to zoom into the low end instead of hiding countries to
      # see them. Highcharts adds its own "Reset zoom" control automatically,
      # but its default styling (small grey text) is easy to miss -- themed to
      # match the app's accent color and made button-shaped so it reads as an
      # actionable control, not incidental chrome.
      gmicp_stacked_chart(d, "Country", rc, "Sector", country_order, sectors, orientation = "h") %>%
        hc_chart(zoomType = "y", resetZoomButton = list(
          theme = list(fill = "#4f46e5", stroke = "none", r = 6,
                       style = list(color = "#ffffff", fontWeight = "600", fontSize = "0.8rem"),
                       states = list(hover = list(fill = "#3730a3"))),
          position = list(align = "right", verticalAlign = "top", x = -10, y = 10)
        )) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_tooltip(shared = FALSE, valueDecimals = 1) %>%
        apply_gmicp_theme()
    })

    output$dl_profile_footprint <- downloadHandler(
      filename = function() paste0("gmicp_", gsub(" ", "_", company()), "_footprint_", input$profile_year, ".csv"),
      content = function(file) write.csv(footprint_data(), file, row.names = FALSE)
    )

    # Exposed so the sticky header (app.R) can show the current company
    # selection alongside the global Sector/Years filters.
    list(company = company)
  })
}
