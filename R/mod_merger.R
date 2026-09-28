library(shiny)
library(highcharter)
library(dplyr)
library(bsicons)

mod_merger_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "panel-card panel-card-wide",
      h3(class = "panel-title", "Merger simulator"),
      p(class = "panel-note",
        "Search for two or more companies and pick a year. The tool automatically finds every country/sector ",
        "combination where all of them operate, and shows what market concentration would look like in each one ",
        "if they merged into one. “Before” figures use GMICP's own published Concentration metrics where ",
        "available; where none exist, they're estimated from company revenue instead — each figure below is ",
        "labeled accordingly. This is an illustrative what-if tool, not a prediction or a substitute for real ",
        "antitrust review."),
      div(class = "panel-toolbar",
        div(class = "filter-group", style = "max-width: 480px;",
          tags$label(class = "filter-label", "Companies to merge"),
          uiOutput(ns("company_pick_ui"))
        ),
        uiOutput(ns("year_pick_ui"))
      )
    ),
    uiOutput(ns("results_ui"))
  )
}

mod_merger_server <- function(id, data, all_countries, all_sectors, all_years, gf, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rev_col <- reactive(currency_col(gf$currency()))
    rev_label <- reactive(currency_label(gf$currency()))

    # Global company universe (every real parent, any country/sector/year) --
    # server-side selectize (like the Companies page's own company search)
    # since this list runs well into the thousands; shipping it all to the
    # browser for client-side filtering was a real, measured performance
    # problem there, so this starts server-side from day one.
    all_parent_names <- reactive({
      d <- data$unified
      d <- d[!d$is_aggregate & !is.na(d$parent) & d$parent != "" & d$parent != "nan", , drop = FALSE]
      sort(unique(d$parent))
    })

    # Client-side selectize (not server-side): this picker's server=TRUE
    # setup never took effect across repeated, otherwise-correct attempts on
    # this specific input (server confirmed sending the right choices/url
    # message each time; client-side "load" never got wired up regardless of
    # timing/options tweaks) -- an unresolved quirk specific to this input,
    # not worth blocking on for a lower-traffic tool page. Full ~6k-company
    # list shipped to the browser, same trade-off the Companies page's
    # leaderboard picker had before its server-side fix.
    output$company_pick_ui <- renderUI({
      selectizeInput(ns("companies"), NULL, choices = all_parent_names(), selected = character(0), multiple = TRUE,
                      options = list(placeholder = "Search for two or more companies...", maxItems = 8))
    })

    output$year_pick_ui <- renderUI({
      yrs <- rev(seq(all_years[1], all_years[2]))
      default <- if (DEFAULT_SNAPSHOT_YEAR %in% yrs) DEFAULT_SNAPSHOT_YEAR else yrs[1]
      div(class = "filter-group",
        tags$label(class = "filter-label", "Year"),
        selectInput(ns("year"), NULL, choices = yrs, selected = default, width = "140px")
      )
    })

    # Every (Country, Sector, parent) with real, positive revenue for the
    # selected companies in the selected year -- the raw material for finding
    # where all of them actually operate.
    company_year_data <- reactive({
      req(length(input$companies) >= 2, input$year)
      rc <- rev_col()
      d <- data$unified[data$unified$Year == as.numeric(input$year), , drop = FALSE]
      d <- d[!d$is_aggregate & d$parent %in% input$companies, , drop = FALSE]
      d <- d[!is.na(d[[rc]]) & d[[rc]] > 0, , drop = FALSE]
      d %>% distinct(Country, Sector, parent)
    })

    # (Country, Sector) combinations where EVERY selected company shows up --
    # the actual overlap of their footprints, not just the union.
    overlap_pairs <- reactive({
      cd <- company_year_data()
      req(nrow(cd) > 0)
      counts <- cd %>% count(Country, Sector)
      counts[counts$n == length(input$companies), c("Country", "Sector")]
    })

    # Authoritative market total (from the Total Revenue sheet, not a sum of
    # Unified-sheet company rows -- see the note on the old single-scope
    # version this replaced) plus each company's share, for one country/sector
    # at the selected year. NULL when there's no usable total to divide by.
    share_list_for <- function(cty, sec, year, rc) {
      totd <- apply_filters(data$revenue, cty, sec, c(year, year))
      total <- if (nrow(totd) == 0) NA_real_ else sum(totd[[rc]], na.rm = TRUE)
      if (is.na(total) || total <= 0) return(NULL)
      ud <- apply_filters(data$unified, cty, sec, c(year, year))
      ud <- ud[!ud$is_aggregate & !is.na(ud$parent) & ud$parent != "" & ud$parent != "nan" & !is.na(ud[[rc]]), , drop = FALSE]
      s <- ud %>% group_by(parent) %>% summarise(revenue = sum(.data[[rc]], na.rm = TRUE), .groups = "drop") %>% as.data.frame()
      s$share <- s$revenue / total * 100
      others <- total - sum(s$revenue)
      if (others > 0.01 * total) {
        s <- bind_rows(s, data.frame(parent = "Others (residual)", revenue = others, share = others / total * 100))
      }
      s
    }

    # One before/after computation per overlapping (Country, Sector) pair.
    all_results <- reactive({
      req(length(input$companies) >= 2, input$year)
      pairs <- overlap_pairs()
      validate(need(nrow(pairs) > 0,
                    "No country/sector where all of the selected companies operate in this year."))
      year <- as.numeric(input$year)
      rc <- rev_col()
      # One query covering every overlap pair at once, rather than one per
      # pair -- official_all is then subset in-memory below.
      official_all <- apply_filters(data$concentration, unique(pairs$Country), unique(pairs$Sector), c(year, year))

      res_list <- lapply(seq_len(nrow(pairs)), function(i) {
        cty <- pairs$Country[i]; sec <- pairs$Sector[i]
        shares <- share_list_for(cty, sec, year, rc)
        if (is.null(shares)) return(NULL)
        sel <- shares[shares$parent %in% input$companies, , drop = FALSE]
        if (nrow(sel) < length(input$companies)) return(NULL) # guard; overlap_pairs() should already ensure this
        rest <- shares[!(shares$parent %in% input$companies), , drop = FALSE]

        off_row <- official_all[official_all$Country == cty & official_all$Sector == sec, , drop = FALSE]
        off_hhi <- if (nrow(off_row) > 0 && !is.na(off_row$hhi[1])) off_row$hhi[1] else NA_real_
        off_cr4 <- if (nrow(off_row) > 0 && !is.na(off_row$cr4[1])) off_row$cr4[1] else NA_real_

        if (!is.na(off_hhi)) {
          hhi_before <- off_hhi; hhi_source <- "GMICP published metric"
        } else {
          hhi_before <- sum(shares$share^2, na.rm = TRUE); hhi_source <- "estimated from company revenue"
        }
        # Standard merger-math identity: squaring the combined share of the
        # merging set adds exactly the sum of their pairwise cross-terms to
        # HHI -- a closed-form delta that works on top of either baseline
        # above, and generalizes to any number (2+) of merging companies.
        pair_sum <- if (nrow(sel) >= 2) sum(combn(sel$share, 2, FUN = prod)) else 0
        hhi_after <- hhi_before + 2 * pair_sum

        cr4_before_modeled <- sum(sort(shares$share, decreasing = TRUE)[seq_len(min(4, nrow(shares)))])
        cr4_before <- if (!is.na(off_cr4)) off_cr4 else cr4_before_modeled
        cr4_source <- if (!is.na(off_cr4)) "GMICP published metric" else "estimated from company revenue"

        merged_share <- sum(sel$share)
        after_shares <- c(rest$share, merged_share)
        cr4_after <- sum(sort(after_shares, decreasing = TRUE)[seq_len(min(4, length(after_shares)))])

        list(label = paste0(cty, " — ", sec), country = cty, sector = sec,
             shares = shares, selected = sel, rest = rest, merged_share = merged_share,
             hhi_before = hhi_before, hhi_after = hhi_after, hhi_source = hhi_source,
             cr4_before = cr4_before, cr4_after = cr4_after, cr4_source = cr4_source)
      })
      res_list <- Filter(Negate(is.null), res_list)
      names(res_list) <- vapply(res_list, function(r) r$label, character(1))
      res_list
    })

    result_stat_block <- function(res) {
      hhi_delta <- res$hhi_after - res$hhi_before
      cr4_delta <- res$cr4_after - res$cr4_before
      hhi_before_color <- concentration_band_color(res$hhi_before, "hhi")
      hhi_after_color <- concentration_band_color(res$hhi_after, "hhi")
      cr4_before_color <- concentration_band_color(res$cr4_before, "cr4")
      cr4_after_color <- concentration_band_color(res$cr4_after, "cr4")
      div(class = "merger-result-block",
        h4(class = "panel-subtitle", res$label),
        div(class = "stat-strip",
          stat_card(
            value = tagList(
              span(style = paste0("color:", hhi_before_color), format(round(res$hhi_before), big.mark = ",")),
              " → ",
              span(style = paste0("color:", hhi_after_color), format(round(res$hhi_after), big.mark = ","))
            ),
            label = "HHI (Herfindahl-Hirschman Index)",
            detail = paste0("Δ ", if (hhi_delta >= 0) "+" else "", format(round(hhi_delta), big.mark = ","),
                             " · before figure: ", res$hhi_source),
            icon = "graph-up-arrow"
          ),
          stat_card(
            value = tagList(
              span(style = paste0("color:", cr4_before_color), paste0(round(res$cr4_before, 1), "%")),
              " → ",
              span(style = paste0("color:", cr4_after_color), paste0(round(res$cr4_after, 1), "%"))
            ),
            label = "CR4 (4-firm concentration ratio)",
            detail = paste0("Δ ", if (cr4_delta >= 0) "+" else "", round(cr4_delta, 1),
                             " pts · before figure: ", res$cr4_source),
            icon = "pie-chart"
          )
        )
      )
    }

    output$results_ui <- renderUI({
      if (length(input$companies) < 2) {
        return(div(class = "panel-card panel-card-wide",
          p(class = "panel-note", "Search for two or more companies above to see everywhere they'd overlap, and the before/after for each.")))
      }
      results <- all_results()
      validate(need(length(results) > 0,
                    "No country/sector where all of the selected companies operate in this year."))

      tagList(
        div(class = "panel-card panel-card-wide",
          h3(class = "panel-title", paste0("Before → after — ", length(results), " overlapping market", if (length(results) != 1) "s" else "")),
          p(class = "panel-note", "Every country/sector combination where all of the selected companies had revenue in this year."),
          tagList(lapply(results, result_stat_block)),
          p(class = "panel-note",
            "DOJ/FTC 2010 Horizontal Merger Guidelines bands — HHI: ", paste(concentration_band_ranges("hhi"), collapse = " · "),
            ". This tool's own estimate is independent of GMICP's official Concentration metrics methodology, ",
            "which may apply adjustments not reproducible from company-level revenue alone.")
        ),
        div(class = "panel-card panel-card-wide",
          h3(class = "panel-title", "Company shares"),
          p(class = "panel-note", "Before (left) and after the simulated merger (right), for one overlapping market at a time. The companies being merged are highlighted."),
          div(class = "panel-toolbar", uiOutput(ns("share_focus_ui"))),
          div(class = "merger-share-row",
            div(class = "merger-share-col",
              h4(class = "panel-subtitle", "Before"),
              highchartOutput(ns("chart_merger_before"), height = "340px")
            ),
            div(class = "merger-share-col",
              h4(class = "panel-subtitle", "After"),
              highchartOutput(ns("chart_merger_after"), height = "340px")
            )
          ),
          downloadButton(ns("dl_merger"), "Download before/after shares for every overlapping market (CSV)", class = "dl-btn")
        )
      )
    })

    output$share_focus_ui <- renderUI({
      results <- all_results()
      req(length(results) > 0)
      selectInput(ns("share_focus"), "Market", choices = names(results), selected = names(results)[1], width = "320px")
    })

    output$chart_merger_before <- renderHighchart({
      req(input$share_focus)
      res <- all_results()[[input$share_focus]]
      validate(need(!is.null(res), "No data for this market."))
      s <- res$shares
      s$col <- ifelse(s$parent %in% input$companies, "#F2618C", "#6C5CE7")
      s$rev_fmt <- format(round(s$revenue, 1), big.mark = ",", trim = TRUE)
      hchart(s, "pie", hcaes(name = parent, y = share, color = col, rev_fmt = rev_fmt)) %>%
        hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y:.1f}%"))) %>%
        hc_tooltip(pointFormat = paste0("Share: <b>{point.y:.1f}%</b><br/>Revenue: <b>{point.rev_fmt} ", rev_label(), "</b>")) %>%
        hc_legend(enabled = FALSE) %>%
        apply_gmicp_theme()
    })

    output$chart_merger_after <- renderHighchart({
      req(input$share_focus)
      res <- all_results()[[input$share_focus]]
      validate(need(!is.null(res), "No data for this market."))
      merged_label <- paste(input$companies, collapse = " + ")
      after_df <- bind_rows(
        res$rest[, c("parent", "share", "revenue")],
        data.frame(parent = merged_label, share = res$merged_share, revenue = sum(res$selected$revenue))
      )
      after_df$col <- ifelse(after_df$parent == merged_label, "#F2618C", "#6C5CE7")
      after_df$rev_fmt <- format(round(after_df$revenue, 1), big.mark = ",", trim = TRUE)
      hchart(after_df, "pie", hcaes(name = parent, y = share, color = col, rev_fmt = rev_fmt)) %>%
        hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y:.1f}%"))) %>%
        hc_tooltip(pointFormat = paste0("Share: <b>{point.y:.1f}%</b><br/>Revenue: <b>{point.rev_fmt} ", rev_label(), "</b>")) %>%
        hc_legend(enabled = FALSE) %>%
        apply_gmicp_theme()
    })

    output$dl_merger <- downloadHandler(
      filename = function() paste0("gmicp_merger_simulation_", input$year, ".csv"),
      content = function(file) {
        results <- all_results()
        merged_label <- paste(input$companies, collapse = " + ")
        rows <- lapply(names(results), function(lbl) {
          res <- results[[lbl]]
          before <- res$shares[, c("parent", "share")]
          before$stage <- "before"; before$Country <- res$country; before$Sector <- res$sector
          after <- bind_rows(res$rest[, c("parent", "share")], data.frame(parent = merged_label, share = res$merged_share))
          after$stage <- "after"; after$Country <- res$country; after$Sector <- res$sector
          bind_rows(before, after)
        })
        write.csv(bind_rows(rows), file, row.names = FALSE)
      }
    )
  })
}
