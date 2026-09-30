library(shiny)
library(highcharter)
library(dplyr)
library(bsicons)

MERGER_MAX_COMPANIES <- 8

mod_merger_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "panel-card panel-card-wide",
      h3(class = "panel-title", "Merger simulator"),
      p(class = "panel-note",
        "Pick two or more companies and a year. The tool automatically finds every country/sector combination ",
        "where all of them operate, and shows what market concentration, market shares, and revenue would look ",
        "like if they merged into one. “Before” figures use GMICP's own published Concentration metrics ",
        "where available; where none exist, they're estimated from company revenue instead -- each figure below ",
        "is labeled accordingly. This is an illustrative what-if tool, not a prediction or a substitute for real ",
        "antitrust review."),
      div(class = "panel-toolbar",
        uiOutput(ns("company_1_ui")),
        uiOutput(ns("company_2_ui")),
        uiOutput(ns("extra_company_slots_ui")),
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
    # shipped client-side to every one of the Company N pickers below, same
    # unresolved server-side-selectize quirk noted for the original single
    # picker (confirmed not worth blocking on for this lower-traffic page).
    all_parent_names <- reactive({
      d <- data$unified
      d <- d[!d$is_aggregate & !is.na(d$parent) & d$parent != "" & d$parent != "nan", , drop = FALSE]
      sort(unique(d$parent))
    })

    # A single-select selectizeInput with `choices` supplied directly at
    # render time and `selected = character(0)` silently defaults to the
    # first choice in the underlying <select> instead of showing empty (e.g.
    # Company 1/2 would both start "selected" as whatever company sorts
    # first alphabetically) -- neither an onInitialize JS callback nor a
    # post-render updateSelectizeInput(selected=) call was able to undo that
    # once the widget was created with real choices baked in. What does work
    # (matching the Companies page's own picker): render with *no* choices
    # at all, then populate them via updateSelectizeInput() -- a widget that
    # never had a first real option to begin with has nothing to default to.
    company_selectize <- function(input_id) {
      selectizeInput(ns(input_id), NULL, choices = character(0), selected = character(0),
                      multiple = FALSE, options = list(placeholder = "Search for a company..."))
    }

    output$company_1_ui <- renderUI({
      div(class = "filter-group merger-company-slot",
        tags$label(class = "filter-label", "Company 1"),
        company_selectize("company_1")
      )
    })
    output$company_2_ui <- renderUI({
      div(class = "filter-group merger-company-slot",
        tags$label(class = "filter-label", "Company 2"),
        company_selectize("company_2")
      )
    })
    outputOptions(output, "company_1_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "company_2_ui", suspendWhenHidden = FALSE)

    observeEvent(all_parent_names(), {
      updateSelectizeInput(session, "company_1", choices = all_parent_names(), selected = character(0), server = TRUE)
      updateSelectizeInput(session, "company_2", choices = all_parent_names(), selected = character(0), server = TRUE)
    }, once = TRUE)

    # Company 1 / Company 2 are always present (static, rendered once each
    # above) so adding extra slots below never resets them. Extra slots
    # (Company 3+) live in their own renderUI, which re-renders *all* of them
    # (empty, per company_selectize() above) every time the count changes --
    # existing values are captured just before that happens and restored
    # afterward via the paired observer below, same empty-then-populate
    # pattern as Company 1/2, so growing the list doesn't blank out slots
    # already filled in.
    n_extra <- reactiveVal(0)
    extra_prior_values <- reactiveVal(list())

    observeEvent(input$add_company, {
      k <- n_extra()
      if (2 + k >= MERGER_MAX_COMPANIES) return(invisible(NULL))
      prior <- list()
      if (k > 0) for (i in seq_len(k)) {
        slot_id <- paste0("company_", i + 2)
        v <- input[[slot_id]]
        if (!is.null(v) && nzchar(v)) prior[[slot_id]] <- v
      }
      extra_prior_values(prior)
      n_extra(k + 1)
    })

    output$extra_company_slots_ui <- renderUI({
      k <- n_extra()
      slots <- if (k > 0) lapply(seq_len(k), function(i) {
        slot_id <- paste0("company_", i + 2)
        div(class = "filter-group merger-company-slot",
          tags$label(class = "filter-label", paste0("Company ", i + 2)),
          company_selectize(slot_id)
        )
      }) else NULL
      add_link <- if (2 + k < MERGER_MAX_COMPANIES) {
        div(class = "filter-group merger-add-company",
          actionLink(ns("add_company"), tagList(bs_icon("plus-circle"), " Add another company"))
        )
      } else NULL
      tagList(slots, add_link)
    })

    observeEvent(n_extra(), {
      k <- n_extra()
      req(k > 0)
      prior <- isolate(extra_prior_values())
      for (i in seq_len(k)) {
        slot_id <- paste0("company_", i + 2)
        updateSelectizeInput(session, slot_id, choices = all_parent_names(), selected = prior[[slot_id]] %||% character(0), server = TRUE)
      }
    })

    # Every non-empty Company N input, in order, deduplicated.
    selected_companies <- reactive({
      k <- n_extra()
      ids <- c("company_1", "company_2", if (k > 0) paste0("company_", seq_len(k) + 2) else character(0))
      vals <- unlist(lapply(ids, function(i) input[[i]]))
      vals <- vals[!is.null(vals) & nzchar(vals)]
      unique(vals)
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
      comps <- selected_companies()
      req(length(comps) >= 2, input$year)
      rc <- rev_col()
      d <- data$unified[data$unified$Year == as.numeric(input$year), , drop = FALSE]
      d <- d[!d$is_aggregate & d$parent %in% comps, , drop = FALSE]
      d <- d[!is.na(d[[rc]]) & d[[rc]] > 0, , drop = FALSE]
      d %>% distinct(Country, Sector, parent)
    })

    # (Country, Sector) combinations where EVERY selected company shows up --
    # the actual overlap of their footprints, not just the union.
    overlap_pairs <- reactive({
      comps <- selected_companies()
      cd <- company_year_data()
      req(nrow(cd) > 0)
      counts <- cd %>% count(Country, Sector)
      counts[counts$n == length(comps), c("Country", "Sector")]
    })

    # Authoritative market total (from the Total Revenue sheet, not a sum of
    # Unified-sheet company rows) plus each company's share, for one
    # country/sector at the selected year. NULL when there's no usable total
    # to divide by.
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
      comps <- selected_companies()
      req(length(comps) >= 2, input$year)
      pairs <- overlap_pairs()
      validate(need(nrow(pairs) > 0,
                    "No country/sector where all of the selected companies operate in this year."))
      year <- as.numeric(input$year)
      rc <- rev_col()
      official_all <- apply_filters(data$concentration, unique(pairs$Country), unique(pairs$Sector), c(year, year))

      res_list <- lapply(seq_len(nrow(pairs)), function(i) {
        cty <- pairs$Country[i]; sec <- pairs$Sector[i]
        shares <- share_list_for(cty, sec, year, rc)
        if (is.null(shares)) return(NULL)
        sel <- shares[shares$parent %in% comps, , drop = FALSE]
        if (nrow(sel) < length(comps)) return(NULL) # guard; overlap_pairs() should already ensure this
        rest <- shares[!(shares$parent %in% comps), , drop = FALSE]

        off_row <- official_all[official_all$Country == cty & official_all$Sector == sec, , drop = FALSE]
        off_hhi <- if (nrow(off_row) > 0 && !is.na(off_row$hhi[1])) off_row$hhi[1] else NA_real_
        off_cr4 <- if (nrow(off_row) > 0 && !is.na(off_row$cr4[1])) off_row$cr4[1] else NA_real_

        if (!is.na(off_hhi)) {
          hhi_before <- off_hhi; hhi_source <- "GMICP published metric"
        } else {
          hhi_before <- sum(shares$share^2, na.rm = TRUE); hhi_source <- "estimated from company revenue"
        }
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
             market_total = sum(shares$revenue),
             hhi_before = hhi_before, hhi_after = hhi_after, hhi_source = hhi_source,
             cr4_before = cr4_before, cr4_after = cr4_after, cr4_source = cr4_source)
      })
      res_list <- Filter(Negate(is.null), res_list)
      names(res_list) <- vapply(res_list, function(r) r$label, character(1))
      res_list
    })

    # Country with the largest combined revenue for the selected companies
    # (summed across every overlapping sector within it) -- the default
    # "home turf" view for Company shares, computed rather than assumed
    # since the merging companies may not share a single obvious home market.
    host_country <- reactive({
      results <- all_results()
      req(length(results) > 0)
      tot_by_country <- sapply(split(results, vapply(results, function(r) r$country, character(1))),
                                function(rs) sum(vapply(rs, function(r) sum(r$selected$revenue), numeric(1))))
      names(tot_by_country)[which.max(tot_by_country)]
    })

    # Aggregate shares across *every* overlapping market combined -- not a
    # true global-media-economy share (there's no single coherent "world
    # total" once countries/sectors are mixed), but the combined position of
    # everyone appearing anywhere the selected companies overlap, against the
    # combined total of those same markets. Small long-tail companies beyond
    # the top 8 (the selected companies are always kept individually) are
    # folded into "Others (residual)" for chart readability.
    world_shares <- reactive({
      results <- all_results()
      req(length(results) > 0)
      comps <- selected_companies()
      all_shares <- bind_rows(lapply(results, function(r) r$shares))
      agg <- all_shares %>% filter(parent != "Others (residual)") %>%
        group_by(parent) %>% summarise(revenue = sum(revenue), .groups = "drop")
      residual <- sum(all_shares$revenue[all_shares$parent == "Others (residual)"])
      total <- sum(agg$revenue) + residual
      agg$share <- agg$revenue / total * 100
      agg <- agg %>% arrange(desc(revenue))
      keep <- agg$parent %in% comps | seq_len(nrow(agg)) <= 8
      kept <- agg[keep, , drop = FALSE]
      folded <- sum(agg$revenue[!keep]) + residual
      if (folded > 0) kept <- bind_rows(kept, data.frame(parent = "Others (residual)", revenue = folded, share = folded / total * 100))
      kept
    })

    result_stat_block <- function(res) {
      hhi_delta <- res$hhi_after - res$hhi_before
      cr4_delta <- res$cr4_after - res$cr4_before
      hhi_before_color <- concentration_band_color(res$hhi_before, "hhi")
      hhi_after_color <- concentration_band_color(res$hhi_after, "hhi")
      cr4_before_color <- concentration_band_color(res$cr4_before, "cr4")
      cr4_after_color <- concentration_band_color(res$cr4_after, "cr4")
      div(class = "merger-result-block",
        h4(class = "panel-subtitle", res$sector),
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

    # One card per country, with every affected sector's HHI/CR4 stat blocks
    # nested inside it.
    country_card <- function(country, results_for_country) {
      div(class = "panel-card",
        h4(class = "panel-title", country),
        p(class = "panel-note", length(results_for_country), " affected sector", if (length(results_for_country) != 1) "s" else "", " in ", country, "."),
        tagList(lapply(results_for_country, result_stat_block))
      )
    }

    output$results_ui <- renderUI({
      comps <- selected_companies()
      if (length(comps) < 2) {
        return(div(class = "panel-card panel-card-wide",
          p(class = "panel-note", "Pick two or more companies above to see everywhere they'd overlap, and the before/after for each.")))
      }
      results <- all_results()
      validate(need(length(results) > 0,
                    "No country/sector where all of the selected companies operate in this year."))

      by_country <- split(results, vapply(results, function(r) r$country, character(1)))

      tagList(
        div(class = "panel-card panel-card-wide",
          h3(class = "panel-title", "Company shares"),
          p(class = "panel-note",
            "Before (left) and after the simulated merger (right). Defaults to the host country -- the ",
            "overlapping market where the selected companies' combined revenue is largest -- and the combined ",
            "position across every overlapping market together (“World”); switch to a specific ",
            "country/sector below to see any one market on its own. The companies being merged are highlighted."),
          div(class = "panel-toolbar",
            radioButtons(ns("share_scope"), "View",
                         c("Host country" = "host", "World (combined across overlaps)" = "world", "Specific market" = "specific"),
                         selected = "host", inline = TRUE),
            uiOutput(ns("share_focus_ui"))
          ),
          uiOutput(ns("share_scope_label_ui")),
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
          h4(class = "panel-subtitle", "Revenue, before vs. after"),
          p(class = "panel-note", "Each selected company's own revenue, compared against the combined merged entity's revenue, for the same view selected above."),
          highchartOutput(ns("chart_merger_revenue"), height = "320px"),
          downloadButton(ns("dl_merger"), "Download before/after shares for every overlapping market (CSV)", class = "dl-btn")
        ),
        div(class = "panel-card panel-card-wide",
          h3(class = "panel-title", paste0("Concentration by country — ", length(results), " overlapping market", if (length(results) != 1) "s" else "")),
          p(class = "panel-note", "Every country/sector combination where all of the selected companies had revenue in this year, grouped by country."),
          tagList(lapply(names(by_country), function(cty) country_card(cty, by_country[[cty]]))),
          p(class = "panel-note",
            "DOJ/FTC 2010 Horizontal Merger Guidelines bands — HHI: ", paste(concentration_band_ranges("hhi"), collapse = " · "),
            ". This tool's own estimate is independent of GMICP's official Concentration metrics methodology, ",
            "which may apply adjustments not reproducible from company-level revenue alone.")
        )
      )
    })

    output$share_focus_ui <- renderUI({
      req(input$share_scope == "specific")
      results <- all_results()
      req(length(results) > 0)
      selectInput(ns("share_focus"), NULL, choices = names(results), selected = names(results)[1], width = "320px")
    })

    output$share_scope_label_ui <- renderUI({
      lbl <- switch(input$share_scope %||% "host",
        "host" = paste0("Host country: ", host_country()),
        "world" = "World: combined across every overlapping market",
        "specific" = if (!is.null(input$share_focus)) input$share_focus else ""
      )
      div(class = "panel-scope-note", lbl)
    })

    # Resolves the current scope to a (before-shares-df, after-shares-df,
    # merged-label, market-total) tuple shared by the pie charts and the
    # revenue comparison chart below, so all three always agree.
    current_scope_data <- reactive({
      comps <- selected_companies()
      scope <- input$share_scope %||% "host"
      if (scope == "world") {
        before <- world_shares()
        merged_label <- paste(comps, collapse = " + ")
        rest <- before[!(before$parent %in% comps), , drop = FALSE]
        merged_share <- sum(before$share[before$parent %in% comps])
        merged_revenue <- sum(before$revenue[before$parent %in% comps])
        after <- bind_rows(rest[, c("parent", "share", "revenue")],
                            data.frame(parent = merged_label, share = merged_share, revenue = merged_revenue))
        list(before = before, after = after, merged_label = merged_label)
      } else {
        results <- all_results()
        req(length(results) > 0)
        lbl <- if (scope == "specific") {
          req(input$share_focus); input$share_focus
        } else {
          hc <- host_country()
          # Largest single sector within the host country, for the pies --
          # matches "host country" being about where combined revenue is
          # biggest, not any one sector in particular.
          in_host <- Filter(function(r) r$country == hc, results)
          in_host[[which.max(vapply(in_host, function(r) sum(r$selected$revenue), numeric(1)))]]$label
        }
        res <- results[[lbl]]
        req(!is.null(res))
        merged_label <- paste(comps, collapse = " + ")
        after <- bind_rows(res$rest[, c("parent", "share", "revenue")],
                            data.frame(parent = merged_label, share = res$merged_share, revenue = sum(res$selected$revenue)))
        list(before = res$shares, after = after, merged_label = merged_label)
      }
    })

    pie_chart <- function(df, highlight_label, comps) {
      df$col <- ifelse(df$parent %in% comps | df$parent == highlight_label, "#F2618C", "#6C5CE7")
      df$rev_fmt <- format(round(df$revenue, 1), big.mark = ",", trim = TRUE)
      hchart(df, "pie", hcaes(name = parent, y = share, color = col, rev_fmt = rev_fmt)) %>%
        hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y:.1f}%"))) %>%
        hc_tooltip(pointFormat = paste0("Share: <b>{point.y:.1f}%</b><br/>Revenue: <b>{point.rev_fmt} ", rev_label(), "</b>")) %>%
        hc_legend(enabled = FALSE) %>%
        apply_gmicp_theme()
    }

    output$chart_merger_before <- renderHighchart({
      sc <- current_scope_data()
      validate(need(!is.null(sc), "No data for this view."))
      pie_chart(sc$before, sc$merged_label, selected_companies())
    })

    output$chart_merger_after <- renderHighchart({
      sc <- current_scope_data()
      validate(need(!is.null(sc), "No data for this view."))
      pie_chart(sc$after, sc$merged_label, selected_companies())
    })

    output$chart_merger_revenue <- renderHighchart({
      sc <- current_scope_data()
      validate(need(!is.null(sc), "No data for this view."))
      comps <- selected_companies()
      before_rows <- sc$before[sc$before$parent %in% comps, , drop = FALSE]
      after_row <- sc$after[sc$after$parent == sc$merged_label, , drop = FALSE]
      cats <- c(before_rows$parent, "Combined (after merger)")
      vals <- c(before_rows$revenue, if (nrow(after_row) > 0) after_row$revenue[1] else sum(before_rows$revenue))
      cmap <- c(rep("#6C5CE7", nrow(before_rows)), "#F2618C")
      highchart() %>% hc_chart(type = "column") %>%
        hc_xAxis(categories = as.list(cats), title = list(text = "")) %>%
        hc_yAxis(title = list(text = rev_label()), labels = list(format = "{value:,.0f}")) %>%
        hc_add_series(name = rev_label(), data = round(vals, 1), colorByPoint = TRUE, colors = cmap) %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.1f}</b>") %>%
        hc_legend(enabled = FALSE) %>%
        apply_gmicp_theme()
    })

    output$dl_merger <- downloadHandler(
      filename = function() paste0("gmicp_merger_simulation_", input$year, ".csv"),
      content = function(file) {
        results <- all_results()
        comps <- selected_companies()
        merged_label <- paste(comps, collapse = " + ")
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
