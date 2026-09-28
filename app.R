library(shiny)
library(bsicons)
library(highcharter)

source("R/data.R")
source("R/gaps.R")
source("R/charts.R")
source("R/filters.R")
source("R/geo.R")
source("R/agg_helpers.R")
source("R/sector_groups.R")
source("R/mod_countries.R")
source("R/mod_companies.R")
source("R/mod_merger.R")
source("R/mod_about.R")

# Loaded once per R process (not per browser session) -- every visitor shares the
# same in-memory copy instead of each one re-parsing the 9MB workbook on connect.
APP_DATA <- load_all_data()
APP_ALL_COUNTRIES <- sort(unique(c(APP_DATA$revenue$Country, APP_DATA$unified$Country, APP_DATA$concentration$Country)))
APP_ALL_SECTORS <- sort(unique(c(APP_DATA$revenue$Sector, APP_DATA$unified$Sector, APP_DATA$concentration$Sector)))
APP_ALL_YEARS <- range(c(APP_DATA$revenue$Year, APP_DATA$unified$Year, APP_DATA$concentration$Year), na.rm = TRUE)

# Cache-busts styles.css on every R process restart, so browsers that already cached
# an older copy (common during development) pick up CSS changes without a hard refresh.
APP_ASSET_VERSION <- as.integer(Sys.time())

# Flipped to FALSE for the production deploy only (see deploy.R, which
# temporarily patches this line before deploying and restores it after, for
# the shinyapps.io deploy path). On Connect Cloud, master/production and the
# staging branch each carry their own value directly instead -- FALSE here on
# master, TRUE on the staging branch -- since that deploy is git-branch-driven
# rather than script-driven.
SHOW_MERGER_SIMULATOR <- TRUE

NAV_ITEMS <- Filter(Negate(is.null), list(
  list(id = "countries", label = "Countries", icon = "flag"),
  list(id = "companies", label = "Companies", icon = "building"),
  if (SHOW_MERGER_SIMULATOR) list(id = "merger", label = "Merger Simulator", icon = "shuffle"),
  list(id = "about", label = "Data & Methods", icon = "info-circle")
))

top_nav_links_ui <- function(active) {
  tags$nav(class = "top-nav-links",
    lapply(NAV_ITEMS, function(item) {
      actionLink(
        paste0("nav_", item$id),
        class = paste("top-nav-link", if (identical(active, item$id)) "active" else ""),
        label = tagList(bs_icon(item$icon, size = "0.9rem"), span(item$label))
      )
    })
  )
}

ui <- bootstrapPage(
  tags$head(
    tags$title("GMICP Explorer — Global Media & Internet Concentration Project"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Manrope:wght@500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?v=", APP_ASSET_VERSION)),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Reports this page's full height to whatever page embeds it in an iframe
    # (postMessage, since a cross-origin iframe can't be measured directly by
    # its parent) -- so the embedding page can size the iframe to fit exactly,
    # no inner scrollbar, no guessed fixed height. Re-reports on any height
    # change: tab switches (Countries/Companies/Data & Methods differ hugely),
    # the filter panel opening/closing, chart/caption content changing, etc.
    # Harmless if the page isn't embedded -- nothing is listening, so this is
    # a no-op message into the void.
    tags$script(HTML(r"(
      (function() {
        function postHeight() {
          window.parent.postMessage(
            { type: 'gmicp-explorer-resize', height: document.documentElement.scrollHeight },
            '*'
          );
        }
        function setup() {
          postHeight();
          if (typeof ResizeObserver !== 'undefined') new ResizeObserver(postHeight).observe(document.body);
          window.addEventListener('load', postHeight);
        }
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setup);
        else setup();
      })();
    )")),
    # .compare-banner is position:fixed at a hardcoded offset below the header
    # (see styles.css) -- this keeps that offset correct when the filter-crumbs
    # strip is also showing, by feeding its live height in as a CSS var, rather
    # than hardcoding a second fixed value that would only be right sometimes.
    tags$script(HTML(r"(
      (function() {
        function setup() {
          var el = document.getElementById('active_filters_summary');
          if (!el || typeof ResizeObserver === 'undefined') return;
          new ResizeObserver(function(entries) {
            document.documentElement.style.setProperty('--crumbs-offset', entries[0].contentRect.height + 'px');
          }).observe(el);
        }
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setup);
        else setup();
      })();
    )")),
    # Sector filter and Sector Groups filter are mutually exclusive -- picking
    # one disables the other (see server), enforced client-side via selectize's
    # own disable()/enable() rather than Shiny's updateSelectizeInput(), which
    # has no "disabled" option.
    tags$script(HTML(r"(
      Shiny.addCustomMessageHandler('setSelectizeDisabled', function(msg) {
        var el = document.getElementById(msg.id);
        if (!el || !el.selectize) return;
        if (msg.disabled) el.selectize.disable(); else el.selectize.enable();
      });
    )"))
  ),
  div(class = "app-shell",
    tags$header(class = "top-nav",
      div(class = "top-nav-inner",
        uiOutput("top_nav_links"),
        div(class = "top-nav-actions",
          div(class = "currency-toggle",
            radioButtons("currency", NULL, c("USD" = "usd", "Local currency" = "local"),
                         selected = "usd", inline = TRUE)
          ),
          # Plain client-side visibility toggle, deliberately not a Shiny
          # actionLink/observeEvent -- it only needs to show/hide the filter
          # strip, and the inputs inside keep their live bindings and values
          # no matter whether the strip is visible, so there's nothing for the
          # server to do here.
          tags$button(
            type = "button", id = "filters_toggle_btn", class = "filters-toggle",
            onclick = "document.getElementById('filterBarPanel').classList.toggle('open'); this.classList.toggle('active');",
            bs_icon("sliders", size = "0.8rem"), " Filters"
          )
        )
      )
    ),
    # Full-width crumb strip under the header, visible even while the filter
    # panel itself is collapsed -- so a filtered view doesn't silently look
    # like the default one from the sticky header alone. Renders nothing (no
    # strip at all) when no filters/selection are active -- see server.
    uiOutput("active_filters_summary"),
    div(id = "filterBarPanel", class = "filter-bar",
      div(class = "filter-group",
        div(class = "filter-label-row",
          tags$label(class = "filter-label", "Sector"),
          actionLink("select_all_sectors", "Select all", class = "filter-select-all"),
          actionLink("deselect_all_sectors", "Deselect all", class = "filter-select-all")
        ),
        uiOutput("sector_filter_ui")
      ),
      div(class = "filter-group",
        div(class = "filter-label-row",
          tags$label(class = "filter-label", "Sector groups"),
          tags$button(
            type = "button", id = "sector_groups_help_btn", class = "help-btn action-button help-btn-inline",
            title = "What's in these sector groups?", `aria-label` = "What's in these sector groups?",
            bs_icon("info-circle", size = "0.85rem")
          )
        ),
        uiOutput("sector_groups_filter_ui")
      ),
      div(class = "filter-group filter-year",
        tags$label(class = "filter-label", "Years"),
        uiOutput("year_filter_ui")
      ),
      div(class = "filter-group filter-reset",
        actionLink("reset_filters", label = tagList(bs_icon("arrow-counterclockwise"), " Reset"))
      ),
      tags$button(
        type = "button", id = "filter_bar_hide_btn", class = "filter-bar-hide",
        onclick = "document.getElementById('filterBarPanel').classList.remove('open'); document.getElementById('filters_toggle_btn').classList.remove('active');",
        title = "Hide filters", `aria-label` = "Hide filters",
        bs_icon("x-lg", size = "0.8rem")
      )
    ),
    div(class = "main-col",
      div(class = "content-area",
        # A hidden tabsetPanel's initial active pane follows DOM order (its
        # client-side init doesn't reliably respect `selected` once reordered),
        # so keep "countries" first here to match NAV_ITEMS and keep it the
        # landing page for anyone opening the link, including repeat visitors.
        tabsetPanel(id = "page", type = "hidden", selected = "countries",
          tabPanelBody("countries", mod_countries_ui("countries")),
          tabPanelBody("companies", mod_companies_ui("companies")),
          if (SHOW_MERGER_SIMULATOR) tabPanelBody("merger", mod_merger_ui("merger")),
          tabPanelBody("about", mod_about_ui("about"))
        )
      )
    ),
    tags$footer(class = "app-footer",
      "Data from the Global Media and Internet Concentration Project. See Data & Methods for sourcing and licensing.")
  )
)

server <- function(input, output, session) {
  data <- APP_DATA
  all_countries <- APP_ALL_COUNTRIES
  all_sectors <- APP_ALL_SECTORS
  all_years <- APP_ALL_YEARS

  output$sector_filter_ui <- renderUI({
    # Starts with every sector selected (rather than empty + a placeholder) so
    # it's visible at a glance which sectors are in scope -- functionally
    # identical to no filter either way (see resolved_sectors() and
    # is_all_sectors() below, which treat "all selected" and "none selected"
    # the same).
    selectizeInput("sector", NULL, choices = all_sectors, selected = all_sectors, multiple = TRUE,
                    options = list(plugins = list("remove_button"), placeholder = "All sectors"), width = "280px")
  })
  output$sector_groups_filter_ui <- renderUI({
    selectizeInput("sector_groups", NULL, choices = sector_group_choices(), selected = character(0), multiple = TRUE,
                    options = list(plugins = list("remove_button"), placeholder = "None"), width = "280px")
  })
  # Two plain dropdowns instead of a range slider -- much easier to set
  # precisely (click a year, done) than dragging two small slider handles,
  # especially on a trackpad. `year_to_ui`'s own choices are capped to
  # `input$year_from` (and vice versa isn't needed, see the observer below)
  # so the pair can't be dragged past each other into an invalid range.
  output$year_filter_ui <- renderUI({
    tagList(
      div(class = "year-range-row",
        selectInput("year_from", NULL, choices = seq(all_years[1], all_years[2]), selected = all_years[1], width = "100px"),
        span(class = "year-range-sep", "to"),
        uiOutput("year_to_ui", inline = TRUE)
      )
    )
  })
  output$year_to_ui <- renderUI({
    from <- suppressWarnings(as.numeric(input$year_from))
    if (length(from) == 0 || is.na(from)) from <- all_years[1]
    # isolate()d so this doesn't reactively depend on the very input it
    # defines. NULL/NA means true first render (no prior "To" yet) -- default
    # to the full range end rather than collapsing to "from".
    current_to <- isolate(suppressWarnings(as.numeric(input$year_to)))
    default_to <- if (length(current_to) == 0 || is.na(current_to)) all_years[2] else max(from, current_to)
    selectInput("year_to", NULL, choices = seq(from, all_years[2]), selected = default_to, width = "100px")
  })
  # By default Shiny defers (never even computes) an output while its container
  # is display:none -- and the filter strip starts collapsed. Without this,
  # input$sector/input$year_from/input$year_to stay NULL until a visitor opens
  # the Filters panel at least once, and every chart in the app filters by
  # year range, so the whole site would look like it has no data until then.
  outputOptions(output, "sector_filter_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sector_groups_filter_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "year_filter_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "year_to_ui", suspendWhenHidden = FALSE)

  # The Sector filter defaults to every sector selected, not empty -- treated
  # identically to "nothing selected" everywhere a "sector filter active?"
  # check is made, since both mean "no filter" in practice.
  is_all_sectors <- function(sel) length(sel) == 0 || setequal(sel, all_sectors)

  # Sector and Sector Groups are two ways to express the same filter -- picking
  # one disables the other client-side (see the setSelectizeDisabled JS handler)
  # so it's never ambiguous which is actually in effect. Sector counts as
  # "picked" only once it's a real (non-default) filter -- otherwise, since it
  # defaults to every sector selected, Sector Groups would be disabled from
  # the moment the page loads.
  observeEvent(input$sector, {
    session$sendCustomMessage("setSelectizeDisabled", list(id = "sector_groups", disabled = !is_all_sectors(input$sector)))
  }, ignoreNULL = FALSE)
  observeEvent(input$sector_groups, {
    session$sendCustomMessage("setSelectizeDisabled", list(id = "sector", disabled = length(input$sector_groups) > 0))
  }, ignoreNULL = FALSE)

  observeEvent(input$sector_groups_help_btn, {
    showModal(modalDialog(
      title = "What's in these sector groups?",
      sector_groups_info_ui(),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "sector", selected = all_sectors)
    updateSelectizeInput(session, "sector_groups", selected = character(0))
    updateSelectInput(session, "year_from", selected = all_years[1])
    updateSelectInput(session, "year_to", selected = all_years[2])
  })
  observeEvent(input$select_all_sectors, updateSelectizeInput(session, "sector", selected = all_sectors))
  observeEvent(input$deselect_all_sectors, updateSelectizeInput(session, "sector", selected = character(0)))

  # Sector and Sector Groups resolve to the same underlying filter -- whichever
  # one is populated (they're mutually exclusive, see above) wins; groups expand
  # to their member sectors so every consumer downstream still just filters by
  # plain sector names, with no group-awareness needed anywhere else in the app.
  resolved_sectors <- reactive({
    if (length(input$sector_groups) > 0) sector_group_sectors(input$sector_groups)
    else if (is_all_sectors(input$sector)) character(0)
    else input$sector
  })
  resolved_years <- reactive({
    from <- suppressWarnings(as.numeric(input$year_from))
    to <- suppressWarnings(as.numeric(input$year_to))
    req(!is.na(from), !is.na(to))
    c(from, to)
  })

  global_filters <- list(
    sectors = resolved_sectors,
    years = resolved_years,
    currency = reactive(input$currency)
  )

  # Nav: a hidden tabsetPanel switches page content; the link row itself is
  # re-rendered on each click so the "active" state can be re-derived from
  # input$page instead of hand-managed with JS.
  output$top_nav_links <- renderUI(top_nav_links_ui(input$page %||% "countries"))
  lapply(NAV_ITEMS, function(item) {
    observeEvent(input[[paste0("nav_", item$id)]], updateTabsetPanel(session, "page", selected = item$id))
  })

  countries_ret <- mod_countries_server("countries", data, all_countries, all_sectors, all_years, global_filters, session)
  companies_ret <- mod_companies_server("companies", data, all_countries, all_sectors, all_years, global_filters, session)
  if (SHOW_MERGER_SIMULATOR) mod_merger_server("merger", data, all_countries, all_sectors, all_years, global_filters, session)
  mod_about_server("about", data)

  # Crumb strip under the header: global Sector/Years filters, plus whichever
  # page-specific selection is relevant to the page currently showing (country
  # selection on Countries, company on Companies -- meaningless elsewhere, so
  # left out on other pages). Returns NULL (no strip at all, not just an empty
  # one) when nothing is active, so the default view stays clean.
  output$active_filters_summary <- renderUI({
    country_part <- NULL
    page <- input$page %||% "countries"
    # Compare mode's country list can run long (up to "every country"), and
    # doesn't fit the same "current single focus" framing as single-country
    # mode or a Company Profile pick -- left out here, still shown in the
    # compare-mode banner and country picker themselves.
    if (page == "countries" && !countries_ret$is_compare()) {
      sel <- countries_ret$selection()
      if (length(sel) == 1) country_part <- sel
    } else if (page == "companies") {
      comp <- companies_ret$company()
      if (!is.null(comp)) country_part <- comp
    }

    sector_groups_active <- length(input$sector_groups) > 0
    sector_active <- !is_all_sectors(input$sector)
    # Always a real value (never omitted) -- "All Sectors" reads as an explicit
    # statement of current scope rather than requiring the absence of a sector
    # crumb to be interpreted as "every sector." Only actually shown, though,
    # when the strip is already appearing for some other active filter below.
    sector_part <- if (sector_groups_active) {
      paste(vapply(input$sector_groups, sector_group_label, character(1)), collapse = ", ")
    } else if (sector_active) {
      # Full list, not a truncated "N sectors" count -- the crumb strip itself
      # is expandable/collapsible (see .filter-crumbs-toggle) for when this
      # runs long, rather than hiding the detail behind just a count.
      paste(input$sector, collapse = ", ")
    } else {
      "All Sectors"
    }

    # Read year_from/year_to directly (not via resolved_years(), which req()s
    # them and would silently blank this whole crumb strip -- country/sector
    # parts included -- during the brief window before they're set).
    yr_from <- suppressWarnings(as.numeric(input$year_from))
    yr_to <- suppressWarnings(as.numeric(input$year_to))
    years_active <- length(yr_from) == 1 && length(yr_to) == 1 && !is.na(yr_from) && !is.na(yr_to) &&
      !(yr_from == all_years[1] && yr_to == all_years[2])
    years_part <- if (years_active) paste0(yr_from, "–", yr_to) else NULL

    # Same "always a real value, only shown once the strip is already up for
    # some other reason" treatment as sector_part's "All Sectors" -- Local
    # currency alone is enough to surface the strip (like a narrowed sector or
    # year range), USD is the default and only appears as confirming text.
    currency_active <- identical(input$currency, "local")
    currency_part <- if (currency_active) "Local currency" else "USD"

    # Nothing to report -- default sectors, full year range, USD, no
    # country/company focus -- so no strip at all (not just an empty one).
    if (is.null(country_part) && !sector_groups_active && !sector_active && !years_active && !currency_active) return(NULL)

    parts <- c(country_part, sector_part, years_part, currency_part)
    div(class = "filter-crumbs-bar",
      div(class = "filter-crumbs-inner",
        bs_icon("funnel-fill", size = "0.65rem"),
        span(class = "filter-crumbs-text", paste(parts, collapse = " · ")),
        # Plain client-side toggle (like the Filters/filter-bar-hide buttons) --
        # collapsed by default (single line, ellipsis-truncated via CSS),
        # expands to wrap and show the full text in place.
        tags$button(
          type = "button", class = "filter-crumbs-toggle",
          onclick = "this.closest('.filter-crumbs-bar').classList.toggle('expanded'); this.classList.toggle('expanded');",
          title = "Show/hide full filter list", `aria-label` = "Show/hide full filter list",
          bs_icon("chevron-down", size = "0.6rem")
        )
      )
    )
  })
}

shinyApp(ui, server)
