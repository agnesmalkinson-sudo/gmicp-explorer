library(shiny)
library(dplyr)
library(highcharter)

mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "panel-card panel-card-wide about-card",
      h3(class = "panel-title", "About the Global Media and Internet Concentration Project"),
      p("GMICP tracks revenue, market concentration, and ownership across media and telecommunications sectors ",
        "in countries around the world. This dashboard is built so scholars, students and researchers, ",
        "journalists, policymakers, media professionals, and the public can explore the underlying data directly ",
        "— every chart can be filtered, and every chart's data can be downloaded as a CSV."),

      h4("Data coverage"),
      p("How many sectors had reported revenue for each country, in each year — a quick way to see how complete ",
        "(or sparse) the underlying data is before relying on a comparison. Darker cells mean more sectors were ",
        "reported that year; pale or blank cells mean little or nothing was reported, so any figures shown there ",
        "for that country/year likely lean on the ±2-year fill described below."),
      uiOutput(ns("coverage_heatmap_ui")),

      h4("Exploring by country"),
      p("The Countries page has two modes, switched at the top of the page:"),
      tags$ul(
        tags$li(tags$strong("Single country"), " — click a country on the map, or use the search box below it, ",
                "to see that country's revenue by sector over time, its top companies, and its market ",
                "concentration (CR4/HHI). Picking a new country replaces the current one."),
        tags$li(tags$strong("Compare countries"), " — click (or search) to add multiple countries instead of ",
                "replacing the selection; a banner at the top confirms you're in compare mode. This shows total ",
                "revenue over time, a snapshot-year breakdown, and concentration, all side by side across the ",
                "countries you've picked.")
      ),
      p("The map itself can be coloured by total revenue, HHI, or CR4 (\"Colour map by\", above the map), for a ",
        "chosen snapshot year — this is just for shading the map and doesn't affect the charts below it."),

      h4("Exploring by company"),
      p("The Companies page has two parts:"),
      tags$ul(
        tags$li(tags$strong("Leading companies"), " — a leaderboard ranked by revenue, worldwide by default. ",
                "Click a country on the map above it (or search) to narrow the leaderboard to companies active ",
                "in just that country instead. \"Show top\" and the year picker control how many companies show ",
                "and for which year; \"Exclude 'Others' aggregate rows\" drops catch-all rows that aren't a real ",
                "single company."),
        tags$li(tags$strong("Company profile"), " — search for a specific company to see its revenue over time ",
                "by country, and its footprint by country and sector for a chosen year. If one country dominates ",
                "the footprint chart's scale, click and drag along the revenue axis to zoom into the low end and ",
                "see the smaller countries more clearly; the \"Reset zoom\" button that appears takes you back out.")
      ),

      h4("Currency"),
      p("Use the USD / Local toggle at the top of the dashboard to switch every revenue chart between US dollars ",
        "and each country's local currency. USD figures are converted in the source workbook at the exchange rate ",
        "for the relevant year; local-currency figures are not inflation-adjusted."),

      h4("Concentration metrics"),
      tags$ul(
        tags$li(tags$strong("CR4"), " — combined market share of the top 4 firms in a market, as a percentage."),
        tags$li(tags$strong("HHI"), " (Herfindahl-Hirschman Index) — sum of squared market shares, ranging from 0 ",
                "(perfectly competitive) to 10,000 (monopoly). Below 1,500 is considered unconcentrated, ",
                "1,500–2,500 moderately concentrated, and above 2,500 highly concentrated.")
      ),

      h4("Source data"),
      p("Data comes from the GMICP unified workbook."),
      p(class = "cite-note", tags$strong("Citing this dashboard: "),
        "Global Media and Internet Concentration Project (GMICP), accessed via the GMICP Explorer dashboard, ", format(Sys.Date(), "%Y"), "."),

      h4("Downloading data"),
      p("Every chart has a \"Download chart data (CSV)\" button underneath it, exporting exactly what that chart ",
        "is currently showing — so it reflects whatever filters, country/company selection, and year are ",
        "currently applied."),

      h4("Missing and estimated data, and how they are handled"),
      p("Not every country reports every sector's revenue, or every concentration metric, in every year. Where a ",
        "data point is missing, this dashboard fills it using the ", tags$strong("nearest available year within ±2 years"),
        " for that same series (country/sector, or company, as relevant) — for example, a missing 2015 value might ",
        "be filled from 2014 or 2016, whichever is closer; ties favour the earlier year."),
      tags$ul(
        tags$li("Filled points are marked with an asterisk (*) in chart tooltips."),
        tags$li("Each affected chart shows a data note below it listing how many points were filled and from which years."),
        tags$li("Downloaded CSVs include a ", tags$code("filled"), " and ", tags$code("source_year"),
                " column so you can identify and, if needed, exclude estimated points."),
        tags$li("Gaps wider than 2 years in either direction are left blank rather than filled.")
      ),

      h4("Company name changes"),
      p("A handful of company names are merged in the underlying data so a rename doesn't look like the company's ",
        "history breaking into two unrelated series — currently ", tags$strong("Google → Alphabet"), " and ",
        tags$strong("Facebook → Meta"), ". This is deliberately narrow: it's only applied where the company is the ",
        "same continuing legal entity under a new name. Corporate splits, spin-offs, and mergers between different ",
        "companies (for example CBS and Viacom operating as separate companies from 2006–2019, or Warner Music ",
        "Group's 2004 spin-off from Time Warner) are left as separate entries."),
      p("The reverse also happens: a handful of parent names in the underlying data are generic collisions between ",
        "unrelated companies that happen to share a name, not one company operating in multiple countries. For ",
        "example, ", tags$strong("\"Power\""), " is split into ", tags$strong("Power Corp"), " in Canada plus ",
        "unrelated firms in Japan and Turkey; ", tags$strong("\"CBS\""), " in South Korea is split out as the ",
        tags$strong("Christian Broadcasting System"), ", a Korean radio broadcaster unrelated to the American CBS/",
        "ViacomCBS network; and ", tags$strong("\"AMC\""), " is split into AMC Networks (the Pay TV channel in ",
        "Latin America) and ", tags$strong("AMC Theatres"), "' European cinema arm (operating as Finnkino in ",
        "Finland and UCI Cinemas in Italy) — two different public companies that happen to share a name. A few ",
        "smaller cases (ABC, KCI, Alpha, AQS) are handled the same way."),

      h4("How this dashboard was built"),
      p("This dashboard is built with ", tags$strong("R"), " and ", tags$strong("Shiny"), ", using ",
        tags$a(href = "https://jkunst.com/highcharter/", target = "_blank", "highcharter"),
        " (an R interface to Highcharts) for the charts and the tidyverse family of packages for reading, cleaning, ",
        "and reshaping the underlying workbook into the country, company, and sector views used throughout. ",
        "Typefaces are Inter and Manrope, both from Google Fonts."),
      p("Data is read directly from the GMICP unified workbook, cleaned and standardised (country and sector ",
        "names, currencies, company name changes), then aggregated on the fly as you filter and navigate. Missing ",
        "values are filled from nearby years as described above, and every chart's underlying data can be ",
        "downloaded as a CSV."),
      p(class = "license-note",
        tags$strong("Highcharts licence: "), "free for personal/non-commercial and nonprofit use (CC BY-NC) and for ",
        "non-funded academic research at an educational institution."),

      h4("Sector and metric definitions"),
      p("What each sector and concentration metric tracked by GMICP covers, and what's deliberately excluded from it."),
      tags$dl(class = "glossary",
        tags$dt("App Distribution"),
        tags$dd(p("Generally refers to Apple's App Store and the Google Play Store but also the app stores of ",
                   "mobile device makers such as Samsung.")),

        tags$dt("Books"),
        tags$dd(p("Includes revenue from all book publishing, including textbooks and e-books. The focus is on ",
                   "publishers' revenue versus those of retail bookstores and/or distributors.")),

        tags$dt("Broadcast Radio"),
        tags$dd(p("AM, FM, digital terrestrial, and satellite audio broadcasting, both stations and networks. In ",
                   "addition to advertising- and publicly-funded broadcast radio, this sector also includes services ",
                   "that rely primarily on paid subscriptions and satellite distribution.")),

        tags$dt("Broadcast TV"),
        tags$dd(p("All “free TV” or public service terrestrial video broadcasting by station and networks ",
                   "supported by advertising and public funds, as well as the retransmission of such channels over ",
                   "cable and satellite.")),

        tags$dt("Concentration Ratio (CR4)"),
        tags$dd(
          p("To determine whether media markets are concentrated or competitive — and the direction of trends ",
            "over time — our research applies two common economic metrics: Concentration Ratios (the CR4) and ",
            "the Herfindahl-Hirschman Index (HHI). These methods are applied to each of the media industries that we ",
            "study and to compare the results across media, time (history) and different countries."),
          p("The CR method adds the shares of each firm in a market and makes judgments based on widely accepted ",
            "standards, with four firms (CR4) having more than 50 percent market share and 8 firms (CR8) more than ",
            "75 percent seen as indicators of media concentration.")
        ),

        tags$dt("Desktop Browsers"),
        tags$dd(p("A desktop browser is application software used to access the World Wide Web.")),

        tags$dt("Desktop OS"),
        tags$dd(p("A desktop operating system manages computer hardware; software resources and provides a common ",
                   "interface that allows people to interact with computers. The two most prominent desktop operating ",
                   "systems at present are Microsoft Windows and Apple's macOS, although there are a variety of open ",
                   "source operating systems built on top of the Linux operating system. Since desktop O/S are ",
                   "bundled with personal computers, market share is usually measured as the share of Microsoft ",
                   "Windows, Apple macOS, and Linux O/S embedded in such computers, not revenue.")),

        tags$dt("Digital Games"),
        tags$dd(
          p("Companies in this sector earn revenue from the sale of physical (boxed) and digital video games, game ",
            "subscriptions (for example, massive multiplayer online games), in-game advertising, and ",
            "microtransactions. Revenue is generally reported across three main platform categories:"),
          tags$ul(
            tags$li(tags$strong("Desktop"), " – Microsoft Windows, Steam, macOS, browsers, and MMOGs"),
            tags$li(tags$strong("Consoles"), " – Sony, Microsoft, Nintendo (including handhelds)"),
            tags$li(tags$strong("Mobile"), " – Mobile app stores or social games delivered via platforms such as Facebook")
          ),
          p("Game titles may be cross-platform (e.g., Fortnite is available on multiple devices). Clear boundaries ",
            "help define this category. Excluded revenue includes e-sports (such as sponsorships), merchandising, ",
            "non-digital games (e.g., board games), game hardware (controllers, consoles, VR headsets), and ",
            "platform-owner subscriptions (e.g., Xbox Live, PlayStation Network). The three major console makers are ",
            "also significant software publishers, and their software sales revenue falls within this category.")
        ),

        tags$dt("Film Exhibition"),
        tags$dd(p("Theatre and box office revenue.")),

        tags$dt("Film Production/Distribution"),
        tags$dd(p("The production, distribution and importing of motion pictures and videos. Third party ",
                   "distributors and disc manufacturers and products produced for TV, such as TV shows and ",
                   "made-for-TV movies, are excluded. Film production firms are notoriously difficult to track ",
                   "because they are often combined with film distribution companies and the revenue for each of ",
                   "those divisions is hard to prise apart. It is also the case that film production companies are ",
                   "often set up and wound down on a project-by-project basis as part of a strategy of managing ",
                   "uncertainty and risk in the film industry. That said, there are some significant film production ",
                   "and distribution companies in major markets for which data can be obtained.")),

        tags$dt("Film TV Online Video Distribution"),
        tags$dd(p("This sector covers revenue derived from the control of distribution rights for all television, ",
                   "film and online video services. In other words, it includes revenue from the exploitation of ",
                   "distribution rights for theatrical and home entertainment services as well as special rights for ",
                   "festivals.")),

        tags$dt("Herfindahl-Hirschman Index (HHI)"),
        tags$dd(
          p("To determine whether media markets are concentrated or competitive — and the direction of trends ",
            "over time — our research applies two common economic metrics: Concentration Ratios (the CR4) and ",
            "the Herfindahl-Hirschman Index (HHI). These methods are applied to each of the media industries that we ",
            "study and to compare the results across media, time (history) and different countries."),
          p("The HHI method is a fine-tuned method that captures subtler changes and differences in media markets. ",
            "It squares the market share of each firm in a given market and then totals them up to arrive at a ",
            "measure of concentration. If there are 100 firms, each with 1% market share, markets are thought to be ",
            "highly competitive (shown by an HHI score of 100), whereas a monopoly prevails when one firm has 100% ",
            "market share (with an HHI score of 10,000)."),
          p("The US Department of Justice embraced a revised set of HHI guidelines in 2010 for categorizing the ",
            "intensity of concentration. The new thresholds are:"),
          tags$ul(
            tags$li("HHI < 1,500 — Unconcentrated"),
            tags$li("HHI > 1,500 but < 2,500 — Moderately Concentrated"),
            tags$li("HHI > 2,500 — Highly Concentrated")
          )
        ),

        tags$dt("Internet Advertising"),
        tags$dd(p("Includes companies whose revenue comes from advertising campaigns and advertising placed on the ",
                   "Internet.")),

        tags$dt("Internet Service Providers (ISP)"),
        tags$dd(p("Internet access services include broadband and dial-up connections delivered via wireline, cable, ",
                   "satellite, or fixed point-to-point wireless networks. This sector generally excludes mobile data ",
                   "or mobile Internet access. In some countries, however, mobile data/Internet may be included; if ",
                   "so, that can be noted for this sector. Where possible, the reported revenue figure typically ",
                   "excludes rental income or fees/taxes, though this depends on local reporting practices.")),

        tags$dt("Magazines"),
        tags$dd(p("Periodicals, mostly consumer oriented, retail magazines rather than professional journals.")),

        tags$dt("Mobile Browsers"),
        tags$dd(p("A mobile browser is a web browser designed for use on mobile phones and other mobile devices and ",
                   "optimized to display Web content effectively for small screens on portable devices. The advent ",
                   "of mobile browsers have helped to bring about the “mobile web” based on mobile versions ",
                   "of websites and pages on the Internet.")),

        tags$dt("Mobile OS"),
        tags$dd(p("A mobile operating system manages computer hardware, software resources and provides a common ",
                   "interface that allows people to interact with mobile phones and other mobile devices. The two ",
                   "most prominent mobile operating systems at present are Apple's iOS and Google's Android. Since ",
                   "mobile O/S are bundled with devices, market share is usually measured as the share of Apple, ",
                   "Google and others' O/S embedded in mobile devices not revenue. There are many, many “white ",
                   "box” devices that run “forked” versions of Android (i.e. that take the core Android ",
                   "Code and modify it to release their own OS) and tracking these is outside of the scope of the ",
                   "project.")),

        tags$dt("Multichannel Video Distribution"),
        tags$dd(
          p("The distribution of linear video programming to end users through cable, DBS/DTH, or IPTV-based ",
            "delivery over fiber. This sector excludes linear streaming services. In other words, it focuses on the ",
            "transmission of programming services rather than the individual channels or broadcasters carried by ",
            "multichannel providers such as Comcast in the United States or Sky in the UK, New Zealand, and ",
            "Australia."),
          p("Revenue for this sector typically combines both the transmission and content/programming components, ",
            "as these are generally bundled and sold as a single package to subscribers. Separate figures are ",
            "sometimes reported solely for “Pay Television Programming Services.” The sector is in flux ",
            "due to cord-cutting and the growth of direct-to-consumer Internet distribution by rights holders and ",
            "programming services.")
        ),

        tags$dt("Music Services"),
        tags$dd(p("The music industry has become immensely more complex over time and now consists of six ",
                   "sub-sectors: 1. Subscription-based streaming services (e.g. Spotify); 2. Download/transactional ",
                   "services (e.g. Apple's iTunes); 3. Direct retail sales/recorded music; 4. Ad-based services (e.g. ",
                   "YouTube); 5. Publishing royalties (e.g. Bertelsmann); 6. Live entertainment (e.g. Live Nation).")),

        tags$dt("Newspapers"),
        tags$dd(p("Daily, community, local and national papers. This sector includes revenues from news stand ",
                   "sales, subscriptions, advertising, patronage and public funds.")),

        tags$dt("Online News"),
        tags$dd(p("Online versions of newspapers, magazines, newsletters, and online providers and compilers of ",
                   "regular news. Does not include online blogs.")),

        tags$dt("Online Video Services"),
        tags$dd(
          p("Firms in this market aggregate and deliver video over the internet. Over time, a range of revenue and ",
            "business models has emerged, with five main sub-categories:"),
          tags$ul(
            tags$li(tags$strong("SVOD (Subscription Video on Demand)"), " – A pure-player service where content ",
                    "is provided without commercials (aside from self-promotion) and accessed through a subscription ",
                    "fee (e.g., Netflix)."),
            tags$li(tags$strong("TVOD (Transactional Video on Demand)"), " – A pure-player service where content ",
                    "is commercial-free and purchased for a one-time fee for unlimited viewing (e.g., Apple iTunes)."),
            tags$li(tags$strong("AVOD (Advertising-Supported Video on Demand)"), " – A pure-player service where ",
                    "content is free but supported by advertising (e.g., YouTube)."),
            tags$li(tags$strong("Linear Streaming Service"), " – A system offering linear programming via ",
                    "subscription, often referred to as streaming pay TV or over-the-top TV (e.g., Sling TV)."),
            tags$li(tags$strong("Hybrid Services"), " – Services that combine two or more of the above models. ",
                    "For example, a platform may provide multiple tiers — such as a free ad-supported tier, a ",
                    "paid tier with limited ads, and a premium ad-free tier — or blend linear streaming with ",
                    "AVOD (e.g., Hulu, Peacock, Hulu + Live TV).")
          ),
          p("Offering on-demand access does not itself make a service hybrid. On-demand components that are part of ",
            "a negotiated rights agreement are considered “authenticated TV,” similar to cable or DBS ",
            "providers that allow streaming access for subscribers (e.g., TLCgo, TBS, HBO Go). This market excludes ",
            "subscription-based pornography sites and companies whose core function is not on-demand video, such as ",
            "Facebook or Twitter.")
        ),

        tags$dt("Others"),
        tags$dd(p("Each sector has an “Others” entry for each relevant year. The aim here is to ensure that ",
                   "every dollar of a topline revenue figure for an industry (such as those from national regulatory ",
                   "and/or statistical agencies, business and trade associations, PwC, European Audiovisual ",
                   "Observatory, IBISWorld, etc.) is accounted for. These entries typically are industry total ",
                   "revenue minus the sum of any individual players with a market share of less than 1%. The ",
                   "purpose is to account for firms active in a market but that have not been accounted for. In ",
                   "other words, the “Others” entry can also be used as a rounding tool or to demonstrate ",
                   "areas of the market which are lacking clarity — a common example of this would be MVNOs in ",
                   "the Wireless sector. Depending on the sector and the size of a nation, an “Others” ",
                   "entry might not be necessary. This can be, for instance, because the data set is complete or the ",
                   "remaining unaccounted for market share so insignificant, that you are confident that each dollar ",
                   "is accounted for among the larger players (such as monopoly sectors).")),

        tags$dt("Pay TV Programming Services"),
        tags$dd(p("Television channels/services not distributed free over-the-air but for a fee over cable, ",
                   "satellite or IPTV platform. This includes standard services delivered over multichannel video ",
                   "distribution services such as the Discovery channel as well as premium pay television services ",
                   "such as HBO.")),

        tags$dt("Revenue"),
        tags$dd(
          p("The GMICP's primary unit of analysis is revenue. This is because it is well-suited to addressing the ",
            "two questions that are driving this effort and for making cross-media, historical and international ",
            "comparisons. As such, the focus of the project is on collecting revenue data wherever possible and ",
            "making best estimates with subscriber, audience and user-based metrics as needed."),
          p("Each team in the project is working towards identifying the relevant firms operating in each sector for ",
            "their country, and, using reliable and publicly available sources (securities/financial reporting is ",
            "prioritized), locate and record revenue and other select information for all firms with a market share ",
            "of 1% or more."),
          p("Authoritative, topline revenue figures for the telecommunications, internet and media sectors that the ",
            "GMICP covers is usually obtained from national regulatory and/or statistical agencies, business and ",
            "trade associations such as the Internet Advertising Bureau, from commercial consultancies such as PwC ",
            "and IBISWorld or local and regional observatories such as the European Audiovisual Observatory and ",
            "Nordicom. Topline revenue data is often more readily available than company-level data.")
        ),

        tags$dt("Search Engines – Mobile"),
        tags$dd(p("Major web-based information search systems accessed through mobile wireless services/smartphones, ",
                   "e.g. Google, Baidu, Bing, Yandex, Naver. The focus is on search engine use on portable devices ",
                   "that have a mobile wireless connection to the Internet (i.e. laptops, phones, including both ",
                   "smart and feature phones since feature phones have the functionality of a smart phone, including ",
                   "internet access, and are hugely popular in South East Asia and Africa, for instance). This does ",
                   "not include devices connected to the Internet by way of a wifi connection.")),

        tags$dt("Search Engines – Desktop"),
        tags$dd(p("Major web-based information search systems available over wireline connections to a personal ",
                   "computer or other devices, e.g. Google, Baidu, Bing, Yandex, Naver. The focus is on search engine ",
                   "use on devices with a wireline connection to the Internet, i.e. a desktop computer, including by ",
                   "way of a wifi connection.")),

        tags$dt("Search Engines"),
        tags$dd(p("Major web-based information search systems regardless of access mode, i.e. mobile and desktop ",
                   "search engines combined, e.g. Google, Baidu, Bing, Yandex, Naver.")),

        tags$dt("Social Media Platforms"),
        tags$dd(p("Platforms whose primary purpose is the sharing of user-generated content and interactions. This ",
                   "can include the sharing of pictures (Instagram), video (YouTube), messages (WeChat), personal ",
                   "information (Facebook) or hybrids of the above (TikTok, Snapchat, etc.). This does not include ",
                   "blogs, as they do not have an interactive function. More generally, examples of social media ",
                   "companies include Facebook's Apps (Facebook, Instagram, WhatsApp), LinkedIn, VKontakte, Twitter, ",
                   "Baidu, YouTube, Vimeo, Reddit, TikTok/Douyin etc. This category does not include SVOD (streaming ",
                   "video on demand) such as Hulu or Netflix as it has no connectivity features or significant ",
                   "amounts of end-user created content.")),

        tags$dt("TV Show Production"),
        tags$dd(p("Companies that produce TV programming licensed or sold to broadcast or cable networks. Movie ",
                   "production is also excluded from this industry, with the exception of made-for-TV movie ",
                   "production. Data for this market is difficult to obtain because there are many smaller ",
                   "television production companies and those companies come and go quickly. That said, there are ",
                   "some significant television production companies in major markets for which data can be ",
                   "obtained.")),

        tags$dt("Wireless"),
        tags$dd(p("Mobile wireless service providers offer both mobile voice and mobile data (i.e., mobile Internet ",
                   "access) services. In some countries it is generally not possible to separate these figures into ",
                   "distinct categories, so they are typically reported as a single wireless revenue figure.")),

        tags$dt("Wireline"),
        tags$dd(
          p("This category was originally intended to capture “voice landline” or “plain old telephone ",
            "service” offered by telephone companies and, more recently, by cable and online providers. It ",
            "excludes mobile wireless telecom, Internet access services, IPTV, equipment rental or sales, and other ",
            "elements sometimes grouped here, such as data centres."),
          p("Over time, the wireline sector has shifted away from its original focus as revenues from traditional ",
            "voice service have declined. Revenue and subscriber data for ISP/Internet access and IPTV (multichannel ",
            "video distribution) services are often reported in the wireline segment; where possible, these are ",
            "typically separated and recorded in the sheets dedicated to those services. The remaining amount is ",
            "generally treated as wireline-sector revenue."),
          p("The category now commonly includes business services, wholesale services, non-broadcasting OTT ",
            "services, and related offerings. Some telecom firms — such as Telus in Canada — also classify ",
            "health, wholesale, or security services as wireline revenue. As these trends continue, the ",
            "“wireline” sector is increasingly regarded as an “other services” category. Equipment ",
            "leasing and sales are usually excluded. Documentation of which services are included can help track how ",
            "the segment evolves over time.")
        )
      )
    )
  )
}

mod_about_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sectors reported per (Country, Year), as a proxy for how complete that
    # cell's data is -- more sectors reported means less reliance on the
    # ±2-year fill for anything summed across sectors for that country/year.
    coverage_agg <- reactive({
      d <- data$revenue
      d <- d[!is.na(d$rev_usd) | !is.na(d$rev_local), , drop = FALSE]
      d %>% group_by(Country, Year) %>% summarise(n_sectors = n_distinct(Sector), .groups = "drop")
    })

    output$coverage_heatmap_ui <- renderUI({
      n <- length(unique(coverage_agg()$Country))
      highchartOutput(ns("coverage_heatmap"), height = paste0(max(400, n * 20 + 90), "px"))
    })

    output$coverage_heatmap <- renderHighchart({
      cov <- coverage_agg()
      validate(need(nrow(cov) > 0, "No data."))
      countries <- sort(unique(cov$Country))
      years <- sort(unique(cov$Year))
      xi <- match(cov$Year, years) - 1
      yi <- match(cov$Country, countries) - 1
      pts <- lapply(seq_len(nrow(cov)), function(i) list(x = xi[i], y = yi[i], value = cov$n_sectors[i]))

      highchart() %>%
        hc_chart(type = "heatmap") %>%
        hc_xAxis(categories = as.list(as.character(years)), title = list(text = ""),
                 labels = list(rotation = -60, style = list(fontSize = "11px"))) %>%
        hc_yAxis(categories = as.list(countries), title = list(text = ""), reversed = TRUE,
                 labels = list(style = list(fontSize = "11px"))) %>%
        hc_colorAxis(min = 0, stops = color_stops(colors = c("#f7f7fb", "#ddd8fb", "#b3a9f4", "#8b7fe8", "#6c5ce7", "#4f3fc9"))) %>%
        hc_add_series(name = "Sectors reported", data = pts, borderWidth = 0.5, borderColor = "#ffffff") %>%
        hc_tooltip(formatter = JS(r"(function() {
          var country = this.series.yAxis.categories[this.point.y];
          var year = this.series.xAxis.categories[this.point.x];
          var n = this.point.value;
          return '<b>' + country + '</b><br/>' + year + ': ' + n + ' sector' + (n === 1 ? '' : 's') + ' reported';
        })")) %>%
        hc_legend(enabled = TRUE, title = list(text = "Sectors reported")) %>%
        apply_gmicp_theme()
    })
  })
}
