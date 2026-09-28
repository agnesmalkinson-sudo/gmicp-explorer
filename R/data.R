library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

DATA_PATH <- file.path("data", "GMICP_unified_workbook.xlsx")
CACHE_DIR <- file.path("data", "cache")

# xlsx parsing (readxl) is the dominant cost of app startup -- far more than
# any of the dplyr processing that follows it -- and grows every time more
# country data gets merged in. Each sheet's raw read_excel() output is cached
# to disk as .rds (much faster to read back than re-parsing xlsx/XML), keyed
# on the source file's mtime so an edited workbook is always detected and the
# cache rebuilt automatically. deploy.R also rebuilds the cache right before
# every deploy, so shinyapps.io ships with (and only ever reads) a fresh one.
read_cached_sheet <- function(sheet, guess_max) {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)
  cache_file <- file.path(CACHE_DIR, paste0(gsub("[^A-Za-z0-9]+", "_", sheet), ".rds"))
  xlsx_mtime <- file.info(DATA_PATH)$mtime
  if (file.exists(cache_file) && file.info(cache_file)$mtime >= xlsx_mtime) {
    return(readRDS(cache_file))
  }
  df <- as.data.frame(read_excel(DATA_PATH, sheet = sheet, guess_max = guess_max))
  saveRDS(df, cache_file)
  df
}

rebuild_cache <- function() {
  unlink(CACHE_DIR, recursive = TRUE)
  dir.create(CACHE_DIR, recursive = TRUE)
  read_cached_sheet("Unified sheet", guess_max = 200000)
  read_cached_sheet("Total Revenue (Millions)", guess_max = 10000)
  read_cached_sheet("Concentration metrics", guess_max = 10000)
  invisible(NULL)
}

SECTOR_NORMALIZE <- c(
  "broadcast television" = "Broadcast TV",
  "broadcast tv" = "Broadcast TV",
  "multichannel video distribution" = "Multichannel Video Distribution (Cable/DBS/IPTV)",
  "multichannel video distribution (cable/dbs/iptv)" = "Multichannel Video Distribution (Cable/DBS/IPTV)",
  # Austria's concentration data doesn't separate MVD from Pay TV Programming
  # the way every other country's does -- counted as MVD per an explicit call,
  # rather than fabricating a split that isn't in the source.
  "multichannel video distribution (cable/dbs/iptv) & pay tv programming" = "Multichannel Video Distribution (Cable/DBS/IPTV)",
  "film production/distribution" = "Film Production/Distribution",
  "film production" = "Film Production/Distribution",
  "film distribution" = "Film Production/Distribution",
  " film production/distribution" = "Film Production/Distribution",
  "film production/distribition" = "Film Production/Distribution", # source typo ("distribition")
  "film tv online video distribution" = "Film & Online Video Distribution",
  "film tv ovs distribution" = "Film & Online Video Distribution",
  "film tv and online video distribution" = "Film & Online Video Distribution", # source variant ("and" instead of "&")
  "film tv online video production" = "Film & Online Video Production",
  "film tv ovs production" = "Film & Online Video Production",
  "film tv and online video production" = "Film & Online Video Production", # source variant ("and" instead of "&")
  "magazines" = "Magazines",
  "magazine" = "Magazines",
  "internet advertising" = "Internet Advertising",
  "online advertising" = "Internet Advertising",
  "search engines" = "Search Engines",
  "search engine" = "Search Engines", # source variant (singular)
  "search engines-desktop" = "Search Engines",
  "search engines-mobile" = "Search Engines",
  "search enginesdesktop" = "Search Engines", # source typo (missing hyphen)
  "search enginesmobile" = "Search Engines", # source typo (missing hyphen)
  "internet search engines" = "Search Engines",
  "music services" = "Music Services",
  "app distribution" = "App Distribution",
  "desktop browsers" = "Desktop Browsers",
  "pay tv programming services" = "Pay TV Programming Services",
  "pay tv services" = "Pay TV Programming Services",
  "pay programming tv services" = "Pay TV Programming Services", # source typo (word order)
  "pay tv programming" = "Pay TV Programming Services", # source variant (missing "Services")
  # Standardized under "Cloud Computing" -- the source data uses "Data Centres"/
  # "Data Centers" and "Cloud Computing" inconsistently for what's the same
  # underlying sector, so both fold into one label instead of splitting a
  # sector's revenue across two entries.
  "data centres" = "Cloud Computing",
  "data centers" = "Cloud Computing",
  "cloud services" = "Cloud Computing", # US source variant
  "social media platforms" = "Social Media Platforms",
  "online video services" = "Online Video Services",
  "video sharing platforms" = "Video Sharing Platforms",
  "wireline" = "Wireline",
  "wireless" = "Wireless",
  "isp" = "ISP",
  "books" = "Books",
  "newspapers" = "Newspapers",
  "broadcast radio" = "Broadcast Radio",
  "digital games" = "Digital Games",
  "film exhibition" = "Film Exhibition",
  "mobile os" = "Mobile OS",
  "desktop os" = "Desktop OS",
  "mobile browsers" = "Mobile Browsers",
  "cloud computing" = "Cloud Computing",
  "content delivery network" = "Content Delivery Network",
  "data brokers" = "Data Brokers",
  "international submarine cables" = "International Submarine Cables",
  "online news media" = "Online News Media",
  "tv show production" = "TV Show Production",
  "digital music" = "Music Services", # Japan source variant
  "pay programing tv services" = "Pay TV Programming Services", # Japan source typo (missing second "m")
  # Singular variant -- critical to map, not just cosmetic: "Social Media
  # Platforms" is in EXCLUDED_SECTORS, and that filter is an exact string
  # match, so an unmapped singular would silently bypass the exclusion.
  "social media platform" = "Social Media Platforms"
)

# Merges parent-company names that are pure renames of the same continuing legal
# entity, so a chart doesn't show a company's history as breaking off into a second,
# unrelated-looking series right at the rename year. Deliberately narrow: this is NOT
# for genuine corporate splits/spin-offs/mergers between different companies (e.g.
# CBS/Viacom's 2006 split and 2019 remerger, or Warner Music Group's 2004 spin-off
# from Time Warner) -- collapsing those would misrepresent the data, not just tidy it.
PARENT_NORMALIZE <- c(
  "Alphabet " = "Alphabet",
  "Facebook" = "Meta",
  "Google" = "Alphabet",
  # Same Finnish regional newspaper publisher (Vasabladet, Österbottens Tidning,
  # Syd-Österbotten) recorded under two capitalizations in different years --
  # not a rename, just inconsistent source data.
  "HSS Media AB" = "HSS Media",
  "HSS Media Ab" = "HSS Media",
  # Modern-era (2017+) spelling/shorthand variants of the current company,
  # post-2022 Discovery merger -- pure labeling inconsistency across
  # countries' source reporting, not a corporate change. Deliberately does
  # NOT include "Warner Music Group"/"Warner Music Australia" (spun off from
  # Time Warner in 2004, a separate public company today, unrelated to WBD),
  # or the two genuine historical joint ventures with other companies
  # ("American TV and Communications & Warner Communications";
  # "Villager Roadshow/ Warner Bros (AUS/US)") -- see PARENT_LINEAGE below for
  # how Time Warner's history still feeds into WBD's long-term revenue trend
  # without being merged into it as the same company everywhere else.
  "Warner" = "Warner Bros. Discovery",
  "Warner Bros" = "Warner Bros. Discovery",
  "Warner Bros Entertainment" = "Warner Bros. Discovery",
  "Warner Brothers Discovery" = "Warner Bros. Discovery",
  # Pre-2018 variants -- same continuing entity as "Time Warner" (AT&T didn't
  # rename it "WarnerMedia" until completing the acquisition in mid-2018, so a
  # "Warner Media" label on 2014-2017 data is that same pre-acquisition
  # company under an inconsistent label, not the later legal entity). "Warner
  # Bros."/"Warner Brothers" alone in 1984-1989 (Italy, Germany) predate even
  # the 1989 Time Inc./Warner Communications merger that created Time Warner,
  # but with only a handful of rows and no separate "Warner Communications"
  # bucket elsewhere in the data, they're folded in here rather than left as
  # their own orphan entries.
  "Time Warner (Warner Bros., US)" = "Time Warner",
  "Warner Media" = "Time Warner",
  "Warner Bros." = "Time Warner",
  "Warner Brothers" = "Time Warner",
  # Disney has no comparable spin-off to guard against (unlike Warner Music) --
  # every variant below is the same continuing entity, just a shorthand or a
  # country-specific label (e.g. "Walt Disney Australia" the same way
  # "Warner Music Australia" is Warner Music Group's Australian arm below).
  "Disney" = "The Walt Disney Company",
  "Walt Disney" = "The Walt Disney Company",
  "Walt Disney (Buena Vista, US)" = "The Walt Disney Company",
  "Walt Disney Australia" = "The Walt Disney Company",
  # Country-specific label for the same global company, same pattern as
  # "Walt Disney Australia" above.
  "Warner Music Australia" = "Warner Music Group",
  # Same global film studio (Comcast/NBCUniversal); NOT "United International
  # Pictures", a genuine historical joint venture between Paramount, Universal
  # and MGM for international distribution -- that stays separate.
  "Universal Pictures Australasia" = "Universal",
  # Same Australian telecom subsidiary under two labeling conventions across
  # non-overlapping periods (1997-2012 vs 2017-2024) -- Optus has been
  # Singtel-owned since 2001, so this is a label change, not an ownership
  # change. Keeps the "Optus" brand name (its more recognizable identity on
  # this dashboard) rather than the parent "Singtel". Doesn't touch the
  # separate, much smaller "Singtel" entry (1996-2008, Multichannel Video
  # Distribution) -- a different sector with no clear evidence it's the same
  # business line as Optus's ISP/Wireless/Wireline operations.
  "Singtel Optus" = "Optus",
  # 2022+ rename after the 2021 corporate restructuring -- same company.
  "Meta Platforms" = "Meta",
  # Below: pure formatting/legal-suffix/capitalization variants of the same
  # continuing company found via a global fuzzy-duplicate scan of every
  # distinct "Parent Ownership Group" value in the workbook (same process
  # used to find the Disney/Warner/Optus variants above). Each one was
  # individually checked against country/sector/year spread before being
  # added here -- see the flagged-but-NOT-merged list in data.R's comments
  # near PARENT_LINEAGE for names that looked similar but turned out to be
  # different real companies.
  "Access Company" = "Access",
  "X" = "X Corp.",
  "X Holdings Corporation" = "X Corp.",
  "Sk" = "SK",
  "Heres" = "HERES",
  # Discovery+ is Discovery Inc.'s own streaming brand, not a different
  # company -- unrelated to whether pre-2022 "Discovery" itself should join
  # Warner Bros. Discovery's PARENT_LINEAGE (it isn't included there; see the
  # note at PARENT_LINEAGE).
  "Discovery+" = "Discovery",
  "NII Holding" = "NII",
  "RSK Holdings" = "RSK",
  # "IAC Inc." (US, Magazines, 2023-2024) and the 2025 row literally
  # annotated by the source data as "(renamed People Inc. in 2025)" are the
  # same continuing entity across that rename; "IAC Publishing" (France,
  # Search Engines, same sector family as plain "IAC" everywhere else) is the
  # same country-specific-label pattern as "Walt Disney Australia" above.
  "IAC Inc." = "IAC",
  "IAC Inc. (renamed People Inc. in 2025)" = "IAC",
  "IAC Publishing" = "IAC",
  "MTN Group" = "MTN"
)

# Fuzzy-duplicate candidates found by the same scan (above) that were
# deliberately NOT merged, because closer inspection (country/sector/year
# spread) showed they're different real companies that happen to share a
# short or generic name -- kept here as a record so a future re-scan doesn't
# re-raise the same false positives:
#   - "Di&Gi" (Italy, Music Services) / "DiGi" (Turkey, cable) / "Digi"
#     (Spain, telecom) -- three unrelated companies.
#   - "AP" (US, Associated Press, Online News Media) / "AP Holding" (France,
#     TV Show Production) -- unrelated despite shared initials.
#   - "ELO" (Italy, Wireless) / "Elo Company" (Brazil, Film Production) --
#     unrelated.
# Two more were left as-is because they need a judgment call rather than a
# clear answer, and are surfaced to the user separately rather than resolved
# here:
#   - "Time" (Canada, Magazines -- plausibly Time Inc., the real magazine
#     publisher) / "Time Inc" (Denmark, Social Media Platforms -- doesn't fit
#     Time Inc.'s actual business, so this may be a different or mislabeled
#     entity).
#   - "Fox" (multiple countries, Broadcast TV/Film/News, some rows continuing
#     past 2019) / "Fox Corporation" (US, 2025) -- ambiguous because of the
#     2019 split between Disney (which took the studio/entertainment assets)
#     and the new Fox Corp (which kept broadcast/news/sports); it's unclear
#     without deeper research whether the post-2019 "Fox"-labeled rows in
#     Belgium/Canada/Mexico are Fox Corp's own business or legacy Fox content
#     that should now be under Disney.

# Companies whose data should read as continuous across a boundary that
# PARENT_NORMALIZE/the ownership-window override below deliberately don't
# collapse into one `parent` value everywhere -- current name -> a list of
# predecessor/ownership segments. Used throughout a company's profile
# (mod_companies.R: Revenue over time, Rank, Footprint) to splice in each
# segment's history, clearly labeled per segment; every `parent` named here
# otherwise stays a fully separate, independently searchable/rankable
# company/attribution everywhere else in the app (its own leaderboard entry,
# its own search result, its own row in a snapshot year it's naturally in).
#
# Warner Bros. Discovery's lineage has two segments: "Time Warner" (the
# pre-2018 independent company, matched on `parent` alone -- see
# PARENT_NORMALIZE) and "AT&T" for 2018-2021 specifically (AT&T owned this
# business, branded WarnerMedia, from its 2018 acquisition close through the
# 2022 Discovery spin-off/merger -- see the ownership-window override below).
# The AT&T segment needs the extra `flag` match so this only pulls the
# Warner-derived slice of AT&T's rows for those years, not AT&T's own native
# telecom business reported under the same `parent` value in the same window.
PARENT_LINEAGE <- list(
  "Warner Bros. Discovery" = list(
    list(parent = "Time Warner", label = "Time Warner"),
    list(parent = "AT&T", label = "AT&T", flag = "is_att_warner_window")
  )
)

# Names that should count as `company` for continuity purposes when checking
# a single point in time (a snapshot-year leaderboard row, a footprint chart)
# -- itself, plus whichever PARENT_LINEAGE segment `parent` names genuinely
# apply WITHIN `df`. A segment's name only counts if its flag (when it has
# one) is actually TRUE for at least one matching row in df -- otherwise a
# shared label like "AT&T" (Warner Bros. Discovery's 2018-2021 ownership
# window, but also AT&T's own unrelated native telecom business every other
# year) would get treated as the searched company's history in years it has
# nothing to do with it. co_data() in mod_companies.R handles the
# full-history equivalent of this for a time series (it needs to actually
# pull and label each segment's rows, not just know their names).
lineage_names_in <- function(df, company) {
  segments <- PARENT_LINEAGE[[company]]
  if (is.null(segments)) return(company)
  applicable <- vapply(segments, function(seg) {
    rows <- !is.na(df$parent) & df$parent == seg$parent
    if (!any(rows)) return(FALSE)
    if (is.null(seg$flag) || !(seg$flag %in% names(df))) return(TRUE)
    any(df[[seg$flag]][rows] & !is.na(df[[seg$flag]][rows]))
  }, logical(1))
  c(company, vapply(segments[applicable], function(s) s$parent, character(1)))
}

# The reverse problem: a handful of parent names in the source data are generic
# collisions between unrelated companies that happen to share a name, rather
# than one company genuinely operating in multiple countries. Left alone, a
# company-profile search would silently merge them into one. Keyed by
# (Country, parent) and applied before parent_original is captured, so the
# former-name disclosure in mod_companies.R doesn't mistake a split for a
# rename. Where the row's own division/brand fields identify the real company,
# that name is used (e.g. Power Corp, KCI S.A.); otherwise a country-qualified
# placeholder distinguishes the entities without asserting a specific identity.
#
# - "Power": Power Corp (Canada) vs. unrelated firms in Japan and Turkey.
# - "CBS" (South Korea): the Christian Broadcasting System, a Korean radio
#   broadcaster -- unrelated to the American CBS/ViacomCBS (US, Canada).
# - "ABC" (Turkey): "ABC Radyo, Televizyon ve Dijital Yayıncılık" per the
#   division field -- unrelated to the American ABC (Disney) network.
# - "AMC": AMC Networks (the Pay TV channel; Argentina/Brazil/Chile) vs. AMC
#   Theatres' European cinema arm, operating as Finnkino (Finland) and UCI
#   Cinemas (Italy) per the division field -- two different public companies
#   that share a name but split apart decades ago.
# - "KCI": KCI S.A., a Polish holding company whose division field shows it
#   owns Gremi Media (publisher of Rzeczpospolita), vs. an unrelated South
#   Korean cable operator recorded under the same "KCI" parent string.
# - "35 mm": a small Czech film distributor vs. an unrelated Turkish magazine
#   imprint (division: Turkuvaz Magazine, brand: Esquire).
# - "Alpha": an Austrian radio brand (division: oe24 Radio) vs. an unrelated
#   small Canadian magazine publisher.
# - "Baidu" (Finland): not a real collision -- a single mislabeled 2020 row.
#   The division/brand fields (HSS Media Ab; Vasabladet, Österbottens Tidning,
#   Syd-Österbotten) match HSS Media's Finland entries in every other year
#   exactly, so this is folded into "HSS Media" instead of split out.
PARENT_DISAMBIGUATE <- data.frame(
  Country = c("Canada", "Japan", "Turkey", "South Korea", "Turkey", "Finland", "Italy",
              "Poland", "South Korea", "Turkey", "Austria", "Canada", "Finland",
              "Czech Republic", "Slovakia"),
  parent = c("Power", "Power", "Power", "CBS", "ABC", "AMC", "AMC",
             "KCI", "KCI", "35 mm", "Alpha", "Alpha", "Baidu",
             "AQS", "AQS"),
  new_parent = c("Power Corp", "Power (Japan)", "Power (Turkey)", "Christian Broadcasting System", "ABC (Turkey)",
                 "AMC Theatres", "AMC Theatres", "KCI S.A.", "KCI (South Korea)", "35 mm (Turkey)",
                 "Alpha (Austria)", "Alpha (Canada)", "HSS Media", "Bioscop", "Magic Box Slovakia"),
  stringsAsFactors = FALSE
)

disambiguate_parent <- function(country, parent) {
  key <- paste(country, parent, sep = "␟")
  dis_key <- paste(PARENT_DISAMBIGUATE$Country, PARENT_DISAMBIGUATE$parent, sep = "␟")
  idx <- match(key, dis_key)
  ifelse(is.na(idx), parent, PARENT_DISAMBIGUATE$new_parent[idx])
}

# Platform/infrastructure sectors excluded project-wide: GMICP's core focus is media
# and telecom revenue concentration, not the tech-platform layer sitting on top of it.
EXCLUDED_SECTORS <- c(
  "Search Engines", "Social Media Platforms", "Desktop Browsers",
  "Mobile Browsers", "Video Sharing Platforms", "Mobile OS", "Desktop OS"
)

drop_excluded_sectors <- function(df) {
  if (!"Sector" %in% names(df)) return(df)
  df[!df$Sector %in% EXCLUDED_SECTORS, , drop = FALSE]
}

# YouTube's ad-supported revenue is recorded inside "Online Video Services" in
# the source data, but it would double-count against the "Internet Advertising"
# sector if left in -- rows are matched by parent/division/brand containing
# "YouTube" (case-insensitive; the source uses several capitalisations, plus at
# least one "YouTube Premuim" typo) so every YouTube-flagged variant is caught.
# hit() must never return NA (a blank field can't contain "youtube", so it's
# FALSE, not "unknown") -- str_detect() on NA returns NA, and `|`-ing that
# across parent/division/brand leaves ovs_yt itself NA for any row with even
# one blank field. Indexing a data frame with a logical vector containing NA
# (df[!ovs_yt, ]) doesn't drop or keep that row -- it silently replaces it
# with an all-NA row, corrupting real data. Discovered via Japan's data, which
# leaves Operating Brand/Title blank for every Online Video Services row.
is_youtube_row <- function(parent, division, brand) {
  hit <- function(x) {
    v <- str_detect(str_to_lower(str_trim(as.character(x))), "youtube")
    ifelse(is.na(v), FALSE, v)
  }
  hit(parent) | hit(division) | hit(brand)
}

# Unified sheet is ~96k rows and gets read twice (once here, once in
# load_unified()) with identical read+normalize steps -- cached so app
# startup only pays for parsing it from disk once.
.unified_raw_cache <- new.env(parent = emptyenv())

read_unified_raw <- function() {
  if (is.null(.unified_raw_cache$data)) {
    df <- read_cached_sheet("Unified sheet", guess_max = 200000)
    df$Country <- normalize_country(as.character(df$Country))
    df$Sector <- normalize_sector(as.character(df$Sector))
    df$Year <- suppressWarnings(as.numeric(df$Year))
    df <- df[!is.na(df$Year), , drop = FALSE]
    df$Year <- as.integer(df$Year)
    .unified_raw_cache$data <- df
  }
  .unified_raw_cache$data
}

# The "Total Revenue" sheet's Online Video Services rows are pre-aggregated at
# country/year level with no company breakdown, so YouTube's share can't be
# read off directly there -- it's computed from the Unified sheet's YouTube-
# flagged rows for the same country/year and netted out in load_revenue().
load_youtube_ovs_totals <- function() {
  df <- read_unified_raw()

  patterns <- list(
    "parent" = function(cl) str_detect(cl, "parent ownership"),
    "division" = function(cl) str_detect(cl, "operating division"),
    "brand" = function(cl) str_detect(cl, "operating brand"),
    "rev_local" = function(cl) str_detect(cl, "total revenue") && str_detect(cl, "local"),
    "rev_usd" = function(cl) str_detect(cl, "total revenue") && str_detect(cl, "us\\$")
  )
  df <- rename_by_map(df, map_columns(names(df), patterns))
  df <- df[str_to_lower(str_trim(df$Sector)) == "online video services", , drop = FALSE]
  df <- df[is_youtube_row(df$parent, df$division, df$brand), , drop = FALSE]
  df$rev_local <- suppressWarnings(as.numeric(df$rev_local))
  df$rev_usd <- suppressWarnings(as.numeric(df$rev_usd))

  df %>%
    group_by(Country, Year) %>%
    summarise(yt_local = sum(rev_local, na.rm = TRUE), yt_usd = sum(rev_usd, na.rm = TRUE), .groups = "drop")
}

normalize_sector <- function(x) {
  stripped <- str_trim(x)
  lowered <- str_to_lower(stripped)
  mapped <- unname(SECTOR_NORMALIZE[lowered])
  ifelse(is.na(mapped), stripped, mapped)
}

normalize_country <- function(x) str_trim(x)

# First-match-wins column detection: workbook column headers vary slightly release
# to release, so columns are identified by pattern rather than exact name.
map_columns <- function(cols, patterns) {
  result <- setNames(rep(NA_character_, length(cols)), cols)
  used <- character(0)
  for (col in cols) {
    cl <- str_to_lower(str_trim(col))
    for (name in names(patterns)) {
      if (name %in% used) next
      if (patterns[[name]](cl)) {
        result[[col]] <- name
        used <- c(used, name)
        break
      }
    }
  }
  result
}

rename_by_map <- function(df, map) {
  keep <- !is.na(map)
  new_names <- names(df)
  for (col in names(map)[keep]) new_names[new_names == col] <- map[[col]]
  names(df) <- new_names
  df
}

load_revenue <- function() {
  df <- read_cached_sheet("Total Revenue (Millions)", guess_max = 10000)

  # Six Argentina rows (2018-2023) under "Music Services Streaming" are exact
  # duplicates of that country/year's regular "Music Services" row -- no other
  # country has this label. Dropped rather than merged, which would double-count
  # Argentina's music revenue for those years relative to every other country.
  df <- df[is.na(df$Sector) | str_to_lower(str_trim(as.character(df$Sector))) != "music services streaming", , drop = FALSE]

  df$Country <- normalize_country(as.character(df$Country))
  df$Sector <- normalize_sector(as.character(df$Sector))
  df$Year <- suppressWarnings(as.numeric(df$Year))
  df <- df[!is.na(df$Year), , drop = FALSE]
  df$Year <- as.integer(df$Year)

  rename_map <- c(
    "Total Revenue (Millions Local$)" = "rev_local",
    "Total Revenue (Millions US$)" = "rev_usd",
    "Subscriber Revenue (Millions Local$)" = "rev_subscriber",
    "Mobile Voice Revenue (Millions Local$)" = "rev_mobile_voice",
    "Mobile Data Revenue (Millions Local$)" = "rev_mobile_data",
    "Ad Revenue (Millions Local$)" = "rev_advertising",
    "Public Funding (Millions Local$)" = "rev_public",
    "Console (Millions Local$)" = "rev_console",
    "PC (Millions Local$)" = "rev_pc",
    "Mobile (Millions Local$)" = "rev_mobile_games",
    "Physical Sales (Millions Local$)" = "rev_physical_sales",
    "Digital Sales/Download (Millions Local$)" = "rev_digital_sales",
    "Recorded Music Revenue (Millions Local$)" = "rev_recorded_music",
    "Live Entertainment Revenue (Millions Local$)" = "rev_live_entertainment",
    "Publishing Royalties (Millions Local$)" = "rev_publishing",
    "Micropayments (Millions Local$)" = "rev_micropayments",
    "Other Revenue (Millions Local$)" = "rev_other"
  )
  rename_map <- rename_map[names(rename_map) %in% names(df)]
  names(df)[match(names(rename_map), names(df))] <- unname(rename_map)

  rev_cols <- names(df)[str_starts(names(df), "rev_")]
  for (col in rev_cols) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))

  df <- df[!(is.na(df$rev_local) & is.na(df$rev_usd)), , drop = FALSE]
  df <- df[df$Year >= 1984, , drop = FALSE]

  # Net YouTube's revenue out of this sheet's pre-aggregated Online Video
  # Services totals -- see load_youtube_ovs_totals() for why this can't be
  # read straight off this (company-detail-free) sheet.
  is_ovs <- str_to_lower(str_trim(df$Sector)) == "online video services"
  if (any(is_ovs)) {
    yt <- load_youtube_ovs_totals()
    df <- merge(df, yt, by = c("Country", "Year"), all.x = TRUE, sort = FALSE)
    df$yt_local[is.na(df$yt_local)] <- 0
    df$yt_usd[is.na(df$yt_usd)] <- 0
    is_ovs <- str_to_lower(str_trim(df$Sector)) == "online video services"
    df$rev_local[is_ovs] <- pmax(df$rev_local[is_ovs] - df$yt_local[is_ovs], 0)
    df$rev_usd[is_ovs] <- pmax(df$rev_usd[is_ovs] - df$yt_usd[is_ovs], 0)
    df$yt_local <- NULL
    df$yt_usd <- NULL
  }

  df <- df %>% arrange(Country, Year, Sector)
  drop_excluded_sectors(df)
}

load_unified <- function() {
  df <- read_unified_raw()

  patterns <- list(
    "parent" = function(cl) str_detect(cl, "parent ownership"),
    "division" = function(cl) str_detect(cl, "operating division"),
    "brand" = function(cl) str_detect(cl, "operating brand"),
    "rev_local" = function(cl) str_detect(cl, "total revenue") && str_detect(cl, "local"),
    "rev_usd" = function(cl) str_detect(cl, "total revenue") && str_detect(cl, "us\\$"),
    "mkt_share_rev" = function(cl) str_detect(cl, "market shares by revenue"),
    "rev_subscriber" = function(cl) str_detect(cl, "subscriber revenue"),
    "rev_advertising" = function(cl) str_detect(cl, "advertising revenue"),
    "rev_public" = function(cl) str_detect(cl, "government") && str_detect(cl, "public"),
    "subscribers" = function(cl) str_detect(cl, "subscribers") && str_detect(cl, "000"),
    "subsector" = function(cl) str_detect(cl, "sub-sector"),
    "ownership_type" = function(cl) cl == "ownership"
  )
  col_map <- map_columns(names(df), patterns)
  df <- rename_by_map(df, col_map)

  if ("parent" %in% names(df)) {
    df$parent <- str_trim(as.character(df$parent))
    df$parent <- disambiguate_parent(df$Country, df$parent)
    df$parent_original <- df$parent
    remap <- unname(PARENT_NORMALIZE[df$parent])
    df$parent <- ifelse(is.na(remap), df$parent, remap)

    # Sector-mislabel correction: WBD doesn't operate a music business --
    # that's exclusively Warner Music Group's, an independent public company
    # since its 2004 spin-off from Time Warner (see PARENT_NORMALIZE above).
    # A handful of countries (Argentina, Brazil, Chile, Germany, India) still
    # report Music Services revenue under a Warner/WBD label rather than
    # Warner Music Group; corrected here rather than left as a data error.
    # Doesn't touch "Time Warner" + Music Services rows (pre-2004, when Time
    # Warner genuinely did own the music business) -- only rows that already
    # normalized to "Warner Bros. Discovery" above. Applied before the AT&T
    # ownership-window override below so a mislabeled Music Services row from
    # 2018-2021 correctly becomes Warner Music Group, not AT&T.
    is_wbd_music_mislabel <- df$parent == "Warner Bros. Discovery" & str_to_lower(str_trim(df$Sector)) == "music services"
    df$parent[is_wbd_music_mislabel] <- "Warner Music Group"
    df$parent_original[is_wbd_music_mislabel] <- "Warner Music Group"

    # Ownership-window override: AT&T owned this business (branded
    # WarnerMedia) from its 2018 acquisition close through the 2022 Discovery
    # spin-off/merger, so for concentration/revenue-attribution purposes --
    # leaderboards, rank, search -- 2018-2021 rolls up to AT&T, not to the
    # standalone Warner Bros. Discovery entity (which only reflects its
    # ownership before and after that window). parent_original is also reset
    # to "AT&T" for these rows so the unrelated "former name" note (driven by
    # parent_original, see former_names_note() in mod_companies.R) doesn't
    # misread this as AT&T itself having once been called "Warner Bros" --
    # is_att_warner_window is the dedicated flag PARENT_LINEAGE uses instead,
    # to splice just this slice back into Warner Bros. Discovery's long-term
    # trend for continuity, without pulling in AT&T's own native business.
    df$is_att_warner_window <- df$parent == "Warner Bros. Discovery" & df$Year >= 2018 & df$Year <= 2021
    df$parent[df$is_att_warner_window] <- "AT&T"
    df$parent_original[df$is_att_warner_window] <- "AT&T"

    df$is_aggregate <- str_to_lower(df$parent) %in% c("others", "other", "nan", "")
  } else {
    df$is_aggregate <- FALSE
  }

  if ("mkt_share_rev" %in% names(df)) {
    df$mkt_share_rev <- suppressWarnings(as.numeric(df$mkt_share_rev))
    df$mkt_share_rev[df$mkt_share_rev > 100] <- NA
  }

  numeric_cols <- c("rev_local", "rev_usd", "rev_subscriber", "rev_advertising", "rev_public", "subscribers")
  for (col in numeric_cols) if (col %in% names(df)) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))

  # Drop YouTube's own company-level rows from Online Video Services -- see
  # is_youtube_row() for why (double-counting with Internet Advertising).
  if (all(c("parent", "division", "brand") %in% names(df))) {
    ovs_yt <- str_to_lower(str_trim(df$Sector)) == "online video services" &
      is_youtube_row(df$parent, df$division, df$brand)
    df <- df[!ovs_yt, , drop = FALSE]
  }

  df <- df[df$Year >= 1984, , drop = FALSE]
  df <- df %>% arrange(Country, Year, Sector)
  drop_excluded_sectors(df)
}

load_concentration <- function() {
  df <- read_cached_sheet("Concentration metrics", guess_max = 10000)

  df$Country <- normalize_country(as.character(df$Country))
  df$Sector <- normalize_sector(as.character(df$Sector))
  df$Year <- suppressWarnings(as.numeric(df$Year))
  df <- df[!is.na(df$Year), , drop = FALSE]
  df$Year <- as.integer(df$Year)

  new_names <- names(df)
  for (i in seq_along(names(df))) {
    cl <- str_to_lower(str_trim(names(df)[i]))
    repl <- switch(cl,
      "cr4" = "cr4", "hhi" = "hhi", "weighted cr4" = "weighted_cr4",
      "weighted hhi" = "weighted_hhi", "cr2" = "cr2", "cr3" = "cr3",
      NULL
    )
    if (!is.null(repl)) new_names[i] <- repl
  }
  names(df) <- new_names

  metric_cols <- c("cr4", "hhi", "weighted_cr4", "weighted_hhi", "cr2", "cr3")
  for (col in metric_cols) if (col %in% names(df)) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))

  if ("cr4" %in% names(df)) {
    df$cr4_suspect <- df$cr4 > 100
    df$cr4 <- pmin(round(df$cr4, 2), 100)
  }

  df <- df[df$Year >= 1984, , drop = FALSE]
  df <- df %>% arrange(Country, Year, Sector)
  drop_excluded_sectors(df)
}

load_all_data <- function() {
  list(
    revenue = load_revenue(),
    unified = load_unified(),
    concentration = load_concentration()
  )
}
