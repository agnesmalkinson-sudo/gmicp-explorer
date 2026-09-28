library(shiny)

# Preset bundles of sectors for the "Sector Groups" filter -- an alternative to
# picking individual sectors, for users who think in terms of a market segment
# rather than GMICP's specific sector taxonomy.
#
# Each group lists its full requested membership as `members` (a data.frame of
# label + whether it maps to an actual filterable sector), and `sectors` is the
# resulting filter list -- just the subset that does. Two reasons a member might
# not:
#
#  - Music Services isn't split into "traditional" vs "streaming" in the source
#    data, so it's included in both Traditional Media and Online & Digital Media
#    (picking either group includes the same combined music revenue).
#  - Search Engines, Social Media & Video Sharing Platforms, Mobile & Desktop
#    Operating Systems, and Mobile & Desktop Browsers are part of a set of
#    platform/infrastructure sectors excluded from the whole dashboard by design
#    (see EXCLUDED_SECTORS in data.R) -- GMICP's focus here is media/telecom
#    revenue, not the tech-platform layer on top of it. They're listed in the
#    info popover for transparency, but selecting "Digital Content Aggregation &
#    Distribution Platforms" only filters by the two members that do exist.
SECTOR_GROUPS <- list(
  list(id = "traditional", label = "Traditional Media Services",
       members = data.frame(stringsAsFactors = FALSE,
         label   = c("Broadcast TV",  "Pay & Specialty TV",          "Radio",           "Traditional Music", "Newspapers", "Magazines"),
         sector  = c("Broadcast TV",  "Pay TV Programming Services", "Broadcast Radio", "Music Services",    "Newspapers", "Magazines"))),
  list(id = "digital_media", label = "Online & Digital Media Services",
       members = data.frame(stringsAsFactors = FALSE,
         label   = c("Streaming Music", "Streaming Video Services",          "Video Games",    "Online News Sources"),
         sector  = c("Music Services",  "Film & Online Video Distribution",  "Digital Games",  "Online News Media"))),
  list(id = "platforms", label = "Digital Content Aggregation & Distribution Platforms",
       members = data.frame(stringsAsFactors = FALSE,
         label   = c("Search Engines", "Social Media & Video Sharing Platforms", "App Distribution", "Internet Advertising",
                      "Mobile & Desktop Operating Systems", "Mobile & Desktop Browsers"),
         sector  = c(NA,               NA,                                       "App Distribution", "Internet Advertising",
                      NA,                                    NA))),
  list(id = "telecom", label = "Telecoms & Internet Infrastructure",
       members = data.frame(stringsAsFactors = FALSE,
         label   = c("Wireline Telecoms", "Mobile Wireless Service", "Internet Service Providers", "Multichannel Video Distribution"),
         sector  = c("Wireline",          "Wireless",                "ISP",                         "Multichannel Video Distribution (Cable/DBS/IPTV)")))
)
for (i in seq_along(SECTOR_GROUPS)) {
  SECTOR_GROUPS[[i]]$sectors <- unique(stats::na.omit(SECTOR_GROUPS[[i]]$members$sector))
}

sector_group_choices <- function() {
  setNames(vapply(SECTOR_GROUPS, function(g) g$id, character(1)),
            vapply(SECTOR_GROUPS, function(g) g$label, character(1)))
}

sector_group_sectors <- function(ids) {
  matched <- Filter(function(g) g$id %in% ids, SECTOR_GROUPS)
  unique(unlist(lapply(matched, function(g) g$sectors)))
}

sector_group_label <- function(id) {
  for (g in SECTOR_GROUPS) if (g$id == id) return(g$label)
  id
}

# Content for the "what's in these groups" info modal. A member shown in
# *italics* isn't tracked in this dashboard, so it's excluded from the actual
# filter even though it's part of the requested group definition.
sector_groups_info_ui <- function() {
  tagList(
    p(class = "panel-note",
      "Each group bundles several sectors into one filter option. A member shown in ", em("italics"),
      " isn't tracked in this dashboard (see the Data & Methods page) and isn't included when you select that ",
      "group -- it's listed here for transparency about the full requested breakdown."),
    p(class = "panel-note",
      "Sector Groups and the plain Sector filter are two ways to express the same thing, so using one disables ",
      "the other -- pick specific sectors, or pick a group, not both at once."),
    tagList(lapply(SECTOR_GROUPS, function(g) {
      tagList(
        h4(g$label),
        tags$ul(
          lapply(seq_len(nrow(g$members)), function(i) {
            lbl <- g$members$label[i]
            if (is.na(g$members$sector[i])) tags$li(em(lbl)) else tags$li(lbl)
          })
        )
      )
    }))
  )
}
