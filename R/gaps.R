library(dplyr)

# GMICP's public-facing rule for missing data: a gap in a (group, Year) series is
# filled from the nearest available year within +/- max_gap years of that group
# (ties broken toward the earlier year). Every filled point is flagged so charts and
# captions can disclose exactly which values were estimated and from which year.

#
# `extrapolate` controls whether a gap can be filled *beyond* a group's own earliest
# or latest real data point. With the default (TRUE), a group with a single real
# point at year Y also gets Y-2..Y+2 filled from it -- reasonable for an ongoing
# series with a sparse gap, but wrong for a one-off data point (e.g. a company that
# reported revenue in a country for exactly one year): it would manufacture several
# years of "presence" that never happened. Passing FALSE restricts filling to
# interpolation strictly between a group's own min and max real year.
fill_year_gaps <- function(agg, group_col, value_col, max_gap = 2, extrapolate = TRUE) {
  years_present <- sort(unique(agg$Year))
  groups <- split(agg, agg[[group_col]])
  filled_rows <- vector("list", 0)
  fill_notes <- vector("list", 0)

  for (group_val in names(groups)) {
    grp <- groups[[group_val]]
    valid <- grp[!is.na(grp[[value_col]]), , drop = FALSE]
    group_map <- setNames(valid[[value_col]], as.character(valid$Year))
    avail <- as.numeric(names(group_map))

    for (year in years_present) {
      yr_chr <- as.character(year)
      if (yr_chr %in% names(group_map)) {
        filled_rows[[length(filled_rows) + 1]] <- data.frame(
          Year = year, group = group_val, value = as.numeric(group_map[[yr_chr]]),
          filled = FALSE, source_year = NA_real_, stringsAsFactors = FALSE
        )
      } else if (length(avail) > 0 && (extrapolate || (year >= min(avail) && year <= max(avail)))) {
        cand <- avail[abs(avail - year) <= max_gap]
        if (length(cand) > 0) {
          best <- cand[order(abs(cand - year), cand)][1]
          filled_rows[[length(filled_rows) + 1]] <- data.frame(
            Year = year, group = group_val, value = as.numeric(group_map[[as.character(best)]]),
            filled = TRUE, source_year = best, stringsAsFactors = FALSE
          )
          fill_notes[[length(fill_notes) + 1]] <- list(year = year, group = group_val, source_year = best)
        }
      }
    }
  }

  if (length(filled_rows) == 0) {
    out <- agg
    out$filled <- FALSE
    out$source_year <- NA_real_
    return(list(df = out, notes = list()))
  }
  result <- do.call(rbind, filled_rows)
  names(result)[names(result) == "group"] <- group_col
  names(result)[names(result) == "value"] <- value_col
  list(df = result, notes = fill_notes)
}

# Fills gaps within each (group1, group2) series -- e.g. one country's one sector --
# before the caller sums across group2 (e.g. sectors) into a group1 total. Without
# this, a year where most sectors simply weren't reported (not truly zero revenue)
# looks like a real collapse in the group1 total, and the group1-level fill in
# fill_year_gaps() never catches it because the (incomplete) total isn't NA.
# `extrapolate` is forwarded to fill_year_gaps() -- see its docstring.
fill_pair_gaps <- function(df, group1, group2, value_col, max_gap = 2, extrapolate = TRUE) {
  raw <- df
  raw$series_key <- paste(raw[[group1]], raw[[group2]], sep = "␟")
  filled <- fill_year_gaps(raw, "series_key", value_col, max_gap = max_gap, extrapolate = extrapolate)$df
  key_parts <- strsplit(filled$series_key, "␟", fixed = TRUE)
  filled[[group1]] <- vapply(key_parts, `[`, character(1), 1)
  filled[[group2]] <- vapply(key_parts, `[`, character(1), 2)
  filled$series_key <- NULL
  filled
}

# Marks a filled point in the hover tooltip with a trailing asterisk (blank for
# real data) -- the caption below the chart explains what the asterisk means,
# rather than spelling out the source year inline in every tooltip row.
fill_tooltip_note <- function(filled) {
  ifelse(filled, " *", "")
}

fill_caption <- function(notes, max_show = 6) {
  if (length(notes) == 0) return(NULL)
  shown <- notes[seq_len(min(max_show, length(notes)))]
  items <- vapply(shown, function(n) paste0(n$group, " ", n$year, " (from ", n$source_year, ")"), character(1))
  tail <- if (length(notes) > max_show) paste0(", and ", length(notes) - max_show, " more") else ""
  paste0(
    "Data note: ", length(notes), " point", if (length(notes) != 1) "s" else "",
    " on this chart ", if (length(notes) != 1) "were" else "was",
    " missing and filled from the nearest year within ±2 years — ",
    paste(items, collapse = "; "), tail, "."
  )
}
