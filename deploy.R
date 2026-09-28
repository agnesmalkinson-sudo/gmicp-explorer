# One-time setup (per machine): copy the rsconnect::setAccountInfo(...) snippet from
# shinyapps.io > Account > Tokens and run it once in your local R console.
#
# Then, any time you want to publish or update the live app, run:
#   source("deploy.R")

source("R/data.R")
rebuild_cache() # keeps the .rds startup cache in sync with the current xlsx

# Production hides the Merger Simulator for now (still being iterated on) --
# temporarily flips app.R's SHOW_MERGER_SIMULATOR flag off just for this
# deploy, then restores the working copy back to TRUE afterward regardless of
# whether the deploy succeeds, so staging/local dev keep seeing it. Backup
# lives outside the app directory (tempdir()) so it never ends up bundled
# into the deploy itself the way a same-directory "app.R.bak" would.
app_r_backup <- file.path(tempdir(), "gmicp_app_R_backup.R")
file.copy("app.R", app_r_backup, overwrite = TRUE)
app_r_lines <- readLines("app.R")
app_r_lines <- sub("^SHOW_MERGER_SIMULATOR <- TRUE$", "SHOW_MERGER_SIMULATOR <- FALSE", app_r_lines)
writeLines(app_r_lines, "app.R")

tryCatch({
  rsconnect::deployApp(
    appDir = ".",
    appName = "gmicp-explorer",
    appTitle = "GMICP Explorer",
    forceUpdate = TRUE
  )
}, finally = {
  file.copy(app_r_backup, "app.R", overwrite = TRUE)
  file.remove(app_r_backup)
})
