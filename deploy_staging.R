# Deploys to a separate staging app on shinyapps.io (gmicp-explorer-staging),
# completely independent from the live app -- use this to try out changes
# (including swapping in a test data workbook) before publishing to production
# with deploy.R. Same one-time account setup as deploy.R.
#
# Run with: source("deploy_staging.R")

source("R/data.R")
rebuild_cache() # keeps the .rds startup cache in sync with the current xlsx

rsconnect::deployApp(
  appDir = ".",
  appName = "gmicp-explorer-staging",
  appTitle = "GMICP Explorer (Staging)",
  forceUpdate = TRUE
)
