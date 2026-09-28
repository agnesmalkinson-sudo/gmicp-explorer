pkgs <- c(
  "shiny", "bsicons", "highcharter", "dplyr", "tidyr",
  "stringr", "DT", "readxl", "rsconnect"
)

installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)
if (length(to_install) > 0) install.packages(to_install)
