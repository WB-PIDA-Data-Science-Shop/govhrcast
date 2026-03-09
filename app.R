# app.R
# =============================================================================
# Posit Connect deployment entry point for govhrcast
# =============================================================================
# This file is the standard Shiny app.R entry point used by rsconnect when
# deploying via rsconnect::deployApp(appDir = getwd()).
#
# Usage (from the package root):
#   rsconnect::deployApp(appDir = getwd(), appTitle = "govhrcast")
# =============================================================================

pkgload::load_all(".")
# Load the pre-computed scenario results from inst/extdata/ (bundled with the
# package and included in the rsconnect deployment bundle automatically).

results <- readRDS("inst/extdata/results_full.rds")

generate_hrcastapp(results)
