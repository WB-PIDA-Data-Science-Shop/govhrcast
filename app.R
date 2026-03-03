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

library(govhrcast)

# Load the pre-computed scenario results bundled with the deployment.
# Replace this path / object with your actual results data.
# For a Connect deployment you can either:
#   (a) bundle an RDS alongside app.R and readRDS() it here, or
#   (b) source a data-prep script that builds results from raw data.
results <- readRDS("spielplatz/results_full.rds")

generate_hrcastapp(results)
