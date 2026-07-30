# R/generate_hrcastapp.R
# =============================================================================
# generate_hrcastapp() — Shiny dashboard for govhrcast simulation results
# =============================================================================
# Thin orchestrator: normalises input, delegates UI to app_shinyui.R and
# server logic to app_shinyserver.R.
#
# UI helpers:   hz_build_ui()  (app_shinyui.R)
# Server:       hz_server()    (app_shinyserver.R)
# Utilities:    hz_*()         (app_shinyutils.R)
# =============================================================================


#' Launch the govhrcast Shiny Dashboard
#'
#' @description
#' Deploys a bslib-styled interactive dashboard for exploring pre-computed
#' govhrcast simulation results.  The app is a \strong{result browser}: no
#' simulation logic runs inside it.  All computation must be done
#' \emph{before} calling this function via \code{\link{simulate_horizon}} or
#' \code{\link{generate_scenario_matrix}}.
#'
#' @details
#' The function accepts either a \code{horizon} S3 object (output of
#' \code{simulate_horizon}) or a plain \code{data.table} produced by
#' \code{generate_scenario_matrix}.  In both cases the data is normalised
#' internally to a \code{horizon} before rendering.
#'
#' ### Dashboard tabs
#' \describe{
#'   \item{Introduction}{Landing page with project context and guidance.}
#'   \item{Policy Analysis}{KPI value boxes + three sub-chart types for a
#'     single selected scenario.  Includes variable-definition tooltips.}
#'   \item{Scenario Comparator}{Side-by-side overlay of two scenarios with
#'     absolute and percentage delta cards for the terminal year.}
#'   \item{Data & Methodology}{Searchable data table with CSV download,
#'     metadata viewer, and links to source modules on GitHub.}
#' }
#'
#' @param horizon_obj A \code{horizon} object (from \code{simulate_horizon})
#'   or a \code{data.table} (from \code{generate_scenario_matrix}).
#' @param ... Additional arguments passed to \code{shiny::shinyApp()}.
#'
#' @return A \code{shiny.appobj} (invisibly).  Calling in an interactive
#'   session launches the browser; in non-interactive sessions returns the
#'   object.
#'
#' @examples
#' \dontrun{
#' res <- simulate_horizon(
#'   contract_dt       = ct,
#'   personnel_dt      = pt,
#'   salary_scale_dt   = ss,
#'   n_periods         = 5L,
#'   retirement_policy = ret_pol,
#'   salary_growth_rate = 0.03,
#'   ref_date          = as.Date("2015-09-01"),
#'   age_col           = "age",
#'   tenure_col        = "tenure_years"
#' )
#' generate_hrcastapp(res)
#'
#' # Or from a scenario matrix:
#' mat <- generate_scenario_matrix(ct, pt, ss, param_grid = grid, ...)
#' generate_hrcastapp(mat)
#' }
#'
#' @importFrom shiny shinyApp runApp addResourcePath
#' @importFrom data.table is.data.table copy
#' @importFrom thematic thematic_shiny
#' @export
generate_hrcastapp <- function(horizon_obj, ...) {

  # ------------------------------------------------------------------
  # 1. Normalise input to horizon + flat_dt
  # ------------------------------------------------------------------
  if (data.table::is.data.table(horizon_obj)) {
    hz      <- hz_dt_to_horizon(horizon_obj)
    flat_dt <- data.table::copy(horizon_obj)
  } else if (inherits(horizon_obj, "horizon")) {
    hz      <- horizon_obj
    flat_dt <- data.table::copy(horizon_obj$comparison)
  } else {
    stop(
      "horizon_obj must be a 'horizon' S3 object or a data.table from ",
      "generate_scenario_matrix().",
      call. = FALSE
    )
  }

  # Ensure scenario_id / scenario_label / is_baseline are present
  hz_ensure_scenario_cols(flat_dt)

  # Expose inst/www/ so that logo src="govhrcast_logo.png" resolves at runtime
  www_path <- system.file("www", package = "govhrcast")
  if (nchar(www_path) > 0L) {
    shiny::addResourcePath("govhrcast_www", www_path)
  }

  # ------------------------------------------------------------------
  # 2. Pre-compute UI parameters
  # ------------------------------------------------------------------
  thematic::thematic_shiny(font = "auto")
  lever_cols  <- hz_lever_cols(flat_dt)
  scenario_ch <- hz_scenario_choices(flat_dt)

  # ------------------------------------------------------------------
  # 3. Assemble UI and server from split modules
  # ------------------------------------------------------------------
  ui     <- hz_build_ui(flat_dt, lever_cols, scenario_ch)
  server <- hz_server(flat_dt, hz, scenario_ch, lever_cols)

  # ------------------------------------------------------------------
  # 4. Launch
  # ------------------------------------------------------------------
  app <- shiny::shinyApp(ui = ui, server = server, ...)
  if (interactive()) shiny::runApp(app)
  invisible(app)
}
