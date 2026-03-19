# R/app_shinyutils.R
# =============================================================================
# Pure utility functions for the govhrcast Shiny app
# =============================================================================
# These are side-effect-free helpers shared between the UI and server layers.
# All functions are prefixed with `hz_` (exported) or `.hz_app_` (internal).
# None of these functions start a Shiny session or produce side-effects.
# =============================================================================

# ---------------------------------------------------------------------------
# Reserved column names that are NOT lever/parameter columns
# ---------------------------------------------------------------------------
.HZ_RESERVED_COLS <- c(
  "scenario_id", "scenario_label", "is_baseline", "period_date",
  "n_headcount_start", "n_headcount_end",
  "wage_bill_start",   "wage_bill_end",
  "n_exits",           "exit_savings",
  "n_non_ret_exits",   "non_ret_exit_savings",
  "pension_cost_new",  "pension_cost_total",
  "n_promotions",      "n_transfers",
  "promotion_effect",  "transfer_effect",
  "n_hires",           "hiring_effect", "inflation_effect",
  "exit_savings_pct_of_end_bill",
  "non_ret_exit_savings_pct_of_end_bill",
  "promotion_effect_pct_of_end_bill",
  "transfer_effect_pct_of_end_bill",
  "hiring_effect_pct_of_end_bill",
  "inflation_effect_pct_of_end_bill"
)


#' Wrap a generate_scenario_matrix data.table into a horizon object
#'
#' @description
#' Converts a plain \code{data.table} (as produced by
#' \code{\link{generate_scenario_matrix}}) into a minimal \code{horizon} S3
#' object so that \code{plot.horizon()} can be called uniformly throughout the
#' Shiny app.
#'
#' @param dt A \code{data.table} with at least \code{period_date} and
#'   \code{wage_bill_end} columns.
#' @return A \code{horizon} object.
#' @keywords internal
hz_dt_to_horizon <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  new_horizon(
    comparison = dt,
    metadata   = list(
      policy_args = list(),
      param_grid  = attr(dt, "param_grid")
    )
  )
}


#' Format a large number for display
#'
#' @description
#' Returns a compact human-readable string for a numeric value using SI
#' suffixes (K, M, B).  Returns \code{"N/A"} for missing values.
#'
#' @param x Numeric scalar.
#' @return Character scalar.
#' @keywords internal
hz_fmt_big <- function(x) {
  if (length(x) == 0L || is.null(x) || is.na(x)) return("N/A")
  scales::label_number(scale_cut = scales::cut_short_scale(),
                       accuracy  = 0.1)(x)
}


#' Identify lever (parameter) columns in a scenario data.table
#'
#' @description
#' Returns the names of columns that represent policy levers — i.e. all
#' columns that are \emph{not} reserved time-series output columns.
#'
#' @param dt A \code{data.table} produced by \code{\link{generate_scenario_matrix}}.
#' @return Character vector of lever column names (may be length 0).
#' @keywords internal
hz_lever_cols <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  setdiff(names(dt), .HZ_RESERVED_COLS)
}


#' Extract the terminal (final-period) row for a scenario
#'
#' @description
#' Returns the single row of \code{dt} with the latest \code{period_date}
#' for the given \code{scenario_id}.
#'
#' @param dt A \code{data.table} with columns \code{scenario_id} and
#'   \code{period_date}.
#' @param sid Integer or numeric.  The scenario identifier to filter on.
#' @return A one-row \code{data.table}, or a zero-row table if \code{sid} is
#'   not found.
#' @keywords internal
hz_terminal_row <- function(dt, sid) {
  stopifnot(data.table::is.data.table(dt))
  sub <- dt[scenario_id == sid]
  if (nrow(sub) == 0L) return(sub)
  sub[which.max(period_date)]
}


#' Build a named list of scenario choices for selectInput
#'
#' @description
#' Returns a named list mapping \code{scenario_label} → \code{scenario_id},
#' suitable for passing directly to \code{shiny::selectInput(choices = ...)}.
#'
#' @param dt A \code{data.table} with columns \code{scenario_id} and
#'   \code{scenario_label}.
#' @return Named list.
#' @keywords internal
hz_scenario_choices <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  if (!all(c("scenario_id", "scenario_label") %in% names(dt))) {
    stop("dt must contain 'scenario_id' and 'scenario_label' columns.",
         call. = FALSE)
  }
  pairs <- unique(dt[, .(scenario_id, scenario_label)])
  setNames(as.list(pairs$scenario_id), pairs$scenario_label)
}


#' Build the delta card HTML for the Scenario Comparator tab
#'
#' @description
#' Returns a \code{shiny.tag.list} showing Scenario A and B values, the
#' absolute delta, and the percentage change between them.  Positive deltas
#' are coloured red (cost increase) and negative deltas green (saving).
#'
#' @param val_a Numeric scalar.  Terminal-year value for Scenario A.
#' @param val_b Numeric scalar.  Terminal-year value for Scenario B.
#' @param label_a Character.  Display label for Scenario A.
#' @param label_b Character.  Display label for Scenario B.
#' @return A \code{shiny.tag.list}.
#' @keywords internal
hz_delta_card_html <- function(val_a, val_b, label_a, label_b) {
  if (is.null(val_a) || is.null(val_b) ||
      length(val_a) == 0L || length(val_b) == 0L ||
      is.na(val_a)  || is.na(val_b)) {
    return(shiny::p("N/A"))
  }

  delta     <- val_b - val_a
  delta_pct <- if (!is.na(val_a) && val_a != 0) (delta / abs(val_a)) * 100
               else NA_real_

  colour <- if (delta > 0) "#c0392b" else "#27ae60"
  arrow  <- if (delta > 0) "\u25b2"  else "\u25bc"
  sign   <- if (delta > 0) "+" else ""

  shiny::tagList(
    shiny::p(shiny::strong(paste0(label_a, ":")), hz_fmt_big(val_a)),
    shiny::p(shiny::strong(paste0(label_b, ":")), hz_fmt_big(val_b)),
    shiny::hr(),
    shiny::p(
      style = paste0("font-size:1.3rem; font-weight:bold; color:", colour, ";"),
      paste0(arrow, " ", sign, hz_fmt_big(delta)),
      if (!is.na(delta_pct))
        shiny::span(
          style = "font-size:0.9rem; color:#555; margin-left:0.5rem;",
          sprintf("(%s%.1f%%)", sign, delta_pct)
        )
    )
  )
}


#' Build the govhrcast app bslib theme
#'
#' @description
#' Returns a \code{bslib::bs_theme()} object with the standard govhrcast
#' colour palette and typography.  Mirrors the cpiaapp theme exactly
#' (litera bootswatch, Source Sans Pro body, Fira Sans headings).
#'
#' @return A \code{bs_theme} object.
#' @keywords internal
hz_app_theme <- function() {
  bslib::bs_theme(
    bootswatch   = "litera",
    base_font    = bslib::font_google("Source Sans Pro", local = FALSE),
    code_font    = bslib::font_google("Source Sans Pro", local = FALSE),
    heading_font = bslib::font_google("Fira Sans",       local = FALSE),
    navbar_bg    = "#FFFFFF"
  )
}


#' Convert a ggplot object to an interactive plotly figure
#'
#' @description
#' Converts a single \code{ggplot2} object to a \code{plotly} figure using
#' \code{plotly::ggplotly()}.  Opens a temporary PDF device so that
#' \code{ggplotly()} can compute plot geometry without a screen device — this
#' ensures correct rendering in Shiny server contexts and allows plotly to
#' auto-size to the browser container.
#'
#' @param p A \code{ggplot2} plot object (not a patchwork composite).
#' @param tooltip Character vector of tooltip aesthetics to show.
#' @return A \code{plotly} object.
#' @keywords internal
hz_to_plotly <- function(p,
                         tooltip = c("x", "y", "colour", "fill",
                                     "linetype", "label")) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp, width = 10, height = 5)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  tryCatch(
    plotly::ggplotly(p, tooltip = tooltip) |>
      plotly::layout(
        legend = list(
          orientation = "h",
          x           = 1,
          y           = 1.02,
          xanchor     = "right",
          yanchor     = "bottom",
          font        = list(size = 11)
        )
      ),
    error = function(e) plotly::plotly_empty()
  )
}


#' Ensure a flat data.table has scenario columns
#'
#' @description
#' If a single-scenario \code{horizon} was passed, the flat \code{comparison}
#' table will be missing \code{scenario_id}, \code{scenario_label}, and
#' \code{is_baseline}.  This helper adds them so all downstream app code can
#' assume they are present.
#'
#' @param dt A \code{data.table}.
#' @return \code{dt} with columns added in-place (returns \code{dt}
#'   invisibly).
#' @keywords internal
hz_ensure_scenario_cols <- function(dt) {
  stopifnot(data.table::is.data.table(dt))
  if (!"scenario_id" %in% names(dt))    dt[, scenario_id    := 1L]
  if (!"scenario_label" %in% names(dt)) dt[, scenario_label := "Simulation"]
  if (!"is_baseline" %in% names(dt))    dt[, is_baseline    := TRUE]
  invisible(dt)
}
