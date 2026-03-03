# R/plot_horizon.R
# =============================================================================
# S3 plot method for `horizon` objects
# =============================================================================
# plot.horizon(x, type = c("fiscal_basics", "spending_effects", "turnover"), ...)
#
# Dependencies: ggplot2, patchwork, scales  (Imports in DESCRIPTION)
# =============================================================================

# ---------------------------------------------------------------------------
# Internal palette helpers
# ---------------------------------------------------------------------------

.hz_palette <- function() {
  list(
    positive  = "#2166AC",   # blue  — costs / spending
    negative  = "#D6604D",   # red   — savings / reductions
    headcount = "#4DAC26",   # green — workforce stock
    neutral   = "#878787",   # grey  — secondary lines
    accent    = "#F4A582"    # peach — new flows
  )
}

.hz_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      strip.text        = ggplot2::element_text(face = "bold", size = 13),
      panel.grid.minor  = ggplot2::element_blank(),
      legend.position   = "bottom",
      legend.text       = ggplot2::element_text(size = 12),
      plot.title        = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle     = ggplot2::element_text(face = "bold", size = 12,
                                                colour = "grey40"),
      axis.title        = ggplot2::element_text(face = "bold", size = 12),
      axis.text         = ggplot2::element_text(size = 11),
      axis.text.x       = ggplot2::element_text(size = 11, angle = 45, hjust = 1),
      plot.caption      = ggplot2::element_text(face = "bold", size = 11)
    )
}


# ---------------------------------------------------------------------------
# type = "fiscal_basics"
# ---------------------------------------------------------------------------

#' @keywords internal
.plot_fiscal_basics <- function(dt, pal, scenario_col) {

  fmt_currency <- scales::label_number(scale_cut = scales::cut_short_scale())
  fmt_count    <- scales::label_number(scale_cut = scales::cut_short_scale(),
                                       accuracy = 0.1)

  # --- Chart 1: Wage bill ---------------------------------------------------
  p1 <- ggplot2::ggplot(dt,
    ggplot2::aes(x = .data[["period_date"]], y = .data[["wage_bill_end"]],
                 colour = if (scenario_col %in% names(dt)) .data[[scenario_col]] else NULL,
                 group  = if (scenario_col %in% names(dt)) .data[[scenario_col]] else 1)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = fmt_currency) +
    ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                         date_labels = "%d %b %Y") +
    ggplot2::labs(
      title    = "Wage Bill",
      subtitle = "End-of-period gross payroll",
      x        = NULL, y = "LCU", colour = NULL
    ) +
    .hz_theme()

  # --- Chart 2: Pension liability build-up ----------------------------------
  # Reshape pension cols to long for a two-line chart
  has_total <- "pension_cost_total" %in% names(dt)
  has_new   <- "pension_cost_new"   %in% names(dt)

  if (has_total && has_new) {
    pension_long <- data.table::melt(
      dt,
      id.vars       = c("period_date", if (scenario_col %in% names(dt)) scenario_col else NULL),
      measure.vars  = c("pension_cost_total", "pension_cost_new"),
      variable.name = "pension_type",
      value.name    = "value",
      na.rm         = TRUE
    )
    pension_long[, pension_type := data.table::fifelse(
      pension_type == "pension_cost_total", "Total (cumulative)", "New (period)"
    )]

    p2 <- ggplot2::ggplot(pension_long,
      ggplot2::aes(
        x      = .data[["period_date"]],
        y      = .data[["value"]],
        colour = .data[["pension_type"]],
        linetype = .data[["pension_type"]],
        group  = if (scenario_col %in% names(pension_long))
                   interaction(.data[[scenario_col]], .data[["pension_type"]])
                 else .data[["pension_type"]])) +
      ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
      ggplot2::scale_y_continuous(labels = fmt_currency) +
      ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                           date_labels = "%d %b %Y") +
      ggplot2::scale_colour_manual(
        values = c("Total (cumulative)" = pal$positive,
                   "New (period)"       = pal$accent)) +
      ggplot2::labs(
        title    = "Pension Liability",
        subtitle = "New-period flow vs. cumulative stock",
        x        = NULL, y = "LCU", colour = NULL, linetype = NULL
      ) +
      .hz_theme()
  } else {
    # Graceful fallback: whichever col is available
    pcol <- if (has_total) "pension_cost_total" else "pension_cost_new"
    p2 <- ggplot2::ggplot(dt[!is.na(dt[[pcol]]), ],
      ggplot2::aes(x = .data[["period_date"]], y = .data[[pcol]],
                   colour = if (scenario_col %in% names(dt)) .data[[scenario_col]] else NULL,
                   group  = if (scenario_col %in% names(dt)) .data[[scenario_col]] else 1)) +
      ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
      ggplot2::scale_y_continuous(labels = fmt_currency) +
      ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                           date_labels = "%d %b %Y") +
      ggplot2::labs(title = "Pension Cost", x = NULL, y = "LCU", colour = NULL) +
      .hz_theme()
  }

  # --- Chart 3: Inflation effect --------------------------------------------
  p3 <- ggplot2::ggplot(dt,
    ggplot2::aes(x = .data[["period_date"]], y = .data[["inflation_effect"]],
                 colour = if (scenario_col %in% names(dt)) .data[[scenario_col]] else NULL,
                 group  = if (scenario_col %in% names(dt)) .data[[scenario_col]] else 1)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = fmt_currency) +
    ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                         date_labels = "%d %b %Y") +
    ggplot2::labs(
      title    = "Inflation / COLA Effect",
      subtitle = "Annual payroll increment from salary_growth_rate",
      x        = NULL, y = "LCU added", colour = NULL
    ) +
    .hz_theme()

  out <- p1 + p2 + p3 + patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  desc <- paste(
    "fiscal_basics: Three-panel line chart showing core fiscal drivers.",
    "wage_bill_end: Total gross payroll at the end of each simulated period.",
    "pension_cost_new: Pension obligations newly issued this period (the check",
    "  written for workers who retired in this period).",
    "pension_cost_total: Cumulative pension payroll -- the total government",
    "  pension liability across all living retirees to date.",
    "inflation_effect: Payroll increment attributable to the COLA/salary_growth_rate",
    "  applied to all active contracts and the salary scale each period."
  )
  attr(out, "description") <- desc
  out
}


# ---------------------------------------------------------------------------
# type = "spending_effects"
# ---------------------------------------------------------------------------

#' @keywords internal
.plot_spending_effects <- function(dt, pal, scenario_col) {

  fmt_currency <- scales::label_number(scale_cut = scales::cut_short_scale())
  fmt_pct      <- scales::label_percent(accuracy = 0.1)

  effect_abs <- c("hiring_effect", "promotion_effect",
                  "transfer_effect", "inflation_effect")
  eff_labels <- c(
    hiring_effect    = "Hiring",
    promotion_effect = "Promotions",
    transfer_effect  = "Transfers",
    inflation_effect = "Inflation / COLA"
  )

  # Absolute effects — bar chart faceted by effect type
  abs_cols    <- intersect(effect_abs, names(dt))
  id_vars_abs <- unique(c("period_date",
                          if (scenario_col %in% names(dt)) scenario_col else NULL))

  long_abs <- data.table::melt(
    dt,
    id.vars      = id_vars_abs,
    measure.vars = abs_cols,
    variable.name = "effect",
    value.name    = "value",
    na.rm         = TRUE
  )
  long_abs[, effect_label := eff_labels[as.character(effect)]]
  long_abs[, fill_dir     := data.table::fifelse(value >= 0, "cost", "saving")]

  p_abs <- ggplot2::ggplot(long_abs,
    ggplot2::aes(
      x    = .data[["period_date"]],
      y    = .data[["value"]],
      fill = .data[["fill_dir"]],
      colour = if (scenario_col %in% names(dt)) .data[[scenario_col]] else NULL
    )) +
    ggplot2::geom_col(position = "dodge", width = 200, na.rm = TRUE) +
    ggplot2::facet_wrap(~ effect_label, scales = "free_y", ncol = 2L) +
    ggplot2::scale_fill_manual(
      values = c(cost = pal$positive, saving = pal$negative),
      labels = c(cost = "Cost (+)", saving = "Saving (\u2212)")
    ) +
    ggplot2::scale_y_continuous(labels = fmt_currency) +
    ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                         date_labels = "%d %b %Y") +
    ggplot2::labs(
      title    = "Spending Effect Decomposition",
      subtitle = "Per-period absolute value of each policy driver",
      x = NULL, y = "LCU", fill = NULL, colour = NULL
    ) +
    .hz_theme()

  # Efficiency metric: exit_savings as % of end wage bill
  pct_col <- "exit_savings_pct_of_end_bill"
  if (pct_col %in% names(dt)) {
    dt_eff <- dt[!is.na(dt[[pct_col]]), ]
    p_eff <- ggplot2::ggplot(dt_eff,
      ggplot2::aes(
        x      = .data[["period_date"]],
        y      = .data[[pct_col]],
        colour = if (scenario_col %in% names(dt_eff)) .data[[scenario_col]] else NULL,
        group  = if (scenario_col %in% names(dt_eff)) .data[[scenario_col]] else 1
      )) +
      ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
      ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
      ggplot2::scale_y_continuous(labels = fmt_pct) +
      ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                           date_labels = "%d %b %Y") +
      ggplot2::labs(
        title    = "Exit Savings Efficiency",
        subtitle = "Retirements savings as % of end-period wage bill",
        x = NULL, y = "% of wage bill", colour = NULL
      ) +
      .hz_theme()

    out <- (p_abs / p_eff) + patchwork::plot_layout(heights = c(3, 1), guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    out <- p_abs
  }

  desc <- paste(
    "spending_effects: Decomposition of per-period spending changes.",
    "hiring_effect: Total gross salary of all new-hire contracts created in",
    "  this period (positive = additional payroll cost).",
    "promotion_effect: Net salary increment for promoted workers (positive =",
    "  upward regrading cost).",
    "transfer_effect: Net salary change for workers transferred between groups",
    "  (can be positive or negative depending on destination grade).",
    "inflation_effect: Total payroll increase from applying salary_growth_rate",
    "  to all active contracts and the salary scale.",
    "exit_savings_pct_of_end_bill: Salary mass freed by retirements expressed",
    "  as a share of the end-period wage bill -- measures fiscal space created."
  )
  attr(out, "description") <- desc
  out
}


# ---------------------------------------------------------------------------
# type = "turnover"
# ---------------------------------------------------------------------------

#' @keywords internal
.plot_turnover <- function(dt, pal, scenario_col) {

  fmt_count <- scales::label_number(scale_cut = scales::cut_short_scale(),
                                    accuracy = 0.1)

  flow_cols <- intersect(c("n_hires", "n_exits"), names(dt))
  id_vars   <- unique(c("period_date",
                        if (scenario_col %in% names(dt)) scenario_col else NULL))

  # Flow panel: n_hires and n_exits as lines
  long_flow <- data.table::melt(
    dt,
    id.vars       = id_vars,
    measure.vars  = flow_cols,
    variable.name = "metric",
    value.name    = "value",
    na.rm         = TRUE
  )
  flow_labels <- c(n_hires = "New hires", n_exits = "Retirements / exits")
  long_flow[, metric_label := flow_labels[as.character(metric)]]

  p_flow <- ggplot2::ggplot(long_flow,
    ggplot2::aes(
      x      = .data[["period_date"]],
      y      = .data[["value"]],
      colour = .data[["metric_label"]],
      linetype = .data[["metric_label"]],
      group  = if (scenario_col %in% names(long_flow))
                 interaction(.data[[scenario_col]], .data[["metric_label"]])
               else .data[["metric_label"]]
    )) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(
      values = c("New hires"            = pal$positive,
                 "Retirements / exits"  = pal$negative)) +
    ggplot2::scale_y_continuous(labels = fmt_count) +
    ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                         date_labels = "%d %b %Y") +
    ggplot2::labs(
      title    = "Workforce Flows",
      subtitle = "Annual hires and exits",
      x = NULL, y = "Persons", colour = NULL, linetype = NULL
    ) +
    .hz_theme()

  # Stock panel: n_headcount_end — separate axis since scale differs
  if ("n_headcount_end" %in% names(dt)) {
    p_stock <- ggplot2::ggplot(dt[!is.na(dt[["n_headcount_end"]]), ],
      ggplot2::aes(
        x      = .data[["period_date"]],
        y      = .data[["n_headcount_end"]],
        colour = if (scenario_col %in% names(dt)) .data[[scenario_col]] else NULL,
        group  = if (scenario_col %in% names(dt)) .data[[scenario_col]] else 1
      )) +
      ggplot2::geom_line(colour = pal$headcount, linewidth = 0.9, na.rm = TRUE) +
      ggplot2::geom_point(colour = pal$headcount, size = 1.8, na.rm = TRUE) +
      ggplot2::scale_y_continuous(labels = fmt_count) +
      ggplot2::scale_x_date(breaks = sort(unique(dt[["period_date"]])),
                           date_labels = "%d %b %Y") +
      ggplot2::labs(
        title    = "Workforce Stock",
        subtitle = "Active headcount at end of period",
        x = NULL, y = "Persons", colour = NULL
      ) +
      .hz_theme()

    out <- (p_flow / p_stock) + patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  } else {
    out <- p_flow
  }

  desc <- paste(
    "turnover: Workforce dynamics across the simulation horizon.",
    "n_hires: Count of new contracts created in this period.",
    "n_exits: Count of contracts ended by retirement (or other attrition)",
    "  in this period.",
    "n_headcount_end: Total active (non-pensioner) contract rows remaining",
    "  at the end of the period -- the workforce stock after all period events."
  )
  attr(out, "description") <- desc
  out
}


# =============================================================================
# Public S3 method
# =============================================================================

#' Plot a `horizon` simulation result
#'
#' @description
#' S3 method for objects of class \code{horizon}.  Produces one of three
#' ggplot2/patchwork composite charts summarising the simulation output.
#'
#' @details
#' ### Plot types
#'
#' \describe{
#'   \item{\code{"fiscal_basics"}}{
#'     Three side-by-side line charts: (1) end-of-period wage bill, (2) pension
#'     liability build-up showing both the \emph{new} period flow
#'     (\code{pension_cost_new}) and the \emph{cumulative stock}
#'     (\code{pension_cost_total}), and (3) the inflation / COLA effect.}
#'   \item{\code{"spending_effects"}}{
#'     Bar charts for each spending driver — hiring, promotions, transfers,
#'     inflation — using a diverging palette (blue = cost, red = saving), plus
#'     a line showing exit savings as a share of the end-period wage bill.}
#'   \item{\code{"turnover"}}{
#'     Flow lines for annual hires and exits, and a separate stock line for
#'     end-of-period headcount.}
#' }
#'
#' A \code{description} attribute is attached to every returned plot object
#' with technical definitions of all plotted variables, suitable for use as
#' tooltip text in interactive dashboards.
#'
#' ### Scenario handling
#' If \code{x$comparison} contains a \code{scenario} column (as produced by
#' \code{\link{generate_scenario_matrix}}), each scenario is drawn as a
#' separate line/colour group.  Single-scenario \code{horizon} objects produced
#' by \code{\link{simulate_horizon}} are plotted without a legend.
#'
#' ### NA handling
#' All geoms pass \code{na.rm = TRUE}.  Rows where the plotted column is
#' \code{NA} are silently dropped; the remaining data are still rendered.
#'
#' @param x A \code{horizon} object.
#' @param type Character scalar.  One of \code{"fiscal_basics"},
#'   \code{"spending_effects"}, or \code{"turnover"}.
#'   Defaults to \code{"fiscal_basics"}.
#' @param scenario_col Character.  Name of the column in \code{x$comparison}
#'   that distinguishes scenarios.  Defaults to \code{"scenario"}.
#' @param ... Passed to ggplot2 theme overrides (currently unused).
#'
#' @return A \code{patchwork} / \code{ggplot} object, returned \strong{invisibly}.
#'   A \code{description} attribute is attached with variable definitions.
#'   Use \code{print()} or simply type the object name to display it.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' res <- simulate_horizon(
#'   contract_dt        = ct,
#'   personnel_dt       = pt,
#'   salary_scale_dt    = ss,
#'   n_periods          = 5L,
#'   retirement_policy  = ret_pol,
#'   salary_growth_rate = 0.03,
#'   ref_date           = as.Date("2015-09-01"),
#'   age_col            = "age",
#'   tenure_col         = "tenure_years"
#' )
#'
#' plot(res, type = "fiscal_basics")
#' plot(res, type = "spending_effects")
#' plot(res, type = "turnover")
#'
#' # Inspect variable definitions
#' p <- plot(res, type = "turnover")
#' cat(attr(p, "description"))
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_col
#'   facet_wrap scale_y_continuous scale_x_date scale_colour_manual
#'   scale_fill_manual labs theme_minimal theme element_text element_blank
#' @importFrom patchwork plot_layout
#' @importFrom scales label_number cut_short_scale label_percent
#' @export
plot.horizon <- function(x,
                         type         = c("fiscal_basics", "spending_effects", "turnover"),
                         scenario_col = "scenario",
                         ...) {
  validate_horizon(x)
  type <- match.arg(type)

  dt  <- data.table::copy(x$comparison)
  pal <- .hz_palette()

  out <- switch(
    type,
    fiscal_basics    = .plot_fiscal_basics(dt, pal, scenario_col),
    spending_effects = .plot_spending_effects(dt, pal, scenario_col),
    turnover         = .plot_turnover(dt, pal, scenario_col)
  )

  invisible(out)
}
