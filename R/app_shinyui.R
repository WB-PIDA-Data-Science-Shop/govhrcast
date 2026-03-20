# R/app_shinyui.R
# =============================================================================
# UI builders for the govhrcast Shiny dashboard
# =============================================================================
# Exports:
#   hz_build_ui(flat_dt, lever_cols, scenario_ch)  — full page_navbar UI
#
# Internal tab builders:
#   .hz_tab_intro()
#   .hz_tab_policy()
#   .hz_tab_comparator()
#   .hz_tab_data()
#   .hz_sidebar(flat_dt, lever_cols, scenario_ch)
# =============================================================================


# ---------------------------------------------------------------------------
# Tab 1: Introduction (landing page)
# ---------------------------------------------------------------------------

#' @keywords internal
.hz_tab_intro <- function() {

  bslib::nav_panel(
    title = shiny::icon("house"),
    value = "tab_intro",
    bslib::card(
      full_screen = FALSE,
      # Logo fills the card header with no padding — mirrors CPIA app pattern
      bslib::card_header(
        shiny::tags$img(
          src   = "govhrcast_www/govhrcast_logo.png",
          alt   = "govhrcast logo",
          width = "100%",
          style = "display:block;"
        )
      ),
      bslib::card_body(
        shiny::tags$br(),
        shiny::tags$h3("Welcome to the Wage Bill Projection Dashboard"),
        shiny::hr(),
        shiny::p(
            "This dashboard provides a data-driven approach to simulate
             the long-term fiscal impact of workforce policies
             (Retirement, Hiring, and Inflation)."
          ),
          shiny::h4("Purpose"),
          shiny::p(
            "This tool assists fiscal and HR specialists in evaluating wage
             bill sustainability. It synthesizes current contract data with
             policy levers to deliver:"
          ),
          shiny::tags$ul(
            shiny::tags$li(
              shiny::strong("Evidence-based projections"),
              " for wage bill and pension growth."
            ),
            shiny::tags$li(
              shiny::strong("Transparent decomposition"),
              " of spending drivers (Hiring vs. Promotions)."
            ),
            shiny::tags$li(
              shiny::strong("Comparative context"),
              " across different retirement and COLA scenarios."
            )
          ),
          shiny::div(
            class = "alert alert-warning",
            style = "margin-top:1rem;",
            shiny::strong("Note:"),
            " These projections are data-driven simulations based on the
             \u2018Simple Sum\u2019 accounting logic. They are intended to
             support professional judgment in fiscal planning rather than
             provide definitive budgetary guarantees."
          ),
          shiny::h4("Dashboard Structure"),
          shiny::tags$ol(
            shiny::tags$li(
              shiny::strong("Policy Analysis:"),
              " Visualizes core fiscal trends, spending drivers, and
               turnover for a selected scenario."
            ),
            shiny::tags$li(
              shiny::strong("Scenario Comparator:"),
              " Overlays two policy paths and shows terminal-year deltas."
            ),
            shiny::tags$li(
              shiny::strong("Data & Methodology:"),
              " Provides the raw simulation tables and links to the
               open-source logic."
            )
          )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# Tab 2: Policy Analysis
# ---------------------------------------------------------------------------

#' @keywords internal
.hz_tab_policy <- function(flat_dt, lever_cols) {
  bslib::nav_panel(
    title = "Policy Analysis",
    value = "tab_policy",

    bslib::layout_sidebar(
      sidebar = .hz_sidebar(flat_dt, lever_cols),

    # KPI row ----------------------------------------------------------------
    bslib::layout_column_wrap(
      width = "250px",
      bslib::value_box(
        title    = "Terminal Wage Bill",
        value    = shiny::textOutput("kpi_wage_bill"),
        showcase = bsicons::bs_icon("cash-stack"),
        theme    = "primary"
      ),
      bslib::value_box(
        title    = "Cumulative Pension Liability",
        value    = shiny::textOutput("kpi_pension"),
        showcase = bsicons::bs_icon("person-check"),
        theme    = "secondary"
      ),
      bslib::value_box(
        title    = "Terminal Headcount",
        value    = shiny::textOutput("kpi_headcount"),
        showcase = bsicons::bs_icon("people"),
        theme    = "success"
      )
    ),

    # Sub-nav chart cards ----------------------------------------------------
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Fiscal Basics",
        bslib::card_body(
          bslib::popover(
            bsicons::bs_icon("info-circle", class = "text-muted"),
            shiny::uiOutput("desc_fiscal"),
            placement = "right"
          ),
          shiny::div(
            style = "overflow-y: auto; max-height: 85vh;",
            # 3 panels side by side — mirrors patchwork p1 + p2 + p3
            bslib::layout_column_wrap(
              width = 1 / 3,
              plotly::plotlyOutput("plot_fiscal_1", height = "420px"),
              plotly::plotlyOutput("plot_fiscal_2", height = "420px"),
              plotly::plotlyOutput("plot_fiscal_3", height = "420px")
            )
          )
        )
      ),
      bslib::nav_panel(
        "Spending Effects",
        bslib::card_body(
          bslib::popover(
            bsicons::bs_icon("info-circle", class = "text-muted"),
            shiny::uiOutput("desc_spending"),
            placement = "right"
          ),
          shiny::div(
            style = "overflow-y: auto; max-height: 85vh;",
            plotly::plotlyOutput("plot_spending_1", height = "650px"),
            shiny::tags$div(style = "height: 1.5rem;"),
            plotly::plotlyOutput("plot_spending_2", height = "380px")
          )
        )
      ),
      bslib::nav_panel(
        "Turnover Dynamics",
        bslib::card_body(
          bslib::popover(
            bsicons::bs_icon("info-circle", class = "text-muted"),
            shiny::uiOutput("desc_turnover"),
            placement = "right"
          ),
          shiny::div(
            style = "overflow-y: auto; max-height: 85vh;",
            # 2 panels side by side — flows | stock
            bslib::layout_column_wrap(
              width = 1 / 2,
              plotly::plotlyOutput("plot_turnover_1", height = "450px"),
              plotly::plotlyOutput("plot_turnover_2", height = "450px")
            )
          )
        )
      )
    )
  ) # end layout_sidebar
  ) # end nav_panel
}


# ---------------------------------------------------------------------------
# Tab 3: Scenario Comparator
# ---------------------------------------------------------------------------

#' @keywords internal
.hz_tab_comparator <- function(flat_dt, lever_cols) {

  has_levers <- length(lever_cols) > 0L

  # ---- Sidebar: Scenario A -------------------------------------------------
  if (has_levers) {
    # Lever-mode: one selectInput per lever, A defaults to baseline row
    baseline_row <- if ("is_baseline" %in% names(flat_dt) && any(flat_dt$is_baseline)) {
      unique(flat_dt[is_baseline == TRUE, .SD, .SDcols = lever_cols])[1L]
    } else {
      unique(flat_dt[, .SD, .SDcols = lever_cols])[1L]
    }
    all_lever_rows <- unique(flat_dt[, .SD, .SDcols = lever_cols])
    alt_row <- if (nrow(all_lever_rows) >= 2L) all_lever_rows[2L] else all_lever_rows[1L]

    make_lever_inputs <- function(prefix, def_row) {
      lapply(lever_cols, function(lv) {
        vals  <- sort(unique(flat_dt[[lv]]))
        label <- tools::toTitleCase(gsub("_", " ", lv))
        def   <- if (!is.null(def_row) && lv %in% names(def_row)) def_row[[lv]] else vals[1L]
        shiny::selectInput(paste0(prefix, lv), label, choices = vals, selected = def)
      })
    }
    sidebar_a_inputs <- make_lever_inputs("cmp_a_lv_", baseline_row)
    sidebar_b_inputs <- make_lever_inputs("cmp_b_lv_", alt_row)
  } else {
    # Label-mode: one selectInput per scenario slot
    choices  <- hz_scenario_choices(flat_dt)
    all_sids <- unique(flat_dt$scenario_id)
    def_a    <- if ("is_baseline" %in% names(flat_dt) && any(flat_dt$is_baseline))
                  flat_dt[is_baseline == TRUE, scenario_id[1L]]
                else all_sids[1L]
    def_b    <- if (length(all_sids) >= 2L) all_sids[2L] else all_sids[1L]
    sidebar_a_inputs <- list(
      shiny::selectInput("cmp_a_sid", "Scenario", choices = choices, selected = def_a)
    )
    sidebar_b_inputs <- list(
      shiny::selectInput("cmp_b_sid", "Scenario", choices = choices, selected = def_b)
    )
  }

  sidebar_content <- bslib::sidebar(
    width = 320,
    # Scenario A card — blue header
    bslib::card(
      bslib::card_header(
        shiny::strong("\u25a0 Scenario A"),
        class = "bg-primary text-white"
      ),
      bslib::card_body(
        class  = "py-2",
        sidebar_a_inputs
      )
    ),
    shiny::tags$div(style = "height: 0.75rem;"),
    # Scenario B card — green header
    bslib::card(
      bslib::card_header(
        shiny::strong("\u25a0 Scenario B"),
        class = "bg-success text-white"
      ),
      bslib::card_body(
        class  = "py-2",
        sidebar_b_inputs
      )
    ),
    shiny::hr(),
    shiny::actionButton(
      "cmp_show_results",
      label = "Compare",
      icon  = shiny::icon("arrows-left-right"),
      class = "btn-primary w-100"
    )
  )

  bslib::nav_panel(
    title = "Scenario Comparator",
    value = "tab_compare",

    bslib::layout_sidebar(
      sidebar = sidebar_content,

      # KPI delta tiles ------------------------------------------------------
      bslib::layout_column_wrap(
        width = "250px",
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("cash-stack"), " Wage Bill",
            class = "fw-bold"
          ),
          bslib::card_body(shiny::uiOutput("diff_wage_bill"))
        ),
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("person-check"), " Pension Liability",
            class = "fw-bold"
          ),
          bslib::card_body(shiny::uiOutput("diff_pension"))
        ),
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("people"), " Headcount",
            class = "fw-bold"
          ),
          bslib::card_body(shiny::uiOutput("diff_headcount"))
        )
      ),

      # Overlay chart panels -------------------------------------------------
      bslib::navset_card_pill(
        bslib::nav_panel(
          "Fiscal Basics",
          bslib::card_body(
            shiny::div(
              style = "overflow-y: auto; max-height: 85vh;",
              bslib::layout_column_wrap(
                width = 1 / 3,
                plotly::plotlyOutput("cmp_fiscal_1", height = "420px"),
                plotly::plotlyOutput("cmp_fiscal_2", height = "420px"),
                plotly::plotlyOutput("cmp_fiscal_3", height = "420px")
              )
            )
          )
        ),
        bslib::nav_panel(
          "Spending Effects",
          bslib::card_body(
            # Horizontally scrollable wide chart so dodged bars have room
            shiny::div(
              style = "overflow-x: auto; overflow-y: auto; max-height: 85vh;",
              shiny::div(
                style = "min-width: 1400px;",
                plotly::plotlyOutput("cmp_spending_1", height = "650px"),
                shiny::tags$div(style = "height: 1.5rem;"),
                plotly::plotlyOutput("cmp_spending_2", height = "380px")
              )
            )
          )
        ),
        bslib::nav_panel(
          "Turnover Dynamics",
          bslib::card_body(
            shiny::div(
              style = "overflow-y: auto; max-height: 85vh;",
              bslib::layout_column_wrap(
                width = 1 / 2,
                plotly::plotlyOutput("cmp_turnover_1", height = "450px"),
                plotly::plotlyOutput("cmp_turnover_2", height = "450px")
              )
            )
          )
        )
      )
    ) # end layout_sidebar
  ) # end nav_panel
}


# ---------------------------------------------------------------------------
# Tab 4: Data & Methodology
# ---------------------------------------------------------------------------

#' @keywords internal
.hz_tab_data <- function() {
  bslib::nav_panel(
    title = "Data & Methodology",
    value = "tab_data",

    bslib::navset_card_pill(
      bslib::nav_panel(
        "Raw Data",
        bslib::card_body(
          shiny::downloadButton("dl_csv", "Download CSV",
                                class = "btn-outline-primary mb-3"),
          DT::dataTableOutput("raw_table")
        )
      ),
      bslib::nav_panel(
        "Source Code",
        bslib::card_body(
          shiny::h5("govhrcast on GitHub"),
          shiny::p(
            "The full source code for this simulation framework is available ",
            "on GitHub:"
          ),
          shiny::tags$p(
            shiny::tags$a(
              shiny::tags$code("WB-PIDA-Data-Science-Shop/govhrcast"),
              href   = "https://github.com/WB-PIDA-Data-Science-Shop/govhrcast",
              target = "_blank",
              style  = "font-size:1.1rem;"
            )
          ),
          shiny::p(
            style = "color:#555; font-size:0.9rem;",
            "Includes modules for retirement, hiring, promotions/transfers, ",
            "salary inflation, scenario matrix generation, and this dashboard."
          )
        )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# Sidebar builder
# ---------------------------------------------------------------------------

#' @keywords internal
.hz_sidebar <- function(flat_dt, lever_cols) {

  # Build one selectInput per lever column showing all unique values.
  # User picks one value per lever; the matching scenario is looked up server-side.
  lever_widgets <- if (length(lever_cols) > 0L) {
    lapply(lever_cols, function(lv) {
      vals   <- sort(unique(flat_dt[[lv]]))
      label  <- tools::toTitleCase(gsub("_", " ", lv))
      # Default to the baseline row's value if available
      def_val <- if ("is_baseline" %in% names(flat_dt) && any(flat_dt$is_baseline)) {
        flat_dt[is_baseline == TRUE, .SD[1L], .SDcols = lv][[1L]]
      } else {
        vals[1L]
      }
      shiny::selectInput(
        inputId  = paste0("lv_", lv),
        label    = label,
        choices  = vals,
        selected = def_val
      )
    })
  } else NULL

  bslib::sidebar(
    width = 300,
    shiny::h5(shiny::strong("Policy Levers")),
    shiny::p(
      class = "text-muted",
      style = "font-size:0.85rem;",
      "Select one value per lever, then click Show Results."
    ),
    lever_widgets,
    shiny::hr(),
    shiny::actionButton(
      "show_results",
      label = "Show Results",
      icon  = shiny::icon("play"),
      class = "btn-primary w-100"
    )
  )
}


# ---------------------------------------------------------------------------
# Public builder: assembles the complete page_navbar UI
# ---------------------------------------------------------------------------

#' Build the govhrcast dashboard UI
#'
#' @description
#' Assembles the complete \code{bslib::page_navbar()} UI object for the
#' govhrcast Shiny dashboard.  This is called once inside
#' \code{\link{generate_hrcastapp}} and the result is passed to
#' \code{shiny::shinyApp(ui = ...)}.
#'
#' @param flat_dt A \code{data.table} with scenario columns already present
#'   (use \code{\link{hz_ensure_scenario_cols}} first).
#' @param lever_cols Character vector of lever column names
#'   (from \code{\link{hz_lever_cols}}).
#' @param scenario_ch Named list of scenario choices
#'   (from \code{\link{hz_scenario_choices}}).
#'
#' @return A \code{shiny.tag} object (\code{page_navbar}).
#' @keywords internal
hz_build_ui <- function(flat_dt, lever_cols, scenario_ch) {
  theme <- hz_app_theme() |>
    bslib::bs_add_rules("
      /* Logo card header: no padding so image fills edge-to-edge */
      .card-header:has(img) { padding: 0 !important; overflow: hidden; }

      /* Navbar brand text */
      .navbar-brand { font-size: clamp(0.95rem, 1.5vw, 1.2rem) !important; }
    ")

  bslib::page_navbar(
    title          = shiny::strong("govhrcast"),
    theme          = theme,
    navbar_options = bslib::navbar_options(underline = TRUE),
    padding        = "20px",

    .hz_tab_intro(),
    .hz_tab_policy(flat_dt, lever_cols),
    if (nrow(flat_dt) > 1L) .hz_tab_comparator(flat_dt, lever_cols),
    .hz_tab_data()
  )
}
