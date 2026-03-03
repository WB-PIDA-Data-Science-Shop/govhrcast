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
              " Overlays two different policy paths to identify the
               fiscal gap between choices."
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
      width = 1 / 3,
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
          shiny::plotOutput("plot_fiscal", height = "480px")
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
          shiny::plotOutput("plot_spending", height = "560px")
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
          shiny::plotOutput("plot_turnover", height = "480px")
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
.hz_tab_comparator <- function() {
  bslib::nav_panel(
    title = "Scenario Comparator",
    value = "tab_compare",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::uiOutput("cmp_levers_a"),
        shiny::hr(),
        shiny::uiOutput("cmp_levers_b"),
        shiny::hr(),
        shiny::selectInput(
          "cmp_plot_type",
          "Chart type",
          choices = c(
            "Fiscal Basics"    = "fiscal_basics",
            "Spending Effects" = "spending_effects",
            "Turnover"         = "turnover"
          )
        )
      ),

      bslib::card(
        bslib::card_header("Overlay Comparison"),
        bslib::card_body(
          shiny::plotOutput("plot_compare", height = "500px")
        )
      ),

      bslib::layout_column_wrap(
        width = 1 / 3,
        bslib::card(
          bslib::card_header("Wage Bill Gap (Terminal Year)"),
          bslib::card_body(shiny::uiOutput("delta_wage_bill"))
        ),
        bslib::card(
          bslib::card_header("Pension Gap (Terminal Year)"),
          bslib::card_body(shiny::uiOutput("delta_pension"))
        ),
        bslib::card(
          bslib::card_header("Headcount Gap (Terminal Year)"),
          bslib::card_body(shiny::uiOutput("delta_headcount"))
        )
      )
    )
  )
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
    ")

  bslib::page_navbar(
    title          = shiny::strong("govhrcast"),
    theme          = theme,
    navbar_options = bslib::navbar_options(underline = TRUE),
    padding        = "20px",

    .hz_tab_intro(),
    .hz_tab_policy(flat_dt, lever_cols),
    .hz_tab_comparator(),
    .hz_tab_data()
  )
}
