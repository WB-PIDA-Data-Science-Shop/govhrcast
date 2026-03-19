# R/app_shinyserver.R
# =============================================================================
# Server logic for the govhrcast Shiny dashboard
# =============================================================================
# Exports:
#   hz_server(flat_dt, hz, scenario_ch, lever_cols)
#     Returns a standard Shiny server function(input, output, session){...}
#     ready to be passed to shiny::shinyApp(server = ...).
#
# Interaction model:
#   • Sidebar has one selectInput per lever column (prefix "lv_<lever>").
#   • An actionButton "show_results" triggers all reactive computations.
#   • active_dt is isolated behind bindEvent(input$show_results), so nothing
#     renders until the user clicks the button.
#   • Comparator uses two sets of lever selectors ("cmp_a_lv_*", "cmp_b_lv_*")
#     exposed via renderUI and also gated by show_results.
# =============================================================================


#' Build the govhrcast dashboard server function
#'
#' @param flat_dt A \code{data.table} of simulation results with scenario
#'   columns present (use \code{\link{hz_ensure_scenario_cols}} first).
#' @param hz A \code{horizon} S3 object for metadata.
#' @param scenario_ch Named list of scenario choices (from
#'   \code{\link{hz_scenario_choices}}).
#' @param lever_cols Character vector of lever column names.
#'
#' @return A Shiny server function \code{function(input, output, session)}.
#' @keywords internal
hz_server <- function(flat_dt, hz, scenario_ch, lever_cols) {

  function(input, output, session) {

    # ----------------------------------------------------------------
    # Static resource path for logo
    # ----------------------------------------------------------------
    www_path <- system.file("www", package = "govhrcast")
    if (nchar(www_path) > 0L) {
      shiny::addResourcePath("govhrcast_www", www_path)
    }

    # ----------------------------------------------------------------
    # Helper: match lever inputs to a scenario_id
    # ----------------------------------------------------------------
    # Given a named list of lever values, find the scenario_id whose row
    # in flat_dt has exactly those values for each lever column.
    .resolve_scenario <- function(lever_values) {
      if (length(lever_values) == 0L || nrow(flat_dt) == 0L) {
        return(flat_dt$scenario_id[1L])
      }
      mask <- rep(TRUE, nrow(flat_dt))
      for (lv in names(lever_values)) {
        if (!lv %in% names(flat_dt)) next
        raw_val <- lever_values[[lv]]
        # Coerce to the column's storage type
        col_class <- class(flat_dt[[lv]])[1L]
        typed_val <- tryCatch(
          switch(col_class,
            integer   = as.integer(raw_val),
            numeric   = as.numeric(raw_val),
            logical   = as.logical(raw_val),
            as.character(raw_val)
          ),
          error = function(e) raw_val
        )
        mask <- mask & (flat_dt[[lv]] == typed_val)
      }
      matched <- unique(flat_dt[mask, scenario_id])
      if (length(matched) == 0L) flat_dt$scenario_id[1L] else matched[1L]
    }

    # ----------------------------------------------------------------
    # Reactive: resolve active scenario from lever inputs
    # Gated by "Show Results" button via bindEvent.
    # ----------------------------------------------------------------
    active_sid <- shiny::reactive({
      if (length(lever_cols) == 0L) {
        sid <- input$lv_scenario_direct
        if (is.null(sid)) flat_dt$scenario_id[1L] else sid
      } else {
        lever_values <- stats::setNames(
          lapply(lever_cols, function(lv) input[[paste0("lv_", lv)]]),
          lever_cols
        )
        .resolve_scenario(lever_values)
      }
    }) |> shiny::bindEvent(input$show_results, ignoreNULL = FALSE)

    active_dt <- shiny::reactive({
      flat_dt[scenario_id == active_sid()]
    })

    active_hz <- shiny::reactive({
      new_horizon(comparison = active_dt(), metadata = hz$metadata)
    })

    terminal <- shiny::reactive({
      dt <- active_dt()
      if (nrow(dt) == 0L) return(dt)
      dt[which.max(period_date)]
    })

    # ----------------------------------------------------------------
    # KPI value boxes
    # ----------------------------------------------------------------
    output$kpi_wage_bill <- shiny::renderText({
      row <- terminal()
      if (nrow(row) == 0L || !"wage_bill_end" %in% names(row)) return("N/A")
      hz_fmt_big(row$wage_bill_end)
    })

    output$kpi_pension <- shiny::renderText({
      row <- terminal()
      if (nrow(row) == 0L || !"pension_cost_total" %in% names(row)) return("N/A")
      hz_fmt_big(row$pension_cost_total)
    })

    output$kpi_headcount <- shiny::renderText({
      row <- terminal()
      if (nrow(row) == 0L || !"n_headcount_end" %in% names(row)) return("N/A")
      scales::comma(row$n_headcount_end)
    })

    # ----------------------------------------------------------------
    # Policy Analysis plots — plotly output
    # ----------------------------------------------------------------
    .make_gg_reactive <- function(type) {
      shiny::reactive({
        hz_sub <- active_hz()
        tryCatch(plot(hz_sub, type = type), error = function(e) NULL)
      })
    }

    plot_fiscal_r   <- .make_gg_reactive("fiscal_basics")
    plot_spending_r <- .make_gg_reactive("spending_effects")
    plot_turnover_r <- .make_gg_reactive("turnover")

    # Render each panel individually so the UI can place them independently.
    # as.list(p) extracts panels in order from the patchwork composite.
    .render_panels <- function(plot_r, ids) {
      for (i in seq_along(ids)) {
        local({
          idx <- i
          id  <- ids[idx]
          output[[id]] <- plotly::renderPlotly({
            p <- plot_r()
            if (is.null(p)) return(plotly::plotly_empty())
            panels <- as.list(p)
            g <- if (idx <= length(panels)) panels[[idx]] else NULL
            if (!is.null(g)) suppressMessages(hz_to_plotly(g)) else plotly::plotly_empty()
          })
        })
      }
    }

    .render_panels(plot_fiscal_r,   c("plot_fiscal_1",   "plot_fiscal_2",   "plot_fiscal_3"))
    .render_panels(plot_spending_r, c("plot_spending_1", "plot_spending_2"))
    .render_panels(plot_turnover_r, c("plot_turnover_1", "plot_turnover_2"))

    # ----------------------------------------------------------------
    # "i" tooltip descriptions
    # ----------------------------------------------------------------
    .make_desc_ui <- function(plot_r) {
      shiny::renderUI({
        p    <- plot_r()
        desc <- if (!is.null(p)) attr(p, "description") else NULL
        if (is.null(desc)) return(shiny::p("No description available."))
        shiny::pre(style = "white-space:pre-wrap; font-size:0.85rem;", desc)
      })
    }
    output$desc_fiscal   <- .make_desc_ui(plot_fiscal_r)
    output$desc_spending <- .make_desc_ui(plot_spending_r)
    output$desc_turnover <- .make_desc_ui(plot_turnover_r)

    # ----------------------------------------------------------------
    # Data & Methodology tab — raw data table
    # ----------------------------------------------------------------
    output$raw_table <- DT::renderDataTable({
      sub <- active_dt()
      DT::datatable(
        sub,
        filter     = "top",
        rownames   = FALSE,
        extensions = "Buttons",
        options    = list(
          scrollX    = TRUE,
          pageLength = 10L,
          dom        = "Bfrtip",
          buttons    = list("copy", "csv", "excel")
        )
      )
    })

    output$dl_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("govhrcast_scenario_", active_sid(), "_",
               format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(active_dt(), file, row.names = FALSE)
      }
    )
  }
}

