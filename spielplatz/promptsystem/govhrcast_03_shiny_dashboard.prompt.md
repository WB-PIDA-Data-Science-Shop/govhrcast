````prompt
# govhrcast: Shiny Dashboard – Design & Implementation Prompt

## Overview

The `generate_hrcastapp()` function launches a `bslib`-based Shiny dashboard
for exploring pre-computed govhrcast simulation results. The app is a **result
browser only** — no simulation runs inside it. All computation happens
beforehand via `simulate_horizon()` or `generate_scenario_matrix()`.

The app accepts two input types:

| Input type | Source function | Mode |
|---|---|---|
| `horizon` S3 object | `simulate_horizon()` | Single-scenario, no levers |
| `data.table` | `generate_scenario_matrix()` | Multi-scenario, lever-mode |

Both are normalised inside `generate_hrcastapp()` to a `horizon` object + a
`flat_dt` (the flat long data.table), so all downstream code can treat them
identically.

---

## File Structure

```
R/
  generate_hrcastapp.R   # Thin orchestrator: normalises input, launches shinyApp()
  app_shinyui.R          # All UI builders: hz_build_ui() + internal tab functions
  app_shinyserver.R      # Server function: hz_server()
  app_shinyutils.R       # Pure helpers: hz_fmt_big(), hz_to_plotly(), hz_app_theme(), etc.
  plot_horizon.R         # S3 plot.horizon() with 3 chart types
```

---

## Input Normalisation (`generate_hrcastapp.R`)

```r
generate_hrcastapp <- function(horizon_obj, ...) {
  if (data.table::is.data.table(horizon_obj)) {
    hz      <- hz_dt_to_horizon(horizon_obj)
    flat_dt <- data.table::copy(horizon_obj)
  } else if (inherits(horizon_obj, "horizon")) {
    hz      <- horizon_obj
    flat_dt <- data.table::copy(horizon_obj$comparison)
  }
  hz_ensure_scenario_cols(flat_dt)   # adds scenario_id/label/is_baseline if missing
  lever_cols  <- hz_lever_cols(flat_dt)
  scenario_ch <- hz_scenario_choices(flat_dt)
  ui     <- hz_build_ui(flat_dt, lever_cols, scenario_ch)
  server <- hz_server(flat_dt, hz, scenario_ch, lever_cols)
  shiny::shinyApp(ui = ui, server = server, ...)
}
```

The **two operating modes** that flow from this:

- **Lever-mode** (`length(lever_cols) > 0`): `flat_dt` comes from
  `generate_scenario_matrix()`. Each unique combination of lever values is a
  scenario. The sidebar shows one `selectInput` per lever column (prefix
  `lv_<lever>`). The server resolves the selected lever values to a
  `scenario_id` via exact column matching.
- **Label-mode** (`length(lever_cols) == 0`): `flat_dt` comes from a `horizon`
  or a `named_scenarios_flat`-style table with no lever columns. The sidebar
  shows a single `selectInput` with scenario labels as choices
  (`input$lv_scenario_direct`).

---

## Utility Functions (`app_shinyutils.R`)

### `.HZ_RESERVED_COLS`

A character vector of all time-series output column names that are **not**
lever/parameter columns. Used by `hz_lever_cols()` to identify which columns
are user-adjustable policy levers.

```r
.HZ_RESERVED_COLS <- c(
  "scenario_id", "scenario_label", "is_baseline", "period_date",
  "n_headcount_start", "n_headcount_end",
  "wage_bill_start", "wage_bill_end",
  "n_exits", "exit_savings",
  "n_non_ret_exits", "non_ret_exit_savings",
  "pension_cost_new", "pension_cost_total",
  "n_promotions", "n_transfers",
  "promotion_effect", "transfer_effect",
  "n_hires", "hiring_effect", "inflation_effect",
  "exit_savings_pct_of_end_bill",
  "non_ret_exit_savings_pct_of_end_bill",
  "promotion_effect_pct_of_end_bill",
  "transfer_effect_pct_of_end_bill",
  "hiring_effect_pct_of_end_bill",
  "inflation_effect_pct_of_end_bill"
)
```

**Critical**: any new output column added to `simulate_horizon()` or
`generate_scenario_matrix()` must also be added here, otherwise it will be
mistakenly identified as a policy lever in the app sidebar.

### `hz_lever_cols(dt)`
Returns `setdiff(names(dt), .HZ_RESERVED_COLS)`.

### `hz_ensure_scenario_cols(dt)`
Adds `scenario_id = 1L`, `scenario_label = "Simulation"`, `is_baseline = TRUE`
in-place if those columns are missing (single-scenario `horizon` case).

### `hz_scenario_choices(dt)`
Returns a named list `scenario_label -> scenario_id` for `selectInput`.

### `hz_terminal_row(dt, sid)`
Returns the single row of `dt` with the latest `period_date` for `scenario_id == sid`.

### `hz_fmt_big(x)`
Compact number formatting: `1.2e6 -> "1.2M"`, using `scales::cut_short_scale()`.

### `hz_delta_card_html(val_a, val_b, label_a, label_b)`
Renders a `shiny::tagList` showing A value, B value, coloured delta arrow
(red = cost increase, green = saving), and percentage change. Returns `p("N/A")`
if either value is NA/NULL.

### `hz_app_theme()`
Returns a `bslib::bs_theme(bootswatch = "litera")` with:
- Google Fonts: Source Sans Pro (body/code), Fira Sans (headings)
- `bs_add_rules()` for responsive CSS:
  - `h3`–`h6`: `clamp()` font sizes
  - `.value-box-title` / `.value-box-value`: `clamp()` font sizes
  - `.bslib-value-box`: `min-height: 100px`
  - `.nav-pills .nav-link`: `clamp()` font size
  - `.tab-pane { height: 100%; }` (supports internal scroll divs)

### `hz_to_plotly(p, tooltip)`

Converts a **single** `ggplot2` object (not a patchwork composite) to a
`plotly` figure. Key implementation details:

1. Opens a temporary PDF device before calling `ggplotly()` — required because
   `ggplotly()` needs an open graphics device to compute geometry in server
   contexts (no screen device available).
2. Collects all axis keys (`xaxis`, `xaxis2`, etc.) from the built plotly
   object to handle faceted plots correctly — then applies tick font (size 11,
   Fira Sans) to **all** axes, not just the primary ones.
3. Places the legend **below** the plot: `y = -0.2, yanchor = "top",
   orientation = "h"`, with `margin = list(b = 80)` to prevent overlap with
   x-axis date labels.

```r
hz_to_plotly <- function(p, tooltip = c("x","y","colour","fill","linetype","label")) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp, width = 10, height = 5)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  .tick_font <- list(size = 11, family = "Fira Sans, sans-serif")
  tryCatch({
    pl        <- plotly::ggplotly(p, tooltip = tooltip)
    axis_keys <- names(pl$x$layout)[grepl("^[xy]axis", names(pl$x$layout))]
    axis_updates <- stats::setNames(
      lapply(axis_keys, function(k) list(tickfont = .tick_font, title = list(font = .tick_font))),
      axis_keys
    )
    legend_cfg <- list(orientation="h", x=0.5, y=-0.2, xanchor="center", yanchor="top",
                       font=list(size=11, family="Fira Sans, sans-serif"))
    do.call(plotly::layout, c(list(pl, legend=legend_cfg, margin=list(b=80)), axis_updates))
  }, error = function(e) plotly::plotly_empty())
}
```

---

## UI Architecture (`app_shinyui.R`)

### `hz_build_ui(flat_dt, lever_cols, scenario_ch)`

Assembles `bslib::page_navbar()` with:
- `hz_app_theme()` plus two extra CSS rules (logo card header padding,
  navbar brand font size)
- Tabs in order:
  1. `tab_intro` — always shown
  2. `tab_policy` — always shown
  3. `tab_compare` — **only shown when `nrow(flat_dt) > 1`**
  4. `tab_data` — always shown

```r
bslib::page_navbar(
  title = shiny::strong("govhrcast"),
  theme = theme,
  .hz_tab_intro(),
  .hz_tab_policy(flat_dt, lever_cols),
  if (nrow(flat_dt) > 1L) .hz_tab_comparator(flat_dt, lever_cols),
  .hz_tab_data()
)
```

### Tab 1: Introduction (`.hz_tab_intro()`)
Static landing page card with logo image, purpose text, and a numbered list of
the dashboard tabs.

### Tab 2: Policy Analysis (`.hz_tab_policy(flat_dt, lever_cols)`)

`layout_sidebar()` with `.hz_sidebar(flat_dt, lever_cols)` on the left.

Main body structure:

```
layout_column_wrap(width = "250px")      # KPI row — 3 value_box, stacks on small screens
  value_box("Terminal Wage Bill",  kpi_wage_bill,  theme="primary")
  value_box("Cumul. Pension Liab", kpi_pension,    theme="secondary")
  value_box("Terminal Headcount",  kpi_headcount,  theme="success")

navset_card_pill                          # 3 chart sub-tabs
  nav_panel("Fiscal Basics")
    div(style="overflow-y:auto; max-height:85vh;")
      layout_column_wrap(width=1/3)
        plotlyOutput("plot_fiscal_1", height="420px")
        plotlyOutput("plot_fiscal_2", height="420px")
        plotlyOutput("plot_fiscal_3", height="420px")

  nav_panel("Spending Effects")
    div(style="overflow-y:auto; max-height:85vh;")
      plotlyOutput("plot_spending_1", height="650px")
      [1.5rem gap]
      plotlyOutput("plot_spending_2", height="380px")

  nav_panel("Turnover Dynamics")
    div(style="overflow-y:auto; max-height:85vh;")
      layout_column_wrap(width=1/2)
        plotlyOutput("plot_turnover_1", height="450px")
        plotlyOutput("plot_turnover_2", height="450px")
```

Each sub-tab also has a `popover(bs_icon("info-circle"), uiOutput("desc_*"))`
for variable definitions.

### Tab 3: Scenario Comparator (`.hz_tab_comparator(flat_dt, lever_cols)`)

Only rendered when `nrow(flat_dt) > 1`.

**Sidebar (width=320):**
Two `bslib::card()` panels — blue header "Scenario A", green header
"Scenario B" — visually separated with distinct `bg-primary` / `bg-success`
card headers.

- **Lever-mode**: one `selectInput` per lever column per scenario slot.
  Inputs prefixed `cmp_a_lv_<lever>` and `cmp_b_lv_<lever>`.
  Scenario A defaults to baseline row values; Scenario B to first non-baseline.
- **Label-mode**: single `selectInput` per slot (`cmp_a_sid`, `cmp_b_sid`)
  using `hz_scenario_choices()`. A defaults to baseline scenario_id, B to
  second unique scenario_id.
- "Compare" `actionButton` with id `cmp_show_results`.

**Main body:**

```
layout_column_wrap(width="250px")        # 3 delta comparison tiles
  card(card_header("Wage Bill"),    card_body(uiOutput("diff_wage_bill")))
  card(card_header("Pension"),      card_body(uiOutput("diff_pension")))
  card(card_header("Headcount"),    card_body(uiOutput("diff_headcount")))

navset_card_pill                          # same 3 sub-tabs as Policy Analysis
  nav_panel("Fiscal Basics")
    div(style="overflow-y:auto; max-height:85vh;")
      layout_column_wrap(width=1/3)
        plotlyOutput("cmp_fiscal_1", height="420px")
        plotlyOutput("cmp_fiscal_2", height="420px")
        plotlyOutput("cmp_fiscal_3", height="420px")

  nav_panel("Spending Effects")
    # Wide horizontal scroll — dodged bars need breathing room
    div(style="overflow-x:auto; overflow-y:auto; max-height:85vh;")
      div(style="min-width:1400px;")
        plotlyOutput("cmp_spending_1", height="650px")
        [1.5rem gap]
        plotlyOutput("cmp_spending_2", height="380px")

  nav_panel("Turnover Dynamics")
    div(style="overflow-y:auto; max-height:85vh;")
      layout_column_wrap(width=1/2)
        plotlyOutput("cmp_turnover_1", height="450px")
        plotlyOutput("cmp_turnover_2", height="450px")
```

### Tab 4: Data & Methodology (`.hz_tab_data()`)

`navset_card_pill` with "Raw Data" (DT::dataTableOutput with CSV download) and
"Source Code" (GitHub link).

### Sidebar (`.hz_sidebar(flat_dt, lever_cols)`)

- **Lever-mode**: one `selectInput(paste0("lv_", lv), ...)` per lever column.
  Default to baseline row value if `is_baseline` column present.
- **Label-mode**: no inputs (Policy Analysis tab will use `input$lv_scenario_direct`
  which is injected by `renderUI` in the server).
- `actionButton("show_results", "Show Results")` at the bottom.

---

## Server Architecture (`app_shinyserver.R`)

### Policy Analysis reactives

```
active_sid  <- reactive({ resolve lever inputs -> scenario_id })
             |> bindEvent(input$show_results, ignoreNULL = FALSE)

active_dt   <- reactive({ flat_dt[scenario_id == active_sid()] })
active_hz   <- reactive({ new_horizon(comparison = active_dt(), metadata = hz$metadata) })
terminal    <- reactive({ active_dt()[which.max(period_date)] })
```

**`.resolve_scenario(lever_values)`** — internal helper. Takes a named list of
lever values, matches each against `flat_dt` columns with type coercion
(integer/numeric/logical/character), returns the first matching `scenario_id`.
Returns `flat_dt$scenario_id[1L]` as fallback.

### Plot rendering pattern

All plots go through a two-step pipeline:

1. **`plot_r <- .make_gg_reactive(type)`** — creates a `reactive({ plot(active_hz(), type=type) })`
2. **`.render_panels(plot_r, ids)`** — for each panel index `i`, registers
   `output[[ids[i]]] <- renderPlotly({...})` using `as.list(patchwork_obj)[[i]]`
   to extract individual panels, then `hz_to_plotly()` to convert.

`as.list()` on a patchwork object extracts individual panels in order.
**Do not** use `c(list(p), p$patches$plots)` — that incorrectly includes the
full composite as the first element.

### Comparator reactives

All inside `if (nrow(flat_dt) > 1L) { ... }`.

```
cmp_sid_a  <- reactive({ resolve cmp_a_lv_* inputs -> scenario_id })
             |> bindEvent(input$cmp_show_results, ignoreNULL = FALSE)

cmp_sid_b  <- reactive({ resolve cmp_b_lv_* inputs -> scenario_id })
             |> bindEvent(input$cmp_show_results, ignoreNULL = FALSE)

compare_dt <- reactive({
  sub <- flat_dt[scenario_id %in% c(cmp_sid_a(), cmp_sid_b())]
  # join scenario_label
  sub[scenario_id == cmp_sid_a(), scenario := "Scenario A"]
  sub[scenario_id == cmp_sid_b(), scenario := "Scenario B"]
  sub
})

compare_hz <- reactive({ new_horizon(comparison = compare_dt(), metadata = hz$metadata) })
```

Comparator plots use `plot(compare_hz(), type=type, scenario_col="scenario")`.
The `scenario` column (values: "Scenario A"/"Scenario B") is used for
colour/group aesthetics.

**`.render_cmp_panels(plot_r, ids)`** — same pattern as `.render_panels()` but
also does trace relabelling: after `hz_to_plotly()`, iterates `pl$x$data`,
matches trace `$name` against scenario IDs via substring search, renames to
"Scenario A"/"Scenario B".

### Delta card outputs

```r
.cmp_terminal <- function(sid, col) {
  sub <- compare_dt()
  row <- sub[scenario_id == sid][which.max(period_date)]
  row[[col]]
}
.make_diff_output("diff_wage_bill",  "wage_bill_end")
.make_diff_output("diff_pension",    "pension_cost_total")
.make_diff_output("diff_headcount",  "n_headcount_end")
# Each calls hz_delta_card_html(val_a, val_b, label_a, label_b)
```

---

## Plot Architecture (`plot_horizon.R`)

`plot.horizon(x, type, scenario_col = "scenario", ...)` dispatches to three
internal functions. Each returns a `patchwork` composite with a `description`
attribute (variable definitions for tooltip display).

### `type = "fiscal_basics"` — 3 panels (p1 + p2 + p3)
- p1: Wage bill line chart (`wage_bill_end` vs `period_date`)
- p2: Pension liability — two lines (`pension_cost_total` and `pension_cost_new`),
  coloured by `pension_type`; when `scenario_col` is present, group =
  `interaction(scenario_col, pension_type)` to avoid crossed lines
- p3: Inflation/COLA effect line chart (`inflation_effect`)
- All three use `patchwork::plot_layout(guides = "collect")` with
  `legend.position = "bottom"`

### `type = "spending_effects"` — 2 panels (stacked)
- p1 (tall): Bar chart of spending driver decomposition. Columns:
  `hiring_effect`, `promotion_effect`, `transfer_effect`, `inflation_effect`,
  faceted by `effect_label`, `fill = fill_dir` (cost=blue, saving=red),
  `position = "dodge"`, `width = 200`. When `scenario_col` present:
  `colour = scenario_col` for second-dimension differentiation.
- p2 (short): Efficiency line — `exit_savings_pct_of_end_bill` as % of wage bill
- Layout: `p_abs / p_eff` with `heights = c(3, 1)`

### `type = "turnover"` — 2 panels (stacked)
- p1: Flows — `n_hires` and `n_exits` as lines, coloured by `metric_label`,
  group = `interaction(scenario_col, metric_label)` when scenario_col present
- p2: Stock — `n_headcount_end` line, coloured by `scenario_col` when present

### Colour palette (`.hz_palette()`)
```r
list(
  positive  = "#2166AC",  # blue  — costs
  negative  = "#D6604D",  # red   — savings
  headcount = "#4DAC26",  # green — workforce stock
  neutral   = "#878787",  # grey  — secondary
  accent    = "#F4A582"   # peach — new flows
)
```

### Theme (`.hz_theme()`)
`theme_minimal(base_size=13)` with:
- Strip text: bold, size 13
- Legend: bottom
- Plot title: bold, size 15; subtitle: bold, size 12, grey40
- Axis title: bold, size 12; axis text: size 11, x-axis angle 45

### `scenario_col` contract
When `scenario_col` is present in `dt`, all plots use `colour = .data[[scenario_col]]`
and/or `group = interaction(...)` for correct multi-scenario overlays.
When absent (single scenario), `colour = NULL` and `group = 1` so no spurious
legend appears.

---

## Known Implementation Pitfalls

1. **`ggplotly()` needs an open graphics device** — always wrap in a temp PDF
   device or it will error in Shiny server (no screen device).

2. **`as.list(patchwork_obj)`** extracts panels correctly. `c(list(p), p$patches$plots)`
   is wrong — it includes the full composite as element 1.

3. **`scenario_id` is character in label-mode** — never coerce with
   `as.integer()` in the server when using `named_scenarios_flat`-style input.
   The type must match whatever `flat_dt$scenario_id` contains.

4. **Reactive context errors** — never call a reactive (e.g. `compare_dt()`)
   outside a reactive context at server init time. If passing a reactive's
   value to a non-reactive function, wrap it in a lambda:
   `relabel_sids = function() unique(compare_dt()$scenario_id)`.

5. **Non-ASCII characters** — all R source files must use only ASCII. Use
   `\uxxxx` escapes in R string literals, but **not** in roxygen comment text
   (Rd processing does not support `\u` escapes). Use plain ASCII alternatives
   (`--`, `->`) in roxygen.

6. **`.HZ_RESERVED_COLS` completeness** — if a new output column is added to
   `generate_scenario_matrix()`, it must be added to this vector or the app
   sidebar will show it as a policy lever.

---

## Testing Strategy

- **Pure helpers** (`hz_fmt_big`, `hz_lever_cols`, `hz_delta_card_html`, etc.)
  → `testthat` in `tests/testthat/test-generate_hrcastapp.R`. Fast, no browser.
- **Server reactive logic** (`compare_dt`, `cmp_sid_a/b`, delta outputs)
  → `shiny::testServer()`. No browser required, ships with Shiny.
- **Full UI/browser flows** → `shinytest2` (optional, requires Chrome, use only
  for critical end-to-end paths like "click Compare, verify plot appears").

```r
# Example testServer pattern for comparator
shiny::testServer(hz_server(flat_dt, hz, scenario_ch, lever_cols), {
  session$setInputs(
    show_results     = 1L,
    cmp_show_results = 1L,
    cmp_a_lv_salary_growth_rate = 0.03,
    cmp_b_lv_salary_growth_rate = 0.05
  )
  expect_equal(cmp_sid_a(), 1L)
  expect_setequal(unique(compare_dt()$scenario), c("Scenario A", "Scenario B"))
})
```

````
