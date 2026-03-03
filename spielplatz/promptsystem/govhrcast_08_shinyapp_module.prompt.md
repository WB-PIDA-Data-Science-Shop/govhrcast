# .prompt_shiny.md

**Role:** Senior R Shiny Developer & UI/UX Designer.
**Context:** Developing `generate_hrcastapp()`. This app mirrors the **CPIA App** structure, providing a professional landing page followed by three analytical and data-driven tabs.

## 1. Core Objective
Implement `generate_hrcastapp(horizon_obj)`. This function takes a `horizon` S3 object (produced by `generate_scenario_matrix()`) and deploys a CPIA-styled dashboard for workforce and fiscal policy exploration.

## 2. UI Structure (CPIA Blueprint)
- **Framework:** `bslib::page_sidebar()` with a professional "Gov-Tech" theme.
- **Sidebar:** Contains dynamic filters (dropdowns/sliders) mapped to the `horizon_obj$metadata`. 
- **Dynamic Filtering:** Use `observeEvent` to ensure users can only select parameter combinations that exist in the simulation matrix.
- **Navigation:** Use `bslib::navset_bar()` for the following four tabs.

## 3. Tab 1: Introduction (Landing Page)
Replicate the CPIA style using Markdown. You can find the cpia folder here (C:\Users\wb559885\OneDrive - WBG\Documents\GitProjects\cpiaapp) The content should be:
- **Header:** Center the `govhrcast` banner image.
- **Text Body:** "This dashboard provides a data-driven approach to simulate the long-term fiscal impact of workforce policies (Retirement, Hiring, and Inflation).
  
  #### Purpose
  This tool assists fiscal and HR specialists in evaluating wage bill sustainability. It synthesizes current contract data with policy levers to deliver:
  - **Evidence-based projections** for wage bill and pension growth.
  - **Transparent decomposition** of spending drivers (Hiring vs. Promotions).
  - **Comparative context** across different retirement and COLA scenarios.
  
  **Note: These projections are data-driven simulations based on the 'Simple Sum' accounting logic. They are intended to support professional judgment in fiscal planning rather than provide definitive budgetary guarantees.**
  
  #### Dashboard Structure
  1. **Policy Analysis:** Visualizes core fiscal trends, spending drivers, and turnover for a selected scenario.
  2. **Scenario Comparator:** Overlays two different policy paths to identify the fiscal 'gap' between choices.
  3. **Data & Methodology:** Provides the raw simulation tables and links to the open-source logic."

## 4. Tab 2: Policy Analysis (Analytical Hub)
- **KPI Row:** Three `bslib::value_box` components at the top showing Terminal Year figures for the active scenario: `wage_bill_end`, `pension_cost_total`, and `n_headcount_end`.
- **Sub-Navigation:** Use `bslib::navset_card_pill()` to toggle between:
    - **Fiscal Basics:** `plot(filtered_horizon, type = "fiscal_basics")`
    - **Spending Effects:** `plot(filtered_horizon, type = "spending_effects")`
    - **Turnover Dynamics:** `plot(filtered_horizon, type = "turnover")`
- **The "i" Tooltip:** Extract `attr(p, "description")` and display it via `bslib::popover` to define all variables.

## 5. Tab 3: Scenario Comparator
- **Selection:** Two distinct sets of inputs (Scenario A vs. Scenario B).
- **Visualization:** Call the S3 `plot()` methods on the subset containing both scenarios to overlay the results.
- **Delta Analysis:** A summary card showing the fiscal difference (absolute and %) between the two paths by the final year.

## 6. Tab 4: Data & Methodology
- **Raw Data:** A searchable `DT::datatable` of the filtered `comparison` table with a CSV download button.
- **Code:** Direct links to the GitHub repository modules (Retirement, Hiring, etc.).
- **Metadata Viewer:** Display the `policy_args` from the object to document the simulation grid.

## 7. Technical Specifications
- **S3 Dispatch:** The app must use `plot(x, type = "...")` for all visuals.
- **Performance:** No simulation logic runs in the app; it is a "Result Browser" for pre-computed data.
- **Function Wrap:** Ensure the app is launched via `generate_hrcastapp(horizon_obj)`.