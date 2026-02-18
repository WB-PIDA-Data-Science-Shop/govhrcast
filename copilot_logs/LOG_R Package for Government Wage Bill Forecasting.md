# Task Log: R Package for Government Wage Bill Forecasting

**Task Name:** R Package for Government Wage Bill Forecasting

**Description:** Develop a system of functions that allows World Bank teams to simulate and project the public sector wage bill using contract and personnel payroll data

**Task Initialized:** 2026-02-18 10:38:15 AM EST

---

## Initial Context

### Workspace Structure
- R package: `govhrcast`
- Contains `DESCRIPTION`, `NAMESPACE`, `govhrcast.Rproj`
- R code directory: `R/`
- Working notes in `spielplatz/` directory

### Available Data (from session context)
- `govhr::bra_hrmis_contract` - Contract-level data (8,885 rows, 23 columns)
  - Key variables: contract_id, personnel_id, est_id, ref_date, salary fields (base_salary_lcu, allowance_lcu, gross_salary_lcu, net_salary_lcu), whours, dates (start_date, end_date), contract_type_code
  - Contract types: "fterm", "perm", "temp", "inactive", "pensioner"
  
- `govhr::bra_hrmis_personnel` - Personnel-level data
  - Status values: "active", "inactive"

### Geographic Coverage
- Country: Brazil (BRA)
- State: Alagoas (AL)

---

## Interaction Log

### Session Start
- User initiated task following DICE protocol
- Task tracking files created

### Design Documents Reviewed
- **Framework Overview** (`govhrcast_01_simulation_framework.prompt.md`)
  - Modular simulation system with 4 core modules: Retirement, Hiring, Promotion, Transfer
  - Stock-flow identity enforcement at each time step
  - Uses `data.table` for performance
  - Compatible with `govhr` harmonized data format
  - Time-stepped simulation (year/month/day intervals)

- **Retirement Module Design** (`govhrcast_02_retirementmod_design.prompt.md`)
  - Main function: `simulate_retirement()`
  - Core functions: `compute_years()`, `compute_age()`, `compute_tenure()`, `identify_retirees()`
  - Update functions: `update_contracts_for_retirees()`, `update_personnel_for_retirees()`
  - Pension systems: DB (defined-benefit), DC (defined-contribution), Flat, Hybrid
  - Eligibility rules: age_only, tenure_only, age_and_tenure
  - Primary contract selection logic for multi-contract personnel

### Implementation Completed (2026-02-18)

**Files Created:**
1. `R/utils.R` - Core utility functions
   - `compute_years()`: Vectorized date difference calculation
   - `compute_age()`: Age calculation from birth date
   - `compute_tenure()`: Tenure calculation from contract history
   - `get_active_contracts()`: Filter active contracts at ref_date
   - `get_primary_contract()`: Select primary contract for multi-contract personnel
   - Helper functions for ID generation, empty events, date formatting

2. `R/validation.R` - Input validation functions
   - `validate_datatable()`, `validate_column_exists()`, `validate_columns_exist()`
   - `validate_date_format()`, `validate_positive_number()`, `validate_number_range()`
   - `validate_character_string()`, `validate_choice()`, `validate_required_params()`
   - `check_retirement_inputs()`: Comprehensive retirement module validation

3. `R/retirement_core.R` - Core retirement logic
   - `identify_retirees()`: Determine eligibility based on age/tenure with switch()
   - `compute_retirement_summary()`: Aggregate statistics
   - `prepare_retiree_data()`: Enrich retirees with salary/contract info

4. `R/retirement_pension.R` - Pension calculations
   - `compute_pension()`: Main dispatcher using switch()
   - `compute_db_pension()`: Defined-benefit pension (accrual rate × years × wage)
   - `compute_dc_pension()`: Defined-contribution (balance / annuity factor)
   - `compute_flat_pension()`: Uniform flat amount
   - `compute_hybrid_pension()`: DB + DC combined

5. `R/retirement_update.R` - State updates
   - `update_contracts_for_retirees()`: Mark contracts as "pensioner" or "closed_due_to_retirement"
   - `update_personnel_for_retirees()`: Set personnel status to "inactive"
   - Uses data.table operations without loops for efficiency

6. `R/simulate_retirement.R` - Main orchestrator
   - `simulate_retirement()`: User-facing function coordinating full workflow
   - Returns list with: summary, updated contract_dt, updated personnel_dt, retirees_dt

**Design Decisions:**
- **Tenure Calculation:** Sum all contract periods per personnel (handles cumulative service)
- **Flexible Column Names:** All functions accept column name parameters with defaults
- **Policy Parameters:** Structured list format allowing extensibility
- **data.table Operations:** Efficient vectorized operations, no loops
- **Salary Prioritization:** User-specifiable via `salary_col` parameter (default: gross_salary_lcu)

**Testing Results:**
- Package builds successfully with `devtools::document()` and `devtools::load_all()`
- Test with Brazil data (govhr::bra_hrmis_contract/personnel) executed
- 558 retirees identified with 15-year tenure threshold
- Total pension bill: 433,174 LCU

**Known Issues / To Do:**
1. **Panel Data Handling:** Current tenure calculation treats each observation row as separate contract
   - Brazil data has multiple rows per contract (time series observations at different ref_dates)
   - Need to deduplicate or restructure tenure calculation to handle panel structure
   - Consider using unique contract_id + start_date combinations

2. **Missing Data:** Some contracts have NA salary values causing pension calculation issues
   - Need to decide how to handle: exclude, impute, or flag as errors

3. **Date Validation:** birth_date column empty in Brazil data - age-based eligibility untested

4. **Documentation:** Roxygen documentation generated but needs review for completeness
