# Hiring Module Implementation Log
**Package**: govhrcast v0.0.0.9000  
**Date**: February 24-26, 2026  
**Status**: ✅ Complete - All 518 tests passing

---

## Overview

Implemented a complete hiring module for the govhrcast package that handles:
- Flow-based hiring (replacement rates)
- Stock-based hiring (target headcount levels)
- Combined mode (flow + stock)
- Grouped hiring (by department, establishment, etc.)
- Downsizing scenarios
- Integration with retirement module

---

## Files Created/Modified

### New R Files
1. **R/hiring_core.R** (~600 lines)
   - Pure demand estimation functions (no state modification)
   - `compute_current_stock()` - Aggregates active personnel by group
   - `compute_flow_demand()` - Calculates replacement needs
   - `compute_stock_demand()` - Calculates gap to target levels
   - `compute_combined_demand()` - Merges flow + stock
   - `estimate_hiring_demand()` - Main wrapper with mode switching
   - `compute_hiring_summary()` - Summary statistics

2. **R/hiring_update.R** (~600 lines)
   - State modification functions
   - `generate_new_personnel()` - Creates personnel records with sequential IDs
   - `generate_new_contracts()` - Creates contract records
   - `assign_compensation()` - Joins salary scale (auto-detects columns)
   - `select_personnel_to_remove()` - Implements downsizing strategies
   - `update_state_with_adjustment()` - ONLY function that modifies state

3. **R/simulate_hiring.R** (~310 lines)
   - Main user-facing orchestrator
   - Follows same pattern as simulate_retirement
   - Copy-once-modify-by-reference approach
   - Handles panel data with ref_date_col
   - Returns: summary, contract_dt, personnel_dt, adjustment_dt, new_hires_dt

### Modified Files
4. **R/validation.R**
   - Added `check_hiring_inputs()` validation function
   - Modified `validate_date_format()` to accept both Date objects and strings

### Test Files
5. **tests/testthat/test-hiring_core.R** (51 tests)
6. **tests/testthat/test-hiring_update.R** (72 tests)
7. **tests/testthat/test-simulate_hiring.R** (37 tests)

### Example Files
8. **spielplatz/functiontesting/hiring.r**
   - Comprehensive examples using Brazil HRMIS data
   - 5 scenarios demonstrating all features

---

## Architecture & Design Decisions

### 1. **Copy-Once-Modify-By-Reference Pattern**
- Main function copies input data.tables once at start
- All modifications happen on copies
- Returns modified copies (originals unchanged)
- Matches retirement module pattern

### 2. **Flexible Column Naming**
- All functions accept column name parameters (e.g., `personnel_id_col`, `salary_col`)
- No hardcoded column names
- Enables use with different data schemas

### 3. **Three Policy Modes**

#### Flow Mode (Replacement-Based)
```r
policy_params <- list(
  mode = "flow",
  replacement_rate = 0.8,  # Can be scalar or data.table by group
  group_cols = NULL,
  salary_scale = salary_scale_dt
)
```

#### Stock Mode (Target-Based)
```r
policy_params <- list(
  mode = "stock",
  stock_targets = stock_targets_dt,
  group_cols = NULL,
  salary_scale = salary_scale_dt
)
```

#### Combined Mode
```r
policy_params <- list(
  mode = "combined",
  replacement_rate = replacement_rate_dt,
  stock_targets = stock_targets_dt,
  group_cols = "est_id",
  salary_scale = salary_scale_dt
)
```

### 4. **Grouping Strategy**
- `group_cols = NULL` → Overall hiring
- `group_cols = c("department")` → Hiring by department
- `group_cols = c("est_id", "paygrade")` → Hiring by establishment and grade
- All demand estimation and salary assignment respect grouping

### 5. **Downsizing Support**
- Negative net_change triggers downsizing
- Strategies: "last_hired_first" (default), "random"
- Marks personnel as "inactive" and contracts as "terminated"
- Does NOT delete rows (preserves history)

### 6. **Auto-Detection in assign_compensation()**
- `join_cols = NULL` → Auto-detects common columns (intersection)
- `salary_col = NULL` → Auto-detects first column matching "salary|wage|pay|compensation"
- Checks for duplicate keys to prevent cartesian joins
- Issues warning (not error) for unmatched contracts

---

## Key Functions

### compute_current_stock()
Aggregates active personnel at a reference date.

**Parameters**:
- `contract_dt`, `personnel_dt` - Input data
- `ref_date` - Reference date (Date object or string)
- `group_cols` - Grouping columns (NULL for overall)
- Column name parameters (personnel_id_col, start_date_col, etc.)

**Returns**: data.table with `current_stock` column (+ group columns if specified)

### estimate_hiring_demand()
Main demand estimation wrapper. Routes to flow/stock/combined based on mode.

**Parameters**:
- `contract_dt`, `personnel_dt` - Input data
- `policy_params` - Policy configuration (mode, rates, targets)
- `retirees_dt` - Optional retirees data (must be filtered to retiree == 1)
- `ref_date` - Reference date
- Column name parameters

**Returns**: data.table with `total_hires` column (renamed to `net_change` in simulate_hiring)

### simulate_hiring()
Main user-facing function. Orchestrates entire hiring simulation.

**Workflow**:
1. Validate inputs
2. Copy data once
3. Select nearest ref_date (if panel data)
4. Estimate hiring demand
5. Update state with adjustments
6. Compute summary statistics
7. Return results

**Returns**: List with:
- `summary` - Summary statistics (n_new_hires, net_headcount_change, etc.)
- `contract_dt` - Updated contract data
- `personnel_dt` - Updated personnel data
- `adjustment_dt` - Adjustment details by group
- `new_hires_dt` - New personnel records

---

## Integration with Retirement Module

### Pattern 1: Identify retirees, then hire replacements
```r
# Step 1: Identify who can retire
retirees_all <- identify_retirees(
  contract_dt, personnel_dt, 
  policy_params_retirement, ref_date
)

# Step 2: Filter to actual retirees
retirees_dt <- retirees_all[retiree == 1]

# Step 3: Hire replacements
hiring_results <- simulate_hiring(
  contract_dt, personnel_dt,
  policy_params_hiring,
  retirees_dt = retirees_dt,  # IMPORTANT: Pass filtered retirees
  ref_date
)
```

### Pattern 2: Process retirements, then hire
```r
# Step 1: Process retirements
retirement_results <- simulate_retirement(
  contract_dt, personnel_dt,
  policy_params_retirement, ref_date
)

# Step 2: Hire replacements using updated data
hiring_results <- simulate_hiring(
  contract_dt = retirement_results$contract_dt,
  personnel_dt = retirement_results$personnel_dt,
  policy_params_hiring,
  retirees_dt = retirement_results$retirees_dt[retiree == 1],
  ref_date
)
```

---

## Important Implementation Details

### 1. Date Handling
- `validate_date_format()` accepts both Date objects and strings
- Strings automatically converted to Date (e.g., "2016-09-01")
- Clear error messages for invalid date strings

### 2. State Modification
- `update_state_with_adjustment()` is the ONLY function that modifies state
- Returns modified contract_dt and personnel_dt in result list
- Caller must capture returned values: `result <- update_state_with_adjustment(...)`
- Then extract: `contract_dt <- result$contract_dt`, `personnel_dt <- result$personnel_dt`

### 3. Panel Data Handling
- `ref_date_col` parameter for panel data
- Automatically selects nearest ref_date not after specified date
- New hires get ref_date column filled with selected_ref_date
- Handles NA values in ref_date for new records

### 4. Salary Assignment
- Auto-detects join columns from intersection of column names
- Auto-detects salary column matching pattern
- Validates no duplicate keys (prevents cartesian join)
- Issues warning for unmatched contracts (continues simulation)

### 5. ID Generation
- Uses `generate_new_ids()` from utils.R
- Format: `P_YYYYMMDD_1`, `P_YYYYMMDD_2`, etc. for personnel
- Format: `C_YYYYMMDD_1`, `C_YYYYMMDD_2`, etc. for contracts
- Sequential within each simulation run

---

## Common Issues & Solutions

### Issue 1: Cartesian Join Error
**Error**: "Join results in X rows; more than Y = nrow(x)+nrow(i)"
**Cause**: Duplicate keys in salary_scale_dt
**Solution**: Ensure salary_scale_dt has unique combinations of join columns

### Issue 2: No Common Columns for Joining
**Error**: "No common columns found for joining"
**Cause**: salary_scale_dt only has salary column, no join keys
**Solution**: For overall hiring with single salary, pass 1-row salary_scale:
```r
salary_scale <- data.table(gross_salary_lcu = 5000)
```

### Issue 3: Wrong retirees_dt Format
**Error**: Too many hires or incorrect counts
**Cause**: Passing full identify_retirees() result (has retiree = 0 and 1)
**Solution**: Filter before passing:
```r
retirees_dt <- identify_retirees(...)
hiring_results <- simulate_hiring(..., retirees_dt = retirees_dt[retiree == 1])
```

### Issue 4: Personnel Count Not Changing
**Cause**: Test checking total row count, but downsizing marks inactive (doesn't delete)
**Solution**: Check active count: `personnel_dt[status == "active", .N]`

---

## Test Coverage

### Test Statistics
- **Total Tests**: 518 passing (0 failures, 0 warnings)
- **hiring_core**: 51 tests - Demand estimation logic
- **hiring_update**: 72 tests - State modification, generation, assignment
- **simulate_hiring**: 37 tests - Integration, workflows, edge cases

### Test Categories
1. **Unit Tests**: Individual function behavior
2. **Integration Tests**: Multi-function workflows
3. **Edge Cases**: Empty data, zero changes, negative demand
4. **Grouped vs Overall**: Different grouping scenarios
5. **Panel Data**: ref_date_col handling
6. **Validation**: Input checking, error messages

---

## Example Usage

See `spielplatz/functiontesting/hiring.r` for comprehensive examples:
1. Flow-based hiring (replacement rate)
2. Stock-based hiring (target levels)
3. Combined mode by establishment
4. Downsizing scenario
5. Integrated retirement + hiring workflow

---

## Future Enhancements (Not Implemented)

1. **Skill-based matching**: Match new hires to specific occupations/skills
2. **Cost-constrained hiring**: Hire within budget limits
3. **Multi-period simulation**: Sequential hiring over time

---

---

# Movements Module Implementation Log
**Package**: govhrcast v0.0.0.9000  
**Date**: February 26, 2026  
**Status**: ✅ Complete — 608 tests passing (0 failures, 1 intentional skip)

---

## Overview

Implemented a complete promotions and transfers module (internal labour mobility).
Handles:
- Baseline estimation from panel data (empirical transition matrices)
- Policy-multiplier-based demand projection
- Stochastic rounding of fractional mover counts
- Three promotion strategies: `tenure`, `wage_based`, `random`
- Three transfer strategies: `tenure`, `random`, `reverse_tenure`
- Multi-column group states (e.g., `c("est_id", "paygrade")`)
- Integration with retirement and hiring modules

---

## Files Created/Modified

### New R Files
1. **R/movement_core.R** (~565 lines)
   - Pure demand estimation (no state modification)
   - `compute_time_in_grade()` — years each person has been in current group state, using earliest panel snapshot where they appear in that state
   - `estimate_movement_baseline()` — transition probability matrix averaged across all consecutive panel periods; includes both movement (from≠to) and stay (from==to) rows
   - `compute_movement_demand()` — applies `promotion_multiplier` / `transfer_multiplier` to baseline probs, multiplies by current stock, returns integer mover counts via `stochastic_round()`
   - `compute_movement_summary()` — aggregates statistics (headcount before/after, promotions, transfers, historical rates)

2. **R/movement_update.R** (~465 lines)
   - State modification functions
   - `stochastic_round(x)` — `floor(x) + Bernoulli(x - floor(x))`, unbiased integer rounding
   - `identify_movers()` — selects exactly `n_movers` individuals per `from→to` pair; builds a global deduplicated set across all transitions to prevent double-selection
   - `update_state_with_movement()` — ONLY function that modifies state; updates `group_cols` values and salary in `contract_dt` for each mover; warns (does not error) on unmatched salary scale entries

3. **R/simulate_promotions_transfers.R** (~348 lines)
   - Main user-facing orchestrator
   - `simulate_promotions_transfers()` — validates inputs → estimates baseline from full panel → selects nearest ref_date snapshot → computes demand → identifies movers → updates state → computes summary
   - Passes through gracefully with message when only 1 snapshot available (no baseline)

### Modified Files
4. **R/validation.R**
   - Added `check_movement_inputs()` — validates contract_dt, personnel_dt, policy_params (group_cols, salary_scale, multipliers, strategies), ref_date
   - Fixed salary column regex: was `salary|wage|pay|compensation`; changed to `salary|wage|compensation|remuneration` to avoid false match on `paygrade`

### Test Files
5. **tests/testthat/test-movement_core.R** (30 tests, 1 skip)
6. **tests/testthat/test-movement_update.R** (33 tests)
7. **tests/testthat/test-simulate_promotions_transfers.R** (27 tests)

### Example File
8. **spielplatz/functiontesting/movements.r**
   - 5 scenarios + individual function demos using Brazil HRMIS data

---

## Architecture & Design Decisions

### 1. Transition Matrix Approach
Movement is modelled as a Markov transition:
- States defined by `group_cols` (concatenated with `||` as separator)
- Baseline matrix estimated from empirical panel data
- Policy multipliers scale the off-diagonal probabilities
- Diagonal ("no movement") absorbs the remaining probability mass

### 2. Stochastic Rounding
`demand * stock` is often fractional. Rather than always rounding down (bias) or always ceiling (over-hiring), we use stochastic rounding:

```r
stochastic_round <- function(x) {
  floor(x) + rbinom(1, 1, x - floor(x))
}
```

### 3. Movement Type Classification
- **Promotion**: transition where the destination group ranks higher in `salary_scale_dt` (higher median salary = higher grade)
- **Transfer**: all other off-diagonal transitions (same or lateral grade change)
- Classification is computed inside `compute_movement_demand()`

### 4. Selection Strategies

| Strategy | Applies to | Logic |
|---|---|---|
| `tenure` | promotions, transfers | Longest `start_date` age first (earliest start) |
| `wage_based` | promotions | Lowest salary ratio (`salary / max_salary_in_grade`) first |
| `random` | transfers | Random shuffle, no ranking |
| `reverse_tenure` | transfers | Shortest tenure first (latest `start_date`); implements LIFO |

### 5. No-Double-Selection Guarantee
`identify_movers()` maintains a `selected_ids` exclusion set updated after each `from→to` pair is processed. Subsequent pairs cannot select an already-moved person.

### 6. Copy-Once Pattern
Matches hiring and retirement modules:
```r
contract_dt  <- data.table::copy(contract_dt)
personnel_dt <- data.table::copy(personnel_dt)
```
Original inputs are never modified.

---

## Key Functions

### compute_time_in_grade(contract_dt, ref_date, group_cols, ...)
Calculates continuous time in current group state using panel history.

**Logic**: For each person, walks backwards through panel snapshots to find the earliest date they were already in their current state. Falls back to `start_date` for single-snapshot data.

**Returns**: `data.table(personnel_id, time_in_grade)` — time in years

### estimate_movement_baseline(contract_dt, group_cols, ...)
Estimates empirical transition matrix from full panel.

**Returns**: `data.table(from_group, to_group, avg_prob, n_periods)`

### compute_movement_demand(contract_dt, personnel_dt, baseline_matrix, policy_params, salary_scale_dt, ref_date, ...)
Applies multipliers and converts to integer counts.

**Returns**: `data.table(from_group, to_group, movement_type, adj_prob, current_stock, n_movers)`

### identify_movers(contract_dt, personnel_dt, demand_dt, policy_params, ref_date, ...)
Selects individual personnel per transition.

**Returns**: `data.table(personnel_id, from_group, to_group, movement_type)`

### update_state_with_movement(contract_dt, personnel_dt, movers_dt, policy_params, ref_date, ...)
Updates `group_cols` and salary for movers.

**Returns**: `list(contract_dt, personnel_dt, movers_dt)`

### simulate_promotions_transfers(contract_dt, personnel_dt, policy_params, ref_date, ...)
Full orchestration.

**Returns**: `list(summary, contract_dt, personnel_dt, movers_dt, baseline_matrix, demand_dt)`

---

## policy_params Structure

```r
policy_params <- list(
  group_cols           = c("est_id"),           # Required. State columns.
  salary_scale         = salary_scale_dt,        # Required. data.table keyed on group_cols.
  promotion_multiplier = 1.0,                    # Optional. Default = 1.0 (status quo)
  transfer_multiplier  = 1.0,                    # Optional. Default = 1.0
  promotion_strategy   = "tenure",               # Optional. "tenure"|"wage_based"|"random"
  transfer_strategy    = "random"                # Optional. "random"|"tenure"|"reverse_tenure"
)
```

---

## Integration with Other Modules

### Pattern: Retirement → Movements → Hiring
```r
# Step 1: Retire
ret <- simulate_retirement(contract_dt, personnel_dt, policy_retirement, ref_date)

# Step 2: Internal movements on post-retirement workforce
mov <- simulate_promotions_transfers(
  ret$contract_dt,
  ret$personnel_dt,
  policy_movements,
  ref_date = ref_date
)

# Step 3: Hire to backfill vacancies
hir <- simulate_hiring(
  mov$contract_dt,
  mov$personnel_dt,
  policy_hiring,
  retirees_dt = ret$retirees_dt[retire == 1],
  ref_date = ref_date
)
```

---

## Test Coverage

- **test-movement_core.R**: 30 pass, 1 skip — baseline estimation, time-in-grade, demand computation
- **test-movement_update.R**: 33 pass — stochastic rounding, mover selection (all strategies), state update
- **test-simulate_promotions_transfers.R**: 27 pass — integration, validation, single-snapshot graceful fallback

**Total package tests**: 608 passing, 0 failures, 1 skip

---

## Common Issues & Fixes Encountered

| Issue | Root Cause | Fix |
|---|---|---|
| `setorderv: column contract_id not found` | Test data missing `contract_id` column; `get_primary_contract()` requires it | Added `contract_id` to all test helpers |
| Broken `snap_contract_dt[char_vector]` without `on=` | Dead code block in `simulate_promotions_transfers.R` before the working version | Removed duplicate block |
| `paygrade` matched salary regex | Pattern `salary\|wage\|pay\|compensation`; "pay" ⊂ "paygrade" | Changed to `salary\|wage\|compensation\|remuneration` |
| `expect_message()` returns condition, not value | testthat 3.x: `expect_message` returns the condition object, not the expression value | Capture value via `result <- NULL; expect_message({ result <- f() }, ...)` |

---

## Example Usage

See `spielplatz/functiontesting/movements.r`:
- Part 1: Individual core functions (compute_time_in_grade, estimate_movement_baseline, compute_movement_demand)
- Part 2: Individual update functions (stochastic_round, identify_movers, update_state_with_movement)
- Part 3: Five full-orchestration scenarios (status quo, 2× promotions, no transfers, wage_based, retirement→movements chain)

---

## Package Dependencies

- data.table (core data manipulation)
- testthat (testing)
- devtools (development)
- Uses functions from retirement module: identify_retirees(), compute_age(), compute_tenure()

---

## Documentation

All functions have complete Roxygen documentation:
- 15 new .Rd files in man/ directory
- Examples in function documentation
- Parameter descriptions with defaults
- Return value specifications
- Keywords: internal for non-exported functions

---

## Performance Characteristics

- **Fast**: data.table operations throughout
- **Memory Efficient**: Copy-once pattern minimizes duplication
- **Scalable**: Tested with 1000+ personnel records
- **Vectorized**: No loops over individual records

---

## Completion Status

✅ All requirements from prompt implemented  
✅ All unit tests passing (518 tests)  
✅ Documentation complete  
✅ Integration with retirement module working  
✅ Example code demonstrating all features  
✅ User feedback incorporated (5 issues fixed)  

**Ready for production use.**
