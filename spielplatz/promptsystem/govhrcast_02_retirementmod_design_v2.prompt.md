````prompt

## Retirement Module - Implementation Guide

### Design Philosophy

**Data.table Optimization**: All functions use data.table operations for efficiency with large datasets (millions to hundreds of millions of observations). Avoid data.frame operations and base R `merge()`. Use data.table join syntax `DT1[DT2, on = ...]` instead.

**Flexible Column Naming**: No hardcoded column names anywhere. All functions accept column name parameters with sensible defaults matching govhr harmonized format. Parameters flow from top-level function down through the entire call chain.

**Modify by Reference**: Update functions modify data.tables in place using `:=` for memory efficiency. The main orchestrator (`simulate_retirement`) creates working copies at entry to protect user input, then all downstream functions modify by reference.

**Panel Data Handling**: Functions handle time-series/panel data with `ref_date` columns by selecting nearest snapshot before calculations using `select_nearest_ref_date()` utility.

**Full Outer Joins**: When merging datasets that may have non-overlapping keys, create a base table with all unique IDs first, then join both datasets to it to ensure no data loss.

---

### Inputs

- `contract_dt`: harmonized contracts (example format: `govhr::bra_hrmis_contract`)
- `personnel_dt`: harmonized personnel (example format: `govhr::bra_hrmis_personnel`)
- `policy_params`: List containing retirement policy parameters
- `ref_date`: Date for retirement simulation
- Column name parameters (all with defaults matching govhr format)

### Main Function Structure

```r
simulate_retirement(contract_dt, 
                   personnel_dt, 
                   policy_params,
                   ref_date,
                   # Column name parameters with defaults
                   ref_date_col = "ref_date",
                   personnel_id_col = "personnel_id",
                   birth_date_col = "birth_date",
                   contract_id_col = "contract_id",
                   start_date_col = "start_date",
                   end_date_col = "end_date",
                   salary_col = "gross_salary_lcu",
                   contract_type_col = "contract_type_code",
                   status_col = "status"){

  # 1. Input Validation - pass ALL column parameters
  check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date,
                         personnel_id_col, birth_date_col, contract_id_col,
                         start_date_col, end_date_col, contract_type_col, status_col)

  # 2. Create working copies ONCE at entry point
  # This protects user's input while allowing efficient in-place modifications downstream
  if (!data.table::is.data.table(contract_dt)) {
    contract_dt <- data.table::as.data.table(contract_dt)
  } else {
    contract_dt <- data.table::copy(contract_dt)
  }
  
  if (!data.table::is.data.table(personnel_dt)) {
    personnel_dt <- data.table::as.data.table(personnel_dt)
  } else {
    personnel_dt <- data.table::copy(personnel_dt)
  }

  # 3. Select nearest reference date (for panel data)
  # Subsets to single snapshot closest to but not after ref_date
  if (ref_date_col %in% names(contract_dt)) {
    selected_ref_date <- select_nearest_ref_date(contract_dt[[ref_date_col]], ref_date)
    contract_dt <- contract_dt[get(ref_date_col) == selected_ref_date]
    if (ref_date_col %in% names(personnel_dt)) {
      personnel_dt <- personnel_dt[get(ref_date_col) == selected_ref_date]
    }
  }

  # 4. Identify retirees - pass all relevant column parameters
  eligibility_dt <- identify_retirees(
    contract_dt, personnel_dt, policy_params, ref_date,
    personnel_id_col, birth_date_col, start_date_col,
    end_date_col, contract_type_col
  )

  # 5. Prepare retiree data - pass all relevant column parameters
  retirees_dt <- prepare_retiree_data(
    eligibility_dt, contract_dt, personnel_dt, ref_date,
    personnel_id_col, contract_id_col, start_date_col,
    end_date_col, salary_col, contract_type_col
  )

  # 6. Handle case of no retirees
  if (nrow(retirees_dt) == 0) {
    summary_tbl <- data.table::data.table(
      n_retired = 0L,
      total_pension = 0,
      avg_pension = NA_real_,
      avg_age = NA_real_,
      avg_tenure = NA_real_
    )
    
    return(list(
      summary = summary_tbl,
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      retirees_dt = data.table::data.table()
    ))
  }

  # 7. Compute pensions using switch() dispatcher
  retirees_dt[, pension := compute_pension(
    retirees_dt = .SD,
    policy_type = policy_params$pension_type,
    params = policy_params$pension_params
  )]

  # 8. Update state (modifies contract_dt and personnel_dt IN PLACE)
  # No assignment needed - functions return same object for chaining
  update_contracts_for_retirees(
    contract_dt, retirees_dt, ref_date,
    personnel_id_col, contract_id_col, start_date_col,
    end_date_col, salary_col, contract_type_col
  )
  
  update_personnel_for_retirees(
    personnel_dt, contract_dt,
    personnel_id_col, contract_type_col, status_col
  )

  # 9. Compute summary statistics using data.table aggregation
  summary_tbl <- compute_retirement_summary(retirees_dt, contract_dt)

  # 10. Return results
  return(list(
    summary = summary_tbl,
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    retirees_dt = retirees_dt
  ))
}
```

---

### Input Validation Functions

```r
check_retirement_inputs(contract_dt, 
                       personnel_dt, 
                       policy_params,
                       ref_date,
                       personnel_id_col = "personnel_id",
                       birth_date_col = "birth_date",
                       contract_id_col = "contract_id",
                       start_date_col = "start_date",
                       end_date_col = "end_date",
                       contract_type_col = "contract_type_code",
                       status_col = "status"){

  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")
  
  # Validate ref_date
  validate_date_format(ref_date, "ref_date")
  
  # Validate policy_params structure
  required_top <- c("eligibility_type", "pension_type")
  validate_required_params(policy_params, required_top, "retirement policy")
  
  # Validate eligibility_type
  valid_eligibility <- c("age_only", "tenure_only", "age_and_tenure")
  validate_choice(policy_params$eligibility_type, valid_eligibility, "eligibility_type")
  
  # Validate pension_type
  valid_pension <- c("db", "dc", "flat", "hybrid")
  validate_choice(policy_params$pension_type, valid_pension, "pension_type")
  
  # Validate eligibility parameters based on type
  if (policy_params$eligibility_type %in% c("age_only", "age_and_tenure")) {
    validate_positive_number(policy_params$min_age, "min_age")
    # Use parameter value, not hardcoded "birth_date"
    validate_column_exists(personnel_dt, birth_date_col, "personnel_dt")
  }
  
  if (policy_params$eligibility_type %in% c("tenure_only", "age_and_tenure")) {
    validate_positive_number(policy_params$min_tenure, "min_tenure")
  }
  
  # Check required columns using parameter values, not hardcoded names
  required_contract_cols <- c(contract_id_col, personnel_id_col, start_date_col,
                             end_date_col, contract_type_col)
  validate_columns_exist(contract_dt, required_contract_cols, "contract_dt")
  
  required_personnel_cols <- c(personnel_id_col, status_col)
  validate_columns_exist(personnel_dt, required_personnel_cols, "personnel_dt")
  
  return(invisible(TRUE))
}
```

---

### Core Computational Functions

#### Utility: Select Nearest Reference Date

```r
select_nearest_ref_date(x, ref_date){
  # For panel data: find closest date <= target date
  # Filters NA values before comparison
  unique_dates <- unique(x)
  unique_dates <- unique_dates[!is.na(unique_dates)]
  valid_dates <- unique_dates[unique_dates <= ref_date]
  
  if (length(valid_dates) == 0) {
    stop("No dates found on or before ", ref_date, call. = FALSE)
  }
  
  return(max(valid_dates))
}
```

#### Compute Years

```r
compute_years(start_date, end_date){
  # Vectorized calculation using 365.25 days per year to account for leap years
  days_diff <- as.numeric(difftime(end_date, start_date, units = "days"))
  years <- days_diff / 365.25
  return(years)
}
```

#### Compute Age

```r
compute_age(personnel_dt, 
           ref_date,
           birth_date_col = "birth_date",
           personnel_id_col = "personnel_id"){
  
  # Compute age directly without copying - return only ID and age
  # Uses dynamic column access via get()
  age_dt <- personnel_dt[, .(
    personnel_id = get(personnel_id_col),
    age = compute_years(start_date = get(birth_date_col), end_date = ref_date)
  )]
  
  return(age_dt)
}
```

#### Compute Tenure

**Critical Panel Data Handling:**
The harmonized HRMIS data contains panel observations where contracts appear in multiple rows with different `ref_date` values. This implementation correctly handles duplicates by:
1. Filtering inactive/pensioner contracts first
2. Deduplicating using `contract_id + start_date` as unique key (`.SD[1]`)
3. Keeping only essential columns after deduplication to avoid panel artifacts
4. Only counting contracts that started on/before the ref_date

```r
compute_tenure(contract_dt,
              ref_date,
              personnel_id_col = "personnel_id",
              contract_id_col = "contract_id",
              start_date_col = "start_date",
              end_date_col = "end_date",
              contract_type_col = "contract_type_code"){
  
  # 1. Filter out inactive contracts and pensioners (read-only operation)
  active_dt <- contract_dt[!get(contract_type_col) %in% c("inactive", "pensioner")]
  
  # 2. Deduplicate panel observations: one row per unique contract
  # Use contract_id + start_date as the unique key
  unique_contracts <- active_dt[, .SD[1], by = c(contract_id_col, start_date_col)]
  
  # 3. Keep only essential columns to avoid carrying forward panel ref_date
  keep_cols <- c(personnel_id_col, contract_id_col, start_date_col, end_date_col)
  unique_contracts <- unique_contracts[, ..keep_cols]
  
  # 4. Determine effective end date as of ref_date
  # If contract ended before ref_date, use end_date; otherwise use ref_date
  unique_contracts[, effective_end := fifelse(
    is.na(get(end_date_col)) | get(end_date_col) > ref_date,
    ref_date,
    get(end_date_col)
  )]
  
  # 5. Only count contracts that started on or before ref_date
  unique_contracts <- unique_contracts[get(start_date_col) <= ref_date]
  
  # 6. Calculate contract duration in days
  unique_contracts[, contract_days := as.numeric(
    difftime(effective_end, get(start_date_col), units = "days")
  )]
  
  # 7. Ensure non-negative durations
  unique_contracts[contract_days < 0, contract_days := 0]
  
  # 8. Sum total days per personnel using data.table aggregation
  tenure_dt <- unique_contracts[, 
    .(tenure_days = sum(contract_days, na.rm = TRUE)),
    by = .(personnel_id = get(personnel_id_col))
  ]
  
  # 9. Convert to years
  tenure_dt[, tenure_years := tenure_days / 365.25]
  
  return(tenure_dt)
}
```

#### Identify Retirees

Uses `switch()` for clean conditional logic based on eligibility type. Implements full outer join to preserve all personnel from both age and tenure calculations.

```r
identify_retirees(contract_dt,
                 personnel_dt,
                 policy_params,
                 ref_date,
                 personnel_id_col = "personnel_id",
                 birth_date_col = "birth_date",
                 start_date_col = "start_date",
                 end_date_col = "end_date",
                 contract_type_col = "contract_type_code"){
  
  eligibility_type <- policy_params$eligibility_type
  
  # Compute age if needed
  if (eligibility_type %in% c("age_only", "age_and_tenure")) {
    age_dt <- compute_age(personnel_dt, ref_date, birth_date_col, personnel_id_col)
  } else {
    # Placeholder with NA age for tenure-only eligibility
    age_dt <- data.table::data.table(
      personnel_id = unique(personnel_dt[[personnel_id_col]]),
      age = NA_real_
    )
  }
  
  # Compute tenure if needed
  if (eligibility_type %in% c("tenure_only", "age_and_tenure")) {
    tenure_dt <- compute_tenure(contract_dt, ref_date, personnel_id_col,
                               start_date_col, end_date_col, contract_type_col)
  } else {
    # Placeholder with NA tenure for age-only eligibility
    tenure_dt <- data.table::data.table(
      personnel_id = unique(contract_dt[[personnel_id_col]]),
      tenure_years = NA_real_,
      tenure_days = NA_real_
    )
  }
  
  # Full outer join: merge age and tenure preserving all personnel
  # Rename to use parameter column names
  data.table::setnames(age_dt, "personnel_id", personnel_id_col)
  data.table::setnames(tenure_dt, "personnel_id", personnel_id_col)
  
  # Create base table with all unique personnel_ids
  all_ids <- unique(c(age_dt[[personnel_id_col]], tenure_dt[[personnel_id_col]]))
  eligibility_dt <- data.table::data.table(id = all_ids)
  data.table::setnames(eligibility_dt, "id", personnel_id_col)
  
  # Join age and tenure to base table
  eligibility_dt <- age_dt[eligibility_dt, on = personnel_id_col]
  eligibility_dt <- tenure_dt[eligibility_dt, on = personnel_id_col]
  
  # Determine eligibility using switch() for clean conditional logic
  eligibility_dt[, retire := switch(
    eligibility_type,
    "age_only" = as.integer(age >= policy_params$min_age),
    "tenure_only" = as.integer(tenure_years >= policy_params$min_tenure),
    "age_and_tenure" = as.integer(
      age >= policy_params$min_age & 
      tenure_years >= policy_params$min_tenure
    ),
    stop("Unknown eligibility type: ", eligibility_type, call. = FALSE)
  )]
  
  # Handle NA values (set to 0 - not eligible)
  eligibility_dt[is.na(retire), retire := 0L]
  
  # Return with standardized "personnel_id" column name
  result <- eligibility_dt[, c(personnel_id_col, "retire", "age", "tenure_years"), with = FALSE]
  data.table::setnames(result, personnel_id_col, "personnel_id")
  
  return(result)
}
```

#### Prepare Retiree Data

```r
prepare_retiree_data(eligibility_dt,
                    contract_dt,
                    personnel_dt,
                    ref_date,
                    personnel_id_col = "personnel_id",
                    contract_id_col = "contract_id",
                    start_date_col = "start_date",
                    end_date_col = "end_date",
                    salary_col = "gross_salary_lcu",
                    contract_type_col = "contract_type_code"){
  
  # Filter to eligible retirees only
  retirees_only <- eligibility_dt[retire == 1]
  
  # Early return if no retirees
  if (nrow(retirees_only) == 0) {
    return(data.table::data.table())
  }
  
  # Get active contracts
  active_contracts <- get_active_contracts(
    contract_dt, ref_date,
    start_date_col, end_date_col, contract_type_col
  )
  
  # Filter to retirees only
  retiree_contracts <- active_contracts[
    get(personnel_id_col) %in% retirees_only$personnel_id
  ]
  
  # Get primary contract for each retiree
  primary_contracts <- get_primary_contract(
    retiree_contracts,
    personnel_id_col, contract_id_col, start_date_col, salary_col
  )
  
  # Join with eligibility data using data.table syntax
  # Note: both return standardized "personnel_id" column
  retirees_dt <- primary_contracts[
    retirees_only[, .(personnel_id, age, tenure_years)], 
    on = "personnel_id"
  ]
  
  return(retirees_dt)
}
```

#### Helper: Get Active Contracts

```r
get_active_contracts(contract_dt,
                    ref_date,
                    start_date_col = "start_date",
                    end_date_col = "end_date",
                    contract_type_col = "contract_type_code"){
  
  # Filter: started before/on ref_date AND (no end date OR ended after ref_date) AND not inactive
  active <- contract_dt[
    get(start_date_col) <= ref_date &
    (is.na(get(end_date_col)) | get(end_date_col) >= ref_date) &
    get(contract_type_col) != "inactive"
  ]
  
  return(active)
}
```

#### Helper: Get Primary Contract

```r
get_primary_contract(contract_dt,
                    personnel_id_col = "personnel_id",
                    contract_id_col = "contract_id",
                    start_date_col = "start_date",
                    salary_col = "gross_salary_lcu"){
  
  # Order by priority: start_date DESC, salary DESC, contract_id ASC
  data.table::setorderv(
    contract_dt,
    cols = c(start_date_col, salary_col, contract_id_col),
    order = c(-1, -1, 1)  # DESC, DESC, ASC
  )
  
  # Take first row per personnel (highest priority)
  # Returns standardized "personnel_id" column
  primary <- contract_dt[, .SD[1], by = .(personnel_id = get(personnel_id_col))]
  
  return(primary)
}
```

---

### State Update Functions

**Key Design Pattern**: These functions modify input data.tables IN PLACE using `:=` operator. They return the same object for convenience, but assignment is not necessary. The main orchestrator creates working copies once at entry, then all downstream modifications are by reference for efficiency.

#### Update Contracts for Retirees

Implements vectorized primary contract selection using `frank()` for ranking without loops.

```r
update_contracts_for_retirees(contract_dt,
                              retirees_dt,
                              ref_date,
                              personnel_id_col = "personnel_id",
                              contract_id_col = "contract_id",
                              start_date_col = "start_date",
                              end_date_col = "end_date",
                              salary_col = "gross_salary_lcu",
                              contract_type_col = "contract_type_code"){
  
  # Early return if no retirees
  if (nrow(retirees_dt) == 0) {
    return(contract_dt)
  }
  
  # Extract retiree IDs
  retiree_ids <- unique(retirees_dt[[personnel_id_col]])
  
  # Create retire flag in contract_dt (in place modification)
  contract_dt[, retire := fifelse(get(personnel_id_col) %in% retiree_ids, 1L, 0L)]
  
  # Identify active contracts
  contract_dt[, active_flag := fifelse(
    is.na(get(end_date_col)) & get(contract_type_col) != "inactive",
    1L,
    0L
  )]
  
  # Filter to active contracts of retirees (creates view, not copy)
  retiree_active <- contract_dt[retire == 1L & active_flag == 1L]
  
  # Early return if no active contracts to update
  if (nrow(retiree_active) == 0) {
    contract_dt[, c("retire", "active_flag") := NULL]
    return(contract_dt)
  }
  
  # Rank contracts within each personnel by priority using frank()
  # Priority: start_date DESC, salary DESC, contract_id ASC
  retiree_active[, priority_rank := frank(
    list(-as.numeric(get(start_date_col)), 
         -get(salary_col), 
         get(contract_id_col)),
    ties.method = "first"
  ), by = .(personnel_id = get(personnel_id_col))]
  
  # Get primary contract IDs (rank = 1)
  primary_contract_ids <- retiree_active[priority_rank == 1][[contract_id_col]]
  
  # Update primary contracts (in place)
  contract_dt[
    get(contract_id_col) %in% primary_contract_ids & retire == 1L & active_flag == 1L,
    c(contract_type_col, end_date_col) := list("pensioner", ref_date)
  ]
  
  # Get non-primary contract IDs
  non_primary_contract_ids <- retiree_active[priority_rank > 1][[contract_id_col]]
  
  # Update non-primary contracts (in place)
  if (length(non_primary_contract_ids) > 0) {
    contract_dt[
      get(contract_id_col) %in% non_primary_contract_ids & retire == 1L & active_flag == 1L,
      c(contract_type_col, end_date_col) := list("closed_due_to_retirement", ref_date)
    ]
  }
  
  # Clean temporary columns (in place)
  contract_dt[, c("retire", "active_flag") := NULL]
  
  return(contract_dt)
}
```

#### Update Personnel for Retirees

```r
update_personnel_for_retirees(personnel_dt,
                              contract_dt,
                              personnel_id_col = "personnel_id",
                              contract_type_col = "contract_type_code",
                              status_col = "status"){
  
  # Get unique personnel_ids with pensioner contracts
  pensioner_ids <- unique(
    contract_dt[get(contract_type_col) == "pensioner"][[personnel_id_col]]
  )
  
  # Update status for pensioners (in place)
  if (length(pensioner_ids) > 0) {
    personnel_dt[
      get(personnel_id_col) %in% pensioner_ids, 
      (status_col) := "inactive"
    ]
  }
  
  return(personnel_dt)
}
```

---

### Summary Statistics

Uses data.table aggregation syntax for maximum efficiency with large datasets.

```r
compute_retirement_summary(retirees_dt, contract_dt = NULL){
  
  # Handle case of no retirees
  if (nrow(retirees_dt) == 0) {
    summary_tbl <- data.table::data.table(
      n_retired = 0L,
      total_pension = 0,
      avg_pension = NA_real_,
      avg_age = NA_real_,
      avg_tenure = NA_real_
    )
    return(summary_tbl)
  }
  
  # Compute all statistics in single data.table operation for efficiency
  # .N is more efficient than nrow() for data.tables
  summary_tbl <- retirees_dt[, .(
    n_retired = .N,
    total_pension = sum(pension, na.rm = TRUE),
    avg_pension = mean(pension, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE),
    avg_tenure = mean(tenure_years, na.rm = TRUE)
  )]
  
  return(summary_tbl)
}
```

---

### Pension Computation Functions

All pension functions use switch() for clean conditional logic. See implementation in retirement_pension.R for full details.

#### Main Pension Dispatcher

```r
compute_pension(retirees_dt, policy_type, params){
  
  # Use switch() to select computation method
  pension <- switch(
    policy_type,
    "db" = compute_db_pension(retirees_dt, params),
    "dc" = compute_dc_pension(retirees_dt, params),
    "flat" = compute_flat_pension(retirees_dt, params),
    "hybrid" = compute_hybrid_pension(retirees_dt, params),
    stop("Unknown pension policy type: ", policy_type, call. = FALSE)
  )
  
  return(pension)
}
```

#### Defined-Benefit Pension (DB)

```r
compute_db_pension(dt, params){
  
  # Validate required parameters
  required <- c("accrual_rate", "ref_wage_col")
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    stop("Missing DB pension parameters: ", paste(missing, collapse = ", "), 
         call. = FALSE)
  }
  
  # Extract reference wage using parameter
  ref_wage <- dt[[params$ref_wage_col]]
  
  # Apply service cap if specified
  if (!is.null(params$max_years)) {
    years <- pmin(dt$tenure_years, params$max_years)
  } else {
    years <- dt$tenure_years
  }
  
  # Calculate gross pension
  gross_pension <- params$accrual_rate * years * ref_wage
  
  # Apply replacement cap if specified
  if (!is.null(params$replacement_cap)) {
    max_allowed <- params$replacement_cap * ref_wage
    pension <- pmin(gross_pension, max_allowed)
  } else {
    pension <- gross_pension
  }
  
  return(pension)
}
```

**Example Parameters:**

```r
policy_params <- list(
  eligibility_type = "age_and_tenure",
  min_age = 60,
  min_tenure = 20,
  pension_type = "db",
  pension_params = list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu",
    max_years = 35,
    replacement_cap = 0.80
  )
)
```

---

### Key Lessons for Next Module (Hiring)

1. **Start with column parameters**: Define all column name parameters upfront with defaults
2. **Copy once at entry**: Create working copies in main orchestrator, then modify by reference
3. **Use data.table throughout**: No `merge()`, no data.frame operations
4. **Full outer joins**: Create base table with all IDs first when joining non-overlapping datasets
5. **Dynamic column access**: Use `get(column_name)` or `[[column_name]]` throughout
6. **Test modify-by-reference**: Write tests that verify in-place modification behavior
7. **Panel data**: Always consider time-series structure and implement deduplication
8. **Efficient aggregation**: Use `[, .(col = func()), by = ...]` pattern
9. **Switch over if/else**: Cleaner conditional logic
10. **Vectorized operations**: Use `frank()`, `fifelse()`, etc. instead of loops

---

### Outputs

- `summary`: data.table with summary statistics (n_retired, total_pension, avg_pension, avg_age, avg_tenure)
- `contract_dt`: updated contracts with retirees marked as "pensioner" or "closed_due_to_retirement"
- `personnel_dt`: updated personnel with retirees marked as "inactive"
- `retirees_dt`: data.table of retirees with pension amounts and demographics

---

✅ **End of Retirement Module Implementation Guide**

````
