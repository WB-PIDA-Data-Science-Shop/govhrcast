
## Retirement Module

### Inputs

- `contract_dt`: harmonized contracts (example format: `govhr::bra_hrmis_contract`)
- `personnel_dt`: harmonized personnel (example format: `govhr::bra_hrmis_personnel`)
- Retirement policy parameters (pension system, eligibility rules)

### Main Function

```
simulate_retirement(contract_dt, personnel_dt, policy_params){

  # 1. Check inputs
  check_retirement_inputs(contract_dt, personnel_dt)

  # 2. Identify retirees according to policy
  retirees_dt <- identify_retirees(contract_dt, personnel_dt, policy_params)

  # 3. Compute pension payments
  retirees_dt[, pension := compute_pension(policy_type = policy_params$type, 
                                           params = policy_params$params)]

  # 4. Update state
  updated_contract_dt <- update_contracts_for_retirees(contract_dt, retirees_dt)
  updated_personnel_dt <- update_personnel_for_retirees(personnel_dt, retirees_dt)

  # 5. Compute summary statistics
  summary_tbl <- compute_retirement_summary(retirees_dt)

  # 6. Return updated state and summary
  return(list(
    summary = summary_tbl,
    contract_dt = updated_contract_dt,
    personnel_dt = updated_personnel_dt
  ))
}

Please note that for the rest of this planning whenever ... is used in the function call, 
it doesnt necessarily mean the arguments should be the ellipsis. It is just saying there 
are more arguments which you can figure out. 


```

---

### Input Validation Functions

```
check_retirement_inputs(contract_dt, personnel_dt, ...){

  # Ensures that:
  # - All required columns exist
  # - Data types are correct
  # - policy rules are properly specified

}

Previous validation functions already developed can be found here: C:\Users\wb559885\OneDrive - WBG\Documents\GitProjects\govhrsim\R\validation.R


```

---

### Core Computational Functions

#### Compute Years

```
compute_years(start_date, end_date){

  # Computes the time elapsed in years between two vectors
  # Can be used to compute age or tenure
  # Returns a numeric vector: years_value

}
```

#### Compute Age

```
compute_age(personnel_dt, birth_date, ref_date){

  # Uses compute_years to calculate age of each personnel
  # Returns numeric vector of ages

}
```

#### Compute Tenure

**Important Note on Panel Data:**
The harmonized HRMIS data (e.g., `govhr::bra_hrmis_contract`) contains panel/time-series observations where the same contract may appear in multiple rows with different `ref_date` values. When computing tenure, you must handle this by:
1. Deduplicating contracts using unique `contract_id` values, OR
2. Using `contract_id + start_date` combinations to identify unique employment spells

The current implementation sums all rows, which overcounts tenure when panel data is present.

```
compute_tenure(contract_dt,
               ref_date,
               personnel_id_col,
               start_date_col,
               end_date_col,
               contract_type_col){
  
  # 1. Remove duplicates: one row per unique contract
  unique_contracts <- contract_dt[, .SD[1], by = .(contract_id, start_date)]
  
  # 2. Filter out inactive contracts and pensioners
  active_contracts <- unique_contracts[
    !get(contract_type_col) %in% c("inactive", "pensioner")
  ]
  
  # 3. For ongoing contracts (end_date is NA), set end_date to ref_date
  active_contracts[is.na(get(end_date_col)), 
                   (end_date_col) := ref_date]
  
  # 4. Calculate contract duration in days
  active_contracts[, contract_days := as.numeric(
    difftime(get(end_date_col), get(start_date_col), units = "days")
  )]
  
  # 5. Sum total days per personnel
  tenure_dt <- active_contracts[, 
    .(tenure_days = sum(contract_days, na.rm = TRUE)),
    by = .(personnel_id = get(personnel_id_col))
  ]
  
  # 6. Convert to years
  tenure_dt[, tenure_years := tenure_days / 365.25]
  
  return(tenure_dt)
}
```

#### Identifying retirees

```
identify_retirees(contract_dt, personnel_dt, policy_params, ref_date){

  ### function uses compute_tenure and/or compute_age as the case maybe depending on policy param
  ### specified to figure out who is eligible for retirement (could use the switch function to 
  ### keep code clean instead of the if statements)

  ### creates an additional column retire (1 if retirement eligible and 0 otherwise). All eligible
  ### will be retired (we may include a model of actual retirement given eligibility later)

  ### return data.table with personnel_ids and their retire status

}


update_contracts_for_retirees(contract_dt,
                              retirees_dt,
                              ref_date,
                              start_date,
                              contract_id){

  # 1. Merge retire results of identify_retirees data.table 
  # into the contract_dt using fast data.table join
  

  # 2. Identify active contracts
  contract_dt[, active_flag :=
      fifelse(is.na(end_date) &
              contract_type_code != "inactive",
              1, 0)]


FOR EACH personnel_id: ### please dont use for loops for this, think more efficient data.table implementation

    Let A = active contracts for this person

    IF A is empty:
        continue

    Let primary_contract =
        argmax over A of
            (start_date DESC,
             salary DESC,
             contract_id ASC)

    Mark primary_contract as:
        contract_type_code = "pensioner"
        end_date = ref_date

    Mark all other contracts in A as:
        contract_type_code = "closed_due_to_retirement"
        end_date = ref_date

  # 4. Clean
  contract_dt[, c("retire","active_flag") := NULL]

  return(contract_dt)
}


update_personnel_for_retirees(contract_dt, personnel_dt, ...){

  ### contract_dt is now the result of update_contracts_for_retirees() previously called

  ### get the set of unique personnel_ids and contract_type_code from contract_dt

  ### merge into personnel_dt and set all "pensioner" values in contract_type_code
  ### into "inactive" within the status variable
  personnel_dt[contract_type_code == "pensioner", status := "inactive]

  return(personnel_dt)

}

```

---

### Pension Computation Functions (Pseudocode)

#### Main Pension Dispatcher

```
compute_pension(retiree_dt, policy_type, params){

  # Use switch() to select computation method
  pension <- switch(policy_type,
    "db"     = compute_db_pension(retiree_dt, params),
    "dc"     = compute_dc_pension(retiree_dt, params),
    "flat"   = compute_flat_pension(retiree_dt, params),
    "hybrid" = compute_hybrid_pension(retiree_dt, params),
    stop("Unknown pension policy type.")
  )

  return(pension)
}
```

---

#### Defined-Benefit Pension (DB)

```
compute_db_pension(dt, params){

  # Reference wage selection based on params$ref_type
  ref_wage <- switch(params$ref_type,
                     "final"         = dt[["final_salary"]],
                     "final_average" = rowMeans(dt[, params$ref_cols]),
                     "career_average"= dt[["career_avg_salary"]],
                     stop("Unknown reference salary type.")
  )

  # Apply service cap
  years <- pmin(dt[["years_service"]], params$max_years)

  # Gross pension
  gross <- params$accrual_rate * years * ref_wage

  # Apply replacement cap
  max_allowed <- params$replacement_cap * ref_wage
  pension <- pmin(gross, max_allowed)

  return(pension)
}
```

**Example Parameters:**

```
params <- list(
  accrual_rate = 0.02,
  ref_type = "final_average",
  ref_cols = c("salary_t_1","salary_t_2","salary_t_3"),
  max_years = 35,
  replacement_cap = 0.80
)
```

---

#### Defined-Contribution Pension (DC)

```
compute_dc_pension(dt, params){

  balance <- switch(params$type,
                    "DC"  = dt[["balance"]],
                    "NDC" = dt[["balance"]] * (1 + params$notional_rate),
                    stop("Unknown DC type.")
  )

  pension <- balance / params$annuity_factor
  return(pension)
}
```

**Example Parameters:**

```
params <- list(
  type = "NDC",
  notional_rate = 0.015,
  annuity_factor = 18
)
```

---

#### Flat Pension

```
compute_flat_pension(dt, params){

  # Assign the same flat pension amount to all retirees
  pension <- rep.int(params$flat_amount, nrow(dt))
  return(pension)
}
```

**Example Parameters:**

```
params <- list(
  flat_amount = 15000
)
```

---

#### Hybrid Pension

```
compute_hybrid_pension(dt, params){

  db_part <- compute_db_pension(dt, params$db_params)
  dc_part <- compute_dc_pension(dt, params$dc_params)

  pension <- db_part + dc_part
  return(pension)
}
```

**Example Parameters:**

```
params <- list(
  db_params = list(...),
  dc_params = list(...)
)
```

---

### Outputs

- `summary`: summary table (# retired, total pension bill)
- `contract_dt`: updated contracts with retirees removed/flagged
- `personnel_dt`: updated personnel with retirees flagged

---

✅ **End of Retirement Module Prompt**

