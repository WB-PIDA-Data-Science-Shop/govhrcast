# Module Specification: Promotions and Transfers (govhrcast_04)

## Context & Objective
Build a "Promotions and Transfers" module for the `govhrcast` R package. This module simulates internal labor movements and must integrate seamlessly with the existing **retirement** and **hiring** modules. The core logic transitions from estimating "status quo" empirical patterns to applying policy-driven interventions.

## Design Philosophy
* **Data-Driven Baseline**: Use multi-period averages from panel data to establish a stable transition matrix, avoiding anomalies from single-year snapshots.
* **Performance**: Use `data.table` for all data manipulation and joins.
* **State Management**: Follow the "copy-once-modify-by-reference" pattern. Only `update_state_with_movement` modifies data, and it must return the modified copies.
* **Validation**: Personnel cannot be moved into organizations or paygrades not defined in the current `salary_scale`.

---

## 1. Estimation: `estimate_movement_baseline()`
Analyze longitudinal/panel data to calculate empirical transition probabilities.

* **Logic**:
    * Compare snapshots ($T_0 \to T_1, T_1 \to T_2$, etc.) across all available periods in a long-format `contract_dt`.
    * Calculate the average probability $P_{i,j}$ for moving from State $i$ (org/grade) to State $j$.
    * Equation: $P_{i,j} = \frac{\sum \text{Movements}_{i \to j}}{\sum \text{Total Population in State } i \text{ at start of each period}}$.
* **Parameters**: `contract_dt` (long format), `group_cols` (e.g., `c("est_id", "paygrade")`).
* **Output**: A transition matrix `data.table` with columns: `from_group`, `to_group`, and `avg_prob`.

## 2. Selection Engine: `identify_movers()`
Once the expected number of moves is determined ($N = \text{Current Stock} \times P_{adjusted}$), select specific individuals. Use stochastic rounding for fractional $N$.

| Movement Type | Strategy | Ranking / Selection Logic |
| :--- | :--- | :--- |
| **Promotions** | `tenure` | Rank by `time_in_grade` descending; select top $N$. |
| | `wage_based` | Rank by `salary` relative to `max_salary` of current grade; select top $N$. |
| **Transfers** | `random` | Stochastic selection using `sample()`. |
| | `tenure` | Rank by `total_tenure` descending; select top $N$. |
| | `reverse_tenure` | Rank by `total_tenure` ascending (LIFO); select top $N$. |

---

## 3. Policy Parameters & Input
The `policy_params` list must support:
* `promotion_multiplier`: A scalar to adjust the baseline empirical probabilities.
* `transfer_strategy`: One of `random`, `tenure`, `reverse_tenure`.
* `promotion_strategy`: One of `tenure`, `wage_based`.
* **Constraint**: If adjusted probabilities for a group exceed 1.0, they must be scaled back proportionally.

## 4. Required Functions to Implement

### `compute_movement_demand()` (Pure Function)
1.  Join current `contract_dt` with the `baseline_matrix`.
2.  Apply policy multipliers and strategy filters.
3.  **NA Scrubbing**: Filter out any `to_group` targets not present in the current `salary_scale`. Redistribute that probability to "No Movement."
4.  Calculate the final integer count of movers per group.

### `update_state_with_movement()` (State Modifier)
1.  Accept the list of `movers` identified by the selection engine.
2.  **Update `contract_dt`**: Change `org_id` and `paygrade_id` for selected `personnel_id`s.
3.  **Update `salary`**: Re-assign compensation based on the new grade using the `salary_scale_dt`.
4.  Return a list containing the updated `contract_dt` and `personnel_dt`.

---

## Technical Constraints
* **No Hardcoding**: All column names (IDs, salaries, dates) must be parameterized.
* **Join Safety**: Implement duplicate key checks in transition matrices to prevent cartesian joins.
* **Date Handling**: Ensure compatibility with both `Date` objects and ISO date strings.

## 5. Summary Statistics
Compute summary statistics to show promotions and transfers both the historical rates and statistics
to show staff counts before and after the transitions. 

