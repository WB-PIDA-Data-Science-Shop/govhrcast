## DEFERRED / FUTURE WORK

### Phase 7a — Richer new-hire characteristics (hiring module extension)

**Motivation:**
Currently `generate_new_personnel()` creates bare-bones synthetic individuals —
only an ID, `ref_date`, and any group values passed via `group_vals`. New hires
have no education level, no paygrade, and therefore no matched salary from
`salary_scale_grade`. This means all backfill hires in flow mode get `NA`
salary unless an est-level scale is used. The fix is to assign realistic
sociodemographic and grade characteristics to new hires at the point of
creation.

**Scope:**

1. **Education assignment (`educat7`)**
   - In `flow` mode, derive the empirical distribution of `educat7` among
     *outgoing* (retiring / exiting) workers in the same `group_cols` group
     during the current period.
   - Draw `n` education levels from that empirical distribution (with
     replacement) and assign to new hires.
   - Fallback: if the outgoing pool for a group is empty or has no `educat7`,
     use the distribution from the full active workforce in that group, then
     the global distribution.

2. **Paygrade assignment**
   - Controlled by a new policy parameter `paygrade_rule` in `hire_policy`:
     - `"merit"`: rank new hires by their drawn `educat7` percentile; assign
       paygrades by matching percentile rank in the empirical paygrade
       distribution of the outgoing pool (10th‑pct education → 10th‑pct
       paygrade). Uses `educat7` ordered factor levels already defined in the
       package.
     - `"bottom_grade"`: all new hires assigned the lowest paygrade in their
       group (simplest assumption — entry-level hiring).
     - `"sample"`: draw paygrade from the empirical distribution of the
       outgoing pool, independent of education (current implicit behaviour,
       made explicit).
     - `"fixed"`: a named vector `paygrade_fixed = c(group_key = "G1")` in
       `hire_policy` overrides per-group.
   - Default: `"bottom_grade"` (least disruptive to existing tests).

3. **Salary resolution after paygrade assignment**
   - Once a new hire has `(est_id, paygrade)`, `assign_compensation()` can
     join to `salary_scale_grade` and resolve a real salary — eliminating the
     `NA` salary / `-Inf` wage bill problem entirely for the common case.

**Files to touch:**
- `R/hiring_core.R` — `generate_new_personnel()`: add `educat7` draw logic
- `R/hiring_update.R` — `generate_new_contracts()`: add paygrade assignment
  step before `assign_compensation()` call; add `paygrade_rule` param
- `R/simulate_hiring.R` — thread `paygrade_rule` through `hire_policy`
- `R/validate_inputs.R` — validate `paygrade_rule` values
- `tests/testthat/test-hiring_core.R` — new tests for education draw
- `tests/testthat/test-hiring_update.R` — new tests for paygrade rules

**Open questions to resolve before implementation:**
- Should `educat7` on the outgoing pool be the *retiring* workers only, the
  *all-exit* pool (retirements + non-ret exits), or the full active stock?
- Should `"merit"` matching be strict (exact percentile interpolation) or
  nearest-rank?
- When `group_cols` includes `paygrade` itself (e.g. move_policy groups),
  does paygrade assignment happen before or after the group split?
- Do we want to store the drawn `educat7` and `paygrade` on `personnel_dt`
  or only on `contract_dt`?