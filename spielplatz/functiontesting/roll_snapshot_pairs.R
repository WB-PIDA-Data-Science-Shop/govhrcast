# =============================================================================
# roll_snapshot_pairs() — worked example with bra_hrmis_contract
# =============================================================================
#
# This script demonstrates how roll_snapshot_pairs() works as a higher-order
# function: it owns the iteration loop and key-setting; you provide the
# comparison logic in `f`.
#
# Data: bra_hrmis_contract  — 8,885 rows, 12 annual snapshots (2007–2018)
#                              for Alagoas state public sector workers.
#
# What we'll build: a function that, for each consecutive snapshot pair,
# computes:
#   - headcount at T0 and T1
#   - net growth (hires minus exits)
#   - mean salary change (%)
#   - number of stayers who changed paygrade  (proxy: promotions/demotions)
#   - number of stayers who changed est_id    (proxy: transfers)
#
# =============================================================================

library(data.table)
library(govhrcast)

# ── 0. Inspect the panel ─────────────────────────────────────────────────────
# 12 annual snapshots, each ~600–930 rows.
bra_hrmis_contract[, .(
  n_rows      = .N,
  n_personnel = uniqueN(personnel_id)
), keyby = ref_date]
#       ref_date n_rows n_personnel
#  1: 2007-09-01    632         604
#  2: 2008-09-01    640         613
#  ...
# 12: 2018-09-01    930         887


# ── 1. Define the comparison function ────────────────────────────────────────
#
# roll_snapshot_pairs() calls f(a, b, ...) for each consecutive date pair.
#   a = data.table of rows from the earlier snapshot (T0)
#   b = data.table of rows from the later  snapshot (T1)
#   ... = whatever extra args you passed to roll_snapshot_pairs()
#
# f must return a data.table (or NULL to skip the pair).

summarise_pair <- function(a, b,
                           personnel_id_col,
                           salary_col,
                           grade_col,
                           est_col) {

  # Dates: grab the first ref_date value from each sub-table.
  # roll_snapshot_pairs() already filtered the panel by date — every row in `a`
  # has the same ref_date, so [1L] is safe and cheap.
  t0_date <- a$ref_date[1L]
  t1_date <- b$ref_date[1L]

  # --- headcount ---
  hc_t0 <- uniqueN(a[[personnel_id_col]])
  hc_t1 <- uniqueN(b[[personnel_id_col]])

  # --- mean salary ---
  sal_t0 <- mean(a[[salary_col]], na.rm = TRUE)
  sal_t1 <- mean(b[[salary_col]], na.rm = TRUE)

  # --- who was present in BOTH snapshots? ---
  # These are the "stayers" — the cohort we can compare grade and est for.
  ids_t0  <- unique(a[[personnel_id_col]])
  ids_t1  <- unique(b[[personnel_id_col]])
  stayers <- intersect(ids_t0, ids_t1)

  # --- paygrade changes among stayers ---
  # One row per person per snapshot, then merge and compare.
  grade_t0 <- a[get(personnel_id_col) %in% stayers,
                .(id = get(personnel_id_col), grade_t0 = get(grade_col))]
  grade_t1 <- b[get(personnel_id_col) %in% stayers,
                .(id = get(personnel_id_col), grade_t1 = get(grade_col))]
  merged_grade   <- merge(grade_t0, grade_t1, by = "id")
  n_grade_change <- sum(merged_grade$grade_t0 != merged_grade$grade_t1,
                        na.rm = TRUE)

  # --- establishment changes among stayers ---
  est_t0 <- a[get(personnel_id_col) %in% stayers,
               .(id = get(personnel_id_col), est_t0 = get(est_col))]
  est_t1 <- b[get(personnel_id_col) %in% stayers,
               .(id = get(personnel_id_col), est_t1 = get(est_col))]
  merged_est   <- merge(est_t0, est_t1, by = "id")
  n_est_change <- sum(merged_est$est_t0 != merged_est$est_t1, na.rm = TRUE)

  # Return one summary row for this pair.
  data.table(
    t0_date        = t0_date,
    t1_date        = t1_date,
    hc_t0          = hc_t0,
    hc_t1          = hc_t1,
    net_growth     = hc_t1 - hc_t0,
    mean_sal_t0    = round(sal_t0, 0),
    mean_sal_t1    = round(sal_t1, 0),
    sal_change_pct = round((sal_t1 - sal_t0) / sal_t0 * 100, 1),
    n_stayers      = length(stayers),
    n_grade_change = n_grade_change,
    n_est_change   = n_est_change
  )
}


# ── 2. Run across all 11 consecutive pairs ───────────────────────────────────
#
# What roll_snapshot_pairs() does internally:
#   1. Validates panel_dt is a data.table and date_col exists
#   2. setkeyv(panel, "ref_date")         — O(log N) lookups from now on
#   3. all_dates = sort(unique(ref_date))  — 12 dates → 11 pairs
#   4. For k in 1:11:
#        snap_a = panel[.(all_dates[k])]   — binary search, no full scan
#        snap_b = panel[.(all_dates[k+1])]
#        results[[k]] = f(snap_a, snap_b, ...extra args...)
#   5. rbindlist(results)                  — stack all 11 rows
#
# Note: any extra named args after `f` are forwarded to EVERY call of f.
# That's how `personnel_id_col`, `salary_col`, etc. reach summarise_pair
# without roll_snapshot_pairs() needing to know they exist.

panel  <- copy(bra_hrmis_contract)   # copy so setkeyv side-effect doesn't
                                     # surprise us on the package dataset

result <- roll_snapshot_pairs(
  panel_dt         = panel,
  date_col         = "ref_date",
  f                = summarise_pair,
  # -- forwarded via ... to every call of summarise_pair: --
  personnel_id_col = "personnel_id",
  salary_col       = "gross_salary_lcu",
  grade_col        = "paygrade",
  est_col          = "est_id"
)

result
#        t0_date    t1_date hc_t0 hc_t1 net_growth mean_sal_t0 mean_sal_t1
#  1: 2007-09-01 2008-09-01   604   613          9        1694        2037
#  2: 2008-09-01 2009-09-01   613   610         -3        2037        2109
#  3: 2009-09-01 2010-09-01   610   616          6        2109        2218
#  4: 2010-09-01 2011-09-01   616   638         22        2218        2436
#  5: 2011-09-01 2012-09-01   638   646          8        2436        2664
#  6: 2012-09-01 2013-09-01   646   663         17        2664        2909
#  7: 2013-09-01 2014-09-01   663   715         52        2909        3192
#  8: 2014-09-01 2015-09-01   715   752         37        3192        3458
#  9: 2015-09-01 2016-09-01   752   875        123        3458        3811
# 10: 2016-09-01 2017-09-01   875   866         -9        3811        3980
# 11: 2017-09-01 2018-09-01   866   887         21        3980         NaN


# ── 3. Explore the results ───────────────────────────────────────────────────

# Mean annual salary growth rate across all pairs with valid salary data
result[!is.nan(sal_change_pct), mean(sal_change_pct)]
# ~8.9% — consistent with Brazilian public sector wage dynamics in this period

# Year with the biggest hiring surge
result[which.max(net_growth), .(t0_date, t1_date, net_growth)]
# 2015-09-01 → 2016-09-01: +123 new personnel

# Grade mobility: what fraction of stayers changed paygrade each year?
result[, .(
  t0_date,
  grade_mobility_pct = round(n_grade_change / n_stayers * 100, 1)
)]

# Transfer rate: share of stayers who changed establishment
result[, .(
  t0_date,
  transfer_rate_pct = round(n_est_change / n_stayers * 100, 1)
)]


# ── 4. NULL-skipping — what it looks like ────────────────────────────────────
#
# If f returns NULL for a pair, that pair is silently dropped from results.
# Useful when a pair has insufficient data to produce a meaningful summary.

flag_thin_pairs <- function(a, b, min_stayers) {
  ids_t0  <- unique(a$personnel_id)
  ids_t1  <- unique(b$personnel_id)
  stayers <- intersect(ids_t0, ids_t1)

  if (length(stayers) < min_stayers) return(NULL)  # skip this pair

  data.table(
    t0_date   = a$ref_date[1L],
    t1_date   = b$ref_date[1L],
    n_stayers = length(stayers)
  )
}

# All pairs have > 100 stayers, so nothing gets dropped here
roll_snapshot_pairs(
  panel_dt    = panel,
  date_col    = "ref_date",
  f           = flag_thin_pairs,
  min_stayers = 100   # forwarded via ...
)


# ── 5. Key mutation side-effect ───────────────────────────────────────────────
#
# After the call above, `panel` now has its key set to "ref_date".
# This is a side effect of setkeyv() inside roll_snapshot_pairs().
key(panel)
# [1] "ref_date"
#
# If you need to preserve the original key, pass data.table::copy(panel).
# In practice, you'll usually want this side-effect — the panel stays fast
# for any subsequent lookups you do yourself.
