# Extracted from test-exit_policy_helpers.R:92

# prequel ----------------------------------------------------------------------
library(testthat)
library(data.table)
.at <- c("permanent", "fixed-term", "short-term")
make_eph_staff <- function(n = 100L, est = "E1") {
  ids <- sprintf("P%03d", seq_len(n))
  list(
    ct = data.table(contract_id = sub("P", "C", ids), personnel_id = ids,
                    est_id = rep_len(est, n), start_date = as.Date("2010-01-01"),
                    end_date = as.Date(NA), gross_salary_lcu = 1000,
                    contract_type = "permanent"),
    pt = data.table(personnel_id = ids, employment_status = "active")
  )
}
n_exits_with <- function(policy, d = make_eph_staff()) {
  simulate_exits(d$ct, d$pt, policy, ref_date = as.Date("2020-01-01"))$summary$n_exits
}
make_eph_panel <- function() {
  ids1 <- sprintf("P%02d", 1:10); ids2 <- sprintf("P%02d", 1:7)
  ct <- data.table(
    ref_date = c(rep(as.Date("2019-01-01"), 10), rep(as.Date("2020-01-01"), 7)),
    personnel_id = c(ids1, ids2), contract_id = sub("P", "C", c(ids1, ids2)),
    est_id = "E1", contract_type = "permanent", start_date = as.Date("2010-01-01"),
    end_date = as.Date(NA), gross_salary_lcu = 1000
  )
  list(ct = ct, pt = ct[, .(ref_date, personnel_id, employment_status = "active",
                            birth_date = as.Date("1980-01-01"))])
}
horizon_exits <- function(exit_policy) {
  p <- make_eph_panel()
  simulate_horizon(
    contract_dt = p$ct, personnel_dt = p$pt,
    salary_scale_dt = data.table(gross_salary_lcu = 1000),
    n_periods = 1L, exit_policy = exit_policy, salary_growth_rate = 0,
    ref_date = as.Date("2020-01-01"), seed = 1L
  )$summary_dt$n_non_ret_exits
}

# test -------------------------------------------------------------------------
expect_equal(horizon_exits(design_exit_policy_status_quo()), 2L)
