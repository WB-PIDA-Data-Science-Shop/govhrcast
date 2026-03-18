# Unit tests for retirement_update.R functions
# Testing contract and personnel state update logic

library(testthat)
library(data.table)

# =============================================================================
# Test data setup
# =============================================================================

create_test_contracts_for_update <- function() {
  data.table(
    contract_id = c("C001", "C002", "C003", "C004", "C005", "C006"),
    personnel_id = c("P001", "P001", "P002", "P003", "P003", "P004"),
    start_date = as.Date(c("2000-01-01", "2010-01-01", "2005-01-01", 
                           "2000-01-01", "2015-01-01", "2010-01-01")),
    end_date = as.Date(c(NA, NA, NA, "2020-01-01", NA, NA)),
    gross_salary_lcu = c(8000, 10000, 9000, 5000, 7000, 6000),
    contract_type_code = c("perm", "perm", "perm", "inactive", "perm", "perm")
  )
}

create_test_personnel_for_update <- function() {
  data.table(
    personnel_id = c("P001", "P002", "P003", "P004"),
    status = c("active", "active", "active", "active")
  )
}

# =============================================================================
# update_contracts_for_retirees()
# =============================================================================

test_that("update_contracts_for_retirees marks all active contracts pensioner", {
  contract_dt <- create_test_contracts_for_update()
  
  # P001 retires - has 2 active contracts
  retirees_dt <- data.table(
    personnel_id = "P001"
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # Both active contracts for P001 become pensioner (Phase 0d: all active → pensioner)
  expect_equal(result[contract_id == "C002"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C002"]$end_date, ref_date)
  
  expect_equal(result[contract_id == "C001"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C001"]$end_date, ref_date)
  
  # Other contracts unchanged
  expect_equal(result[contract_id == "C003"]$contract_type_code, "perm")
  expect_true(is.na(result[contract_id == "C003"]$end_date))
})

test_that("update_contracts_for_retirees handles multiple retirees", {
  contract_dt <- create_test_contracts_for_update()
  
  # P001 and P002 retire
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002")
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # P001's primary
  expect_equal(result[contract_id == "C002"]$contract_type_code, "pensioner")
  
  # P002's only contract
  expect_equal(result[contract_id == "C003"]$contract_type_code, "pensioner")
  
  # P003 and P004 unchanged
  expect_equal(result[contract_id == "C005"]$contract_type_code, "perm")
  expect_equal(result[contract_id == "C006"]$contract_type_code, "perm")
})

test_that("update_contracts_for_retirees marks all active contracts pensioner (start_date fixture)", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P001"),
    start_date = as.Date(c("2000-01-01", "2015-01-01")),  # C002 is later
    end_date = as.Date(c(NA, NA)),
    gross_salary_lcu = c(10000, 10000),  # Same salary
    contract_type_code = c("perm", "perm")
  )
  
  retirees_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # Both active contracts become pensioner (Phase 0d)
  expect_equal(result[contract_id == "C002"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C001"]$contract_type_code, "pensioner")
})

test_that("update_contracts_for_retirees marks all active contracts pensioner (salary fixture)", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P001"),
    start_date = as.Date(c("2015-01-01", "2015-01-01")),  # Same start
    end_date = as.Date(c(NA, NA)),
    gross_salary_lcu = c(8000, 12000),  # C002 higher
    contract_type_code = c("perm", "perm")
  )
  
  retirees_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # Both active contracts become pensioner (Phase 0d)
  expect_equal(result[contract_id == "C002"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C001"]$contract_type_code, "pensioner")
})

test_that("update_contracts_for_retirees marks all active contracts pensioner (contract_id fixture)", {
  contract_dt <- data.table(
    contract_id = c("C003", "C001", "C002"),  # Intentionally unordered
    personnel_id = c("P001", "P001", "P001"),
    start_date = as.Date(c("2015-01-01", "2015-01-01", "2015-01-01")),  # All same
    end_date = as.Date(c(NA, NA, NA)),
    gross_salary_lcu = c(10000, 10000, 10000),  # All same
    contract_type_code = c("perm", "perm", "perm")
  )
  
  retirees_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # All three active contracts become pensioner (Phase 0d)
  expect_equal(result[contract_id == "C001"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C002"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C003"]$contract_type_code, "pensioner")
})

test_that("update_contracts_for_retirees only updates active contracts", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P001", "P001"),
    start_date = as.Date(c("2000-01-01", "2010-01-01", "2015-01-01")),
    end_date = as.Date(c("2010-01-01", NA, NA)),  # C001 already ended
    gross_salary_lcu = c(15000, 10000, 8000),
    contract_type_code = c("inactive", "perm", "perm")
  )
  
  retirees_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # C001 should remain inactive (already closed — not overwritten)
  expect_equal(result[contract_id == "C001"]$contract_type_code, "inactive")
  expect_equal(result[contract_id == "C001"]$end_date, as.Date("2010-01-01"))
  
  # Both active contracts become pensioner (Phase 0d: all active → pensioner)
  expect_equal(result[contract_id == "C003"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C002"]$contract_type_code, "pensioner")
})

test_that("update_contracts_for_retirees handles single contract per retiree", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2000-01-01", "2005-01-01")),
    end_date = as.Date(c(NA, NA)),
    gross_salary_lcu = c(10000, 9000),
    contract_type_code = c("perm", "perm")
  )
  
  retirees_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # P001's only contract becomes pensioner
  expect_equal(result[contract_id == "C001"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C001"]$end_date, ref_date)
  
  # P002 unchanged
  expect_equal(result[contract_id == "C002"]$contract_type_code, "perm")
  expect_true(is.na(result[contract_id == "C002"]$end_date))
})

test_that("update_contracts_for_retirees handles empty retirees list", {
  contract_dt <- create_test_contracts_for_update()
  
  retirees_dt <- data.table(personnel_id = character())
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # Nothing should change
  expect_equal(nrow(result), nrow(contract_dt))
  expect_true(all(result$contract_type_code %in% c("perm", "inactive")))
})

test_that("update_contracts_for_retirees modifies input data.table in place", {
  contract_dt <- create_test_contracts_for_update()
  original_contract_dt <- copy(contract_dt)
  
  retirees_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # Input should be modified (same object returned)
  expect_true(identical(contract_dt, result))
  
  # Should have changes compared to original
  expect_false(identical(contract_dt, original_contract_dt))
  expect_true(any(contract_dt$contract_type_code == "pensioner"))
})

test_that("update_contracts_for_retirees handles NA salaries (all active become pensioner)", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P001"),
    start_date = as.Date(c("2015-01-01", "2015-01-01")),
    end_date = as.Date(c(NA, NA)),
    gross_salary_lcu = c(10000, NA),  # C002 has NA salary
    contract_type_code = c("perm", "perm")
  )
  
  retirees_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # Both active contracts become pensioner (Phase 0d)
  expect_equal(result[contract_id == "C001"]$contract_type_code, "pensioner")
  expect_equal(result[contract_id == "C002"]$contract_type_code, "pensioner")
})

test_that("update_contracts_for_retirees handles retiree with no active contracts", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2000-01-01", "2005-01-01")),
    end_date = as.Date(c("2020-01-01", NA)),  # P001's contract ended
    gross_salary_lcu = c(10000, 9000),
    contract_type_code = c("inactive", "perm")
  )
  
  retirees_dt <- data.table(personnel_id = "P001")  # But has no active contracts
  ref_date <- as.Date("2025-01-01")
  
  result <- update_contracts_for_retirees(contract_dt, retirees_dt, ref_date)
  
  # P001's inactive contract is not overwritten (already closed)
  expect_equal(result[contract_id == "C001"]$contract_type_code, "inactive")
  expect_equal(result[contract_id == "C002"]$contract_type_code, "perm")
})

# =============================================================================
# update_personnel_for_retirees()
# =============================================================================

test_that("update_personnel_for_retirees updates status correctly", {
  personnel_dt <- create_test_personnel_for_update()
  
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    contract_type_code = c("pensioner", "perm")
  )
  
  result <- update_personnel_for_retirees(personnel_dt, contract_dt)
  
  # P001 should be inactive (has pensioner contract)
  expect_equal(result[personnel_id == "P001"]$status, "inactive")
  
  # Others remain active
  expect_equal(result[personnel_id == "P002"]$status, "active")
  expect_equal(result[personnel_id == "P003"]$status, "active")
  expect_equal(result[personnel_id == "P004"]$status, "active")
})

test_that("update_personnel_for_retirees handles multiple retirees", {
  personnel_dt <- create_test_personnel_for_update()
  
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003", "C004"),
    personnel_id = c("P001", "P002", "P003", "P004"),
    contract_type_code = c("pensioner", "pensioner", "perm", "perm")
  )
  
  result <- update_personnel_for_retirees(personnel_dt, contract_dt)
  
  # P001 and P002 should be inactive
  expect_equal(result[personnel_id == "P001"]$status, "inactive")
  expect_equal(result[personnel_id == "P002"]$status, "inactive")
  
  # P003 and P004 remain active
  expect_equal(result[personnel_id == "P003"]$status, "active")
  expect_equal(result[personnel_id == "P004"]$status, "active")
})

test_that("update_personnel_for_retirees handles no pensioners", {
  personnel_dt <- create_test_personnel_for_update()
  
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003", "C004"),
    personnel_id = c("P001", "P002", "P003", "P004"),
    contract_type_code = c("perm", "perm", "perm", "perm")
  )
  
  result <- update_personnel_for_retirees(personnel_dt, contract_dt)
  
  # All should remain active
  expect_true(all(result$status == "active"))
})

test_that("update_personnel_for_retirees modifies input data.table in place", {
  personnel_dt <- create_test_personnel_for_update()
  original_personnel_dt <- copy(personnel_dt)
  
  contract_dt <- data.table(
    contract_id = "C001",
    personnel_id = "P001",
    contract_type_code = "pensioner"
  )
  
  result <- update_personnel_for_retirees(personnel_dt, contract_dt)
  
  # Input should be modified (same object returned)
  expect_true(identical(personnel_dt, result))
  
  # Should have changes compared to original
  expect_false(identical(personnel_dt, original_personnel_dt))
  expect_equal(personnel_dt[personnel_id == "P001"]$status, "inactive")
})

test_that("update_personnel_for_retirees handles personnel with multiple contracts", {
  personnel_dt <- create_test_personnel_for_update()
  
  # P001 has multiple contracts, one is pensioner
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P001", "P002"),
    contract_type_code = c("pensioner", "closed_due_to_retirement", "perm")
  )
  
  result <- update_personnel_for_retirees(personnel_dt, contract_dt)
  
  # P001 should be inactive (has at least one pensioner contract)
  expect_equal(result[personnel_id == "P001"]$status, "inactive")
  
  # P002 remains active
  expect_equal(result[personnel_id == "P002"]$status, "active")
})

test_that("update_personnel_for_retirees handles empty contract table", {
  personnel_dt <- create_test_personnel_for_update()
  
  contract_dt <- data.table(
    contract_id = character(),
    personnel_id = character(),
    contract_type_code = character()
  )
  
  result <- update_personnel_for_retirees(personnel_dt, contract_dt)
  
  # All should remain active
  expect_true(all(result$status == "active"))
})

test_that("update_personnel_for_retirees handles pensioner not in personnel table", {
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    status = c("active", "active")
  )
  
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P002", "P999"),  # P999 not in personnel_dt
    contract_type_code = c("perm", "perm", "pensioner")
  )
  
  result <- update_personnel_for_retirees(personnel_dt, contract_dt)
  
  # Should not error, just update existing personnel
  expect_equal(nrow(result), 2)
  expect_true(all(result$status == "active"))
})
