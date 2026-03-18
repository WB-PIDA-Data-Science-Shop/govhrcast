library(testthat)
library(data.table)

make_scale <- function() {
  data.table::data.table(
    grade            = c("G1", "G2", "G3"),
    gross_salary_lcu = c(40000, 55000, 70000)
  )
}

# =============================================================================
# apply_salary_scale_adjustment()
# =============================================================================

test_that("Phase 4a: scalar multiplier applied to all entries", {
  sc     <- make_scale()
  result <- apply_salary_scale_adjustment(sc, adjustment = 1.05)

  expect_equal(result$gross_salary_lcu,
               c(40000, 55000, 70000) * 1.05)
})

test_that("Phase 4a: scalar multiplier = 1.0 leaves scale unchanged", {
  sc     <- make_scale()
  result <- apply_salary_scale_adjustment(sc, adjustment = 1.0)

  expect_equal(result$gross_salary_lcu, sc$gross_salary_lcu)
})

test_that("Phase 4a: named vector adjusts matched entries, unmatched stay unchanged", {
  sc <- make_scale()
  # G2 and G3 adjusted; G1 not in names → multiplier 1.0
  adj    <- c(G2 = 1.1, G3 = 1.15)
  result <- suppressWarnings(
    apply_salary_scale_adjustment(sc, adjustment = adj, key_col = "grade")
  )

  expect_equal(result[grade == "G1"]$gross_salary_lcu, 40000)   # unchanged
  expect_equal(result[grade == "G2"]$gross_salary_lcu, 55000 * 1.1)
  expect_equal(result[grade == "G3"]$gross_salary_lcu, 70000 * 1.15)
})

test_that("Phase 4a: all grades matched in named vector — no unmatched warning", {
  sc  <- make_scale()
  adj <- c(G1 = 1.02, G2 = 1.03, G3 = 1.04)
  expect_no_warning(
    apply_salary_scale_adjustment(sc, adjustment = adj, key_col = "grade")
  )
})

test_that("Phase 4a: unmatched key_col values produce a warning", {
  sc <- make_scale()
  # G1 not in names(adj)
  adj <- c(G2 = 1.05, G3 = 1.05)
  expect_warning(
    apply_salary_scale_adjustment(sc, adjustment = adj, key_col = "grade"),
    regexp = "G1"
  )
})

test_that("Phase 4a: zero multiplier raises error", {
  sc <- make_scale()
  expect_error(
    apply_salary_scale_adjustment(sc, adjustment = 0),
    regexp = "> 0"
  )
})

test_that("Phase 4a: negative multiplier raises error", {
  sc <- make_scale()
  expect_error(
    apply_salary_scale_adjustment(sc, adjustment = -1.05),
    regexp = "> 0"
  )
})

test_that("Phase 4a: extreme multiplier triggers warning", {
  sc <- make_scale()
  expect_warning(
    apply_salary_scale_adjustment(sc, adjustment = 2.0),
    regexp = "1\\.2"
  )
})

test_that("Phase 4a: output schema identical to input (same columns)", {
  sc     <- make_scale()
  result <- apply_salary_scale_adjustment(sc, adjustment = 1.05)

  expect_equal(names(result), names(sc))
  expect_equal(nrow(result), nrow(sc))
})

test_that("Phase 4a: input is not modified (copy semantics)", {
  sc         <- make_scale()
  orig_vals  <- copy(sc$gross_salary_lcu)
  apply_salary_scale_adjustment(sc, adjustment = 1.1)

  expect_equal(sc$gross_salary_lcu, orig_vals)
})

test_that("Phase 4a: named vector with unknown key gives warning", {
  sc <- make_scale()
  adj <- c(G1 = 1.0, G2 = 1.0, G3 = 1.0, G99 = 1.0)  # G99 not in scale, value in-range
  expect_warning(
    apply_salary_scale_adjustment(sc, adjustment = adj, key_col = "grade"),
    regexp = "G99"
  )
})

test_that("Phase 4a: key_col must be provided for named vector", {
  sc <- make_scale()
  adj <- c(G1 = 1.05, G2 = 1.05, G3 = 1.05)
  expect_error(
    apply_salary_scale_adjustment(sc, adjustment = adj, key_col = NULL),
    regexp = "key_col"
  )
})

test_that("Phase 4a: non-data.table input raises error", {
  sc <- as.data.frame(make_scale())
  expect_error(
    apply_salary_scale_adjustment(sc, adjustment = 1.05),
    regexp = "data.table"
  )
})
