# Fitness functions: automated checks of the architecture's constraints
#
# Each test guards a constraint from the architecture decision records (ADR
# 0011 and the ADR named in the test; kept locally in spielplatz/adr/). They
# pass today; they exist to fail if a later change breaks the constraint --
# e.g. a new dependency, a write to disk, or a slowdown.

library(data.table)

# Every function in the package, with its name, for static checks.
package_functions <- function() {
  ns <- asNamespace("govhrcast")
  nms <- ls(ns, all.names = TRUE)
  nms <- nms[vapply(nms, function(n) is.function(get(n, ns)), logical(1))]
  stats::setNames(lapply(nms, function(n) get(n, ns)), nms)
}

# Names of every function called anywhere inside an expression.
called_functions <- function(e) {
  if (!is.call(e)) return(character(0))
  head <- e[[1]]
  name <- if (is.name(head)) as.character(head) else
    if (is.call(head) && identical(head[[1]], as.name("::"))) as.character(head[[3]]) else ""
  inner <- unlist(lapply(as.list(e)[-1], function(a) if (missing(a)) NULL else called_functions(a)))
  c(name, inner)
}

test_that("package code never writes files, calls the network or changes global settings (ADR 0011)", {
  # Results stay in memory unless the user saves them; session settings
  # belong to the user (the RNG is handled separately -- ADR 0008).
  forbidden <- c(
    "saveRDS", "save", "write", "write.csv", "write.table", "fwrite", "writeLines",
    "sink", "file", "file.create", "file.copy", "unlink", "dir.create",
    "download.file", "url",
    "options", "Sys.setenv", "setDTthreads", "set_collapse"
  )
  fns <- package_functions()
  hits <- unlist(lapply(names(fns), function(n) {
    used <- intersect(called_functions(body(fns[[n]])), forbidden)
    if (length(used)) paste0(n, "(): ", paste(used, collapse = ", "))
  }))
  expect_identical(hits, NULL)
})

test_that("a bare ref_date never appears inside data.table brackets (B-86)", {
  # There it reads a column called ref_date when the table has one, which stays
  # at the start date during a projection, instead of the date passed in. Use a
  # local alias such as .as_of_date. Listed here: `[` on a plain vector, or a
  # table made in the function that holds no such column.
  safe <- c("compute_age", "compute_time_in_grade", "select_nearest_ref_date",
            "update_allowance_register")
  names_in <- function(e) {
    if (is.name(e)) return(as.character(e))
    if (!is.call(e)) return(character(0))
    unlist(lapply(as.list(e)[-1], function(a) if (missing(a)) NULL else names_in(a)))
  }
  uses_it <- function(e) {
    if (!is.call(e)) return(FALSE)
    parts <- as.list(e)
    in_brackets <- identical(e[[1]], as.name("[")) &&
      "ref_date" %in% unlist(lapply(parts[-(1:2)], function(a) if (missing(a)) NULL else names_in(a)))
    in_brackets || any(vapply(parts, function(a) !missing(a) && uses_it(a), logical(1)))
  }
  fns <- package_functions()
  hits <- Filter(function(n) !n %in% safe && !is.primitive(fns[[n]]) && uses_it(body(fns[[n]])),
                 names(fns))
  expect_identical(hits, character(0))
})

test_that("Imports stay within the approved list; a new import needs an ADR (ADR 0004)", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "govhrcast"), fields = "Imports")
  imports <- trimws(sub("\\(.*\\)", "", strsplit(desc[1, "Imports"], ",")[[1]]))
  approved <- c("data.table", "govhr", "methods", "stats", "utils")
  expect_identical(setdiff(imports, approved), character(0))
})

test_that("a 5-year status-quo projection on the bundled data runs within 10 seconds (ADR 0011)", {
  skip_on_covr()  # coverage instrumentation slows the code several-fold
  start <- as.Date("2012-09-01")
  hc <- bra_hrmis_contract[ref_date <= start]
  hp <- bra_hrmis_personnel[ref_date <= start]
  scale <- hc[ref_date == start & !is.na(base_salary_lcu),
              .(base_salary_lcu = mean(base_salary_lcu)), by = est_id]
  elapsed <- system.time(suppressMessages(suppressWarnings(simulate_horizon(
    contract_dt = hc, personnel_dt = hp, salary_scale_dt = scale,
    n_periods = 5L, ref_date = start,
    retirement_policy = design_retirement_policy_status_quo(
      pension = pension_db(0.02, "base_salary_lcu", 35, 0.80)),
    exit_policy     = design_exit_policy_status_quo(group_cols = "est_id"),
    movement_policy = design_movement_policy_status_quo(
      group_cols = "est_id", movement_type = "transfer", salary_update_rule = "keep"),
    hiring_policy   = design_hiring_policy_status_quo(group_cols = "est_id",
                                                      salary_scale = scale),
    allowance_policy = design_allowance_policy_status_quo(
      bra_hrmis_allowance, hc, group_cols = "est_id", ref_date = start),
    salary_growth_rate = 0.05,
    column_spec_args   = list(salary_col = "base_salary_lcu"),
    seed = 1L
  ))))[["elapsed"]]
  expect_lt(elapsed, 10)
})
