# Builds a map of the package's code: every function, the file it lives in,
# what it takes, what it calls, and what calls it. Writes
#   data-raw/schemas/code_map.json  - the map (files, functions, constants, call edges)
#   data-raw/schemas/findings.md    - lists to audit from: unused code, calls that
#                                     cross module lines, long functions, ...
#   data-raw/schemas/code_atlas.html - an interactive page to explore the map,
#                                     built from code_atlas_template.html
#
# Everything is read from the source, so the map is never out of date: rerun
# this after changing the code, from the package root:
#   source("data-raw/schemas/build_code_map.R")
#
# Only code structure is read. No data is loaded and none is written.

# ---------------------------------------------------------------------------
# Where each file sits: its module and its role. This is the one part written
# by hand; a new file gets "other" until it is added here.
# ---------------------------------------------------------------------------
file_layout <- data.frame(
  file = c(
    "retirement_core.R", "retirement_update.R", "simulate_retirement.R",
    "retirement_policy_helpers.R", "retirement_pension.R",
    "exit_core.R", "exit_update.R", "simulate_exits.R", "exit_policy_helpers.R",
    "movement_core.R", "movement_update.R", "simulate_promotions_transfers.R",
    "movement_policy_helpers.R",
    "hiring_core.R", "hiring_update.R", "simulate_hiring.R", "hiring_policy_helpers.R",
    "allowance_core.R", "allowance_update.R", "simulate_allowances.R",
    "allowance_policy_helpers.R",
    "simulate_horizon.R", "horizon_core.R", "horizon_utils.R", "horizon_class.R",
    "utils.R", "validation.R", "salary_scale_utils.R", "govhr_adapters.R",
    "data.R", "zzz.R", "govhrcast-package.R"
  ),
  module = c(
    rep("retirement", 5), rep("exit", 4), rep("movement", 4), rep("hiring", 4),
    rep("allowance", 4), rep("horizon", 4),
    rep("shared", 4), rep("package", 3)
  ),
  role = c(
    "core", "update", "simulate", "policy_helpers", "pension",
    "core", "update", "simulate", "policy_helpers",
    "core", "update", "simulate", "policy_helpers",
    "core", "update", "simulate", "policy_helpers",
    "core", "update", "simulate", "policy_helpers",
    "orchestrator", "core", "utils", "class",
    "utils", "validation", "utils", "adapters",
    "data_docs", "package", "package"
  ),
  stringsAsFactors = FALSE
)

# Modules every other module may call into. Calls to anything else outside the
# caller's own module are listed as crossing module lines.
shared_modules <- c("shared", "package")

# data.table functions that change a table in place
by_reference_fns <- c(":=", "set", "setnames", "setkey", "setkeyv", "setorder",
                      "setorderv", "setcolorder", "setattr", "setDT", "setindex")

# ---------------------------------------------------------------------------
# Read the source
# ---------------------------------------------------------------------------
r_files   <- sort(list.files("R", pattern = "\\.R$"))
namespace <- readLines("NAMESPACE", warn = FALSE)
exported  <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE))
s3_methods <- sub("^S3method\\(([^,]+),([^)]+)\\)$", "\\1.\\2",
                  grep("^S3method\\(", namespace, value = TRUE))

unknown <- setdiff(r_files, file_layout$file)
if (length(unknown) > 0L) {
  message("Files not in file_layout (mapped to module/role 'other'): ",
          paste(unknown, collapse = ", "))
  file_layout <- rbind(file_layout, data.frame(file = unknown, module = "other",
                                               role = "other", stringsAsFactors = FALSE))
}

# Roxygen block directly above a definition: its title and tags
roxygen_above <- function(lines, start) {
  i <- start - 1L
  while (i >= 1L && grepl("^\\s*$", lines[i])) i <- i - 1L
  end <- i
  while (i >= 1L && grepl("^#'", lines[i])) i <- i - 1L
  block <- if (end > i) lines[(i + 1L):end] else character(0)
  text <- sub("^#' ?", "", block)
  title <- text[nzchar(trimws(text)) & !grepl("^@", text)][1]
  list(
    n_lines  = length(block),
    title    = if (is.na(title)) NA_character_ else trimws(title),
    internal = any(grepl("@keywords\\s+internal", text)),
    no_rd    = any(grepl("@noRd", text))
  )
}

# Walk a function's code and collect what it calls and refers to
walk_code <- function(expr) {
  calls <- character(0); refs <- character(0); strings <- character(0)
  external <- character(0); spec_keys <- character(0); returns <- list()
  changed_in_place <- character(0); assigned <- character(0)

  walk <- function(e) {
    if (is.call(e)) {
      head <- e[[1]]
      if (is.symbol(head)) {
        fn <- as.character(head)
        calls <<- c(calls, fn)
        # x[, col := ...] and set*(x, ...) change the table x itself
        if (fn == "[" && length(e) >= 2L && is.symbol(e[[2]]) &&
            any(vapply(as.list(e)[-(1:2)], function(a)
              !missing(a) && is.call(a) && identical(a[[1]], as.symbol(":=")), logical(1)))) {
          changed_in_place <<- c(changed_in_place, as.character(e[[2]]))
        }
        if (fn %in% setdiff(by_reference_fns, ":=") && length(e) >= 2L && is.symbol(e[[2]])) {
          changed_in_place <<- c(changed_in_place, as.character(e[[2]]))
        }
        if (fn %in% c("<-", "=") && length(e) == 3L && is.symbol(e[[2]])) {
          assigned <<- c(assigned, as.character(e[[2]]))
        }
        if (fn %in% c("::", ":::") && length(e) == 3L) {
          external <<- c(external, paste0(as.character(e[[2]]), "::", as.character(e[[3]])))
          return(invisible())
        }
        if (fn %in% c("$", "[[") && length(e) == 3L && is.symbol(e[[2]]) &&
            identical(as.character(e[[2]]), "column_spec_args")) {
          key <- e[[3]]
          if (is.symbol(key) || is.character(key)) spec_keys <<- c(spec_keys, as.character(key))
        }
        if (fn %in% c("do.call", "match.fun") && length(e) >= 2L && is.character(e[[2]])) {
          strings <<- c(strings, e[[2]])
        }
        if (fn == "return" && length(e) >= 2L) returns[[length(returns) + 1L]] <<- e[[2]]
      } else {
        walk(head)
      }
      for (arg in as.list(e)[-1]) if (!missing(arg)) walk(arg)
    } else if (is.symbol(e)) {
      name <- as.character(e)
      if (nzchar(name)) refs <<- c(refs, name)
    } else if (is.pairlist(e)) {
      for (arg in as.list(e)) if (!missing(arg)) walk(arg)
    }
  }
  walk(expr)
  list(calls = calls, refs = refs, strings = strings, external = external,
       spec_keys = spec_keys, returns = returns,
       changed_in_place = unique(changed_in_place), assigned = unique(assigned))
}

# What a function hands back: the names of a returned list, or the object's name
describe_return <- function(e) {
  if (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("list", "invisible") ) {
    if (as.character(e[[1]]) == "invisible" && length(e) >= 2L) return(describe_return(e[[2]]))
    nm <- names(as.list(e)[-1])
    if (!is.null(nm) && any(nzchar(nm))) return(paste0("list(", paste(nm[nzchar(nm)], collapse = ", "), ")"))
    return("list(...)")
  }
  if (is.symbol(e)) return(as.character(e))
  if (is.call(e) && is.symbol(e[[1]])) return(paste0(as.character(e[[1]]), "(...)"))
  if (is.null(e)) return("NULL")
  if (is.atomic(e) && length(e) == 1L) return(deparse(e))
  "<expression>"
}

last_expression <- function(body) {
  if (is.call(body) && identical(body[[1]], as.symbol("{"))) {
    if (length(body) < 2L) return(NULL)
    return(body[[length(body)]])
  }
  body
}

deparse_default <- function(x) {
  if (missing(x) || identical(x, quote(expr = ))) return(NA_character_)
  out <- paste(deparse(x, width.cutoff = 500L), collapse = " ")
  if (nchar(out) > 80L) out <- paste0(substr(out, 1L, 77L), "...")
  out
}

# ---------------------------------------------------------------------------
# Process trace: what a function does, step by step, in source order. Each step
# is a call to another package function (with its arguments, where the result is
# kept and what is read from it later), a hand-off (a result put into a variable
# the function carries on with), a table changed in place, or a section header
# from the code's own comment banners.
# ---------------------------------------------------------------------------
is_fn_def <- function(e) is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") &&
  is.symbol(e[[2]]) && is.call(e[[3]]) && identical(e[[3]][[1]], as.symbol("function"))
all_fn_names <- unlist(lapply(r_files, function(f) {
  ex <- parse(file.path("R", f), keep.source = FALSE)
  unlist(lapply(ex, function(e) if (is_fn_def(e)) as.character(e[[2]])))
}))

short <- function(x, n = 100L) {
  s <- paste(trimws(deparse(x, width.cutoff = 500L)), collapse = " ")
  if (nchar(s) > n) paste0(substr(s, 1L, n - 3L), "...") else s
}
base_symbol <- function(x) {
  while (is.call(x) && length(x) >= 2L) x <- x[[2]]
  if (is.symbol(x)) as.character(x) else NA_character_
}
# The function a call runs, without any pkg:: in front
bare_fn <- function(e) {
  if (!is.call(e)) return("")
  h <- e[[1]]
  if (is.symbol(h)) return(as.character(h))
  if (is.call(h) && is.symbol(h[[1]]) && as.character(h[[1]]) %in% c("::", ":::")) return(as.character(h[[3]]))
  ""
}

# The named parts of list(...) or data.table(...), and the variable behind each
# part: the variable itself, or the one variable an expression uses
# (as.integer(n_exits) -> n_exits); "" when there is no single variable
named_parts <- function(e) {
  if (!is.call(e)) return(list(names = character(0), values = character(0)))
  a <- as.list(e)[-1]; n <- names(a)
  if (is.null(n)) return(list(names = character(0), values = character(0)))
  keep <- nzchar(n)
  one_var <- function(v) {
    if (is.symbol(v)) return(as.character(v))
    vs <- all.vars(v)
    if (length(vs) == 1L) vs else ""
  }
  list(names = n[keep], values = vapply(a[keep], one_var, ""))
}

# Wrappers whose first argument is the real work: x <- tryCatch(estimate(...))
wrappers <- c("tryCatch", "suppressMessages", "suppressWarnings", "withCallingHandlers", "invisible",
              "try", "structure")

trace_process <- function(body, formal_names, lines, fn_start, fn_end) {
  items <- list(); accesses <- list(); return_langs <- list()
  add <- function(x) items[[length(items) + 1L]] <<- x
  head_name <- function(e) if (is.call(e) && is.symbol(e[[1]])) as.character(e[[1]]) else ""
  is_pkg_call <- function(e) head_name(e) %in% all_fn_names
  unwrap <- function(e) {
    while (head_name(e) %in% wrappers && length(e) >= 2L) e <- e[[2]]
    e
  }

  record_call <- function(e, ctx, line, target) {
    a <- as.list(e)[-1]; nm <- names(a); if (is.null(nm)) nm <- rep("", length(a))
    args <- list()
    for (k in seq_along(a)) {
      if (identical(a[[k]], quote(expr = ))) next
      args[[length(args) + 1L]] <- list(name = nm[k], expr = short(a[[k]], 100L),
                                         refs = I(unique(all.names(a[[k]]))))
    }
    add(list(type = "call", line = line, fn = head_name(e), target = target,
             base = if (is.null(target)) NA_character_ else target_base, args = args,
             context = I(ctx)))
  }
  target_base <- NA_character_

  walk <- function(e, ctx, line) {
    if (!is.call(e)) return(invisible())
    h <- head_name(e)
    if (h == "{") {
      sr <- attr(e, "srcref")
      for (k in seq_along(e)[-1]) {
        ln <- if (!is.null(sr) && length(sr) >= k) sr[[k]][1] else line
        walk(e[[k]], ctx, ln)
      }
      return(invisible())
    }
    if (h == "if") {
      cond <- short(e[[2]], 70L)
      walk(e[[2]], ctx, line)
      walk(e[[3]], c(ctx, paste0("if (", cond, ")")), line)
      if (length(e) == 4L) walk(e[[4]], c(ctx, paste0("otherwise, not (", cond, ")")), line)
      return(invisible())
    }
    if (h == "for") {
      walk(e[[3]], ctx, line)
      walk(e[[4]], c(ctx, paste0("for each ", as.character(e[[2]]), " in ", short(e[[3]], 60L))), line)
      return(invisible())
    }
    if (h == "while") {
      walk(e[[3]], c(ctx, paste0("while (", short(e[[2]], 60L), ")")), line)
      return(invisible())
    }
    if (h == "function") {
      walk(e[[3]], c(ctx, "inside a local function"), line)
      return(invisible())
    }
    if (h %in% c("<-", "=", "<<-") && length(e) == 3L) {
      target <- short(e[[2]], 60L); rhs <- unwrap(e[[3]])
      if (is_pkg_call(rhs)) {
        target_base <<- base_symbol(e[[2]])
        record_call(rhs, ctx, line, target)
        for (arg in as.list(rhs)[-1]) if (!missing(arg)) walk(arg, ctx, line)
      } else {
        builds <- if (bare_fn(rhs) %in% c("data.table", "data.frame")) "table"
                  else if (bare_fn(rhs) == "list") "list" else NA_character_
        # Parts only of a list or table built here, not any call's named arguments
        parts <- if (is.na(builds)) named_parts(NULL) else named_parts(rhs)
        add(list(type = "assign", line = line, target = target, base = base_symbol(e[[2]]),
                 expr = short(e[[3]], 110L), refs = I(unique(all.names(e[[3]]))),
                 builds = builds,
                 list_names = I(parts$names), list_values = I(parts$values),
                 is_null = is.null(e[[3]]), context = I(ctx)))
        walk(e[[3]], ctx, line)
      }
      return(invisible())
    }
    if (is_pkg_call(e)) {
      target_base <<- NA_character_
      record_call(e, ctx, line, NULL)
      for (arg in as.list(e)[-1]) if (!missing(arg)) walk(arg, ctx, line)
      return(invisible())
    }
    if (h == "[" && length(e) >= 2L && is.symbol(e[[2]]) &&
        any(vapply(as.list(e)[-(1:2)], function(a)
          !missing(a) && is.call(a) && identical(a[[1]], as.symbol(":=")), logical(1)))) {
      add(list(type = "inline", line = line, target = as.character(e[[2]]),
               expr = short(e, 110L), context = I(ctx)))
    }
    if (h %in% setdiff(by_reference_fns, ":=") && length(e) >= 2L && is.symbol(e[[2]])) {
      add(list(type = "inline", line = line, target = as.character(e[[2]]),
               expr = short(e, 110L), context = I(ctx)))
    }
    if (h == "$" && length(e) == 3L && is.symbol(e[[2]])) {
      accesses[[length(accesses) + 1L]] <<- list(var = as.character(e[[2]]),
                                                 field = as.character(e[[3]]), line = line)
      return(invisible())
    }
    if (h == "return" && length(e) >= 2L) {
      return_langs[[length(return_langs) + 1L]] <<- e[[2]]
      add(list(type = "return", line = line, expr = short(e[[2]], 110L),
               base = if (is.symbol(e[[2]])) as.character(e[[2]]) else NA_character_, context = I(ctx)))
    }
    if (is.call(e[[1]])) walk(e[[1]], ctx, line)
    for (arg in as.list(e)[-1]) if (!missing(arg)) walk(arg, ctx, line)
  }
  walk(body, character(0), fn_start)
  has_calls <- any(vapply(items, function(x) x$type == "call", TRUE))

  # The value handed back: a returned name, or the name on the last line
  last <- last_expression(body)
  returned <- unique(na.omit(c(
    vapply(Filter(function(x) x$type == "return", items), function(x) x$base, ""),
    if (is.symbol(last)) as.character(last) else NA_character_)))

  # Keep the hand-offs that carry results along; drop local bookkeeping
  outputs <- character(0); kept <- list()
  for (x in items) {
    if (x$type == "call") {
      if (!is.na(x$base)) outputs <- union(outputs, x$base)
      kept[[length(kept) + 1L]] <- x
    } else if (x$type == "assign") {
      if (!is.na(x$base) && (x$base %in% c(formal_names, outputs, returned) ||
                             any(unclass(x$refs) %in% outputs))) {
        outputs <- union(outputs, x$base)
        kept[[length(kept) + 1L]] <- x
      }
    } else if (x$type == "inline") {
      if (x$target %in% c(formal_names, outputs)) kept[[length(kept) + 1L]] <- x
    } else kept[[length(kept) + 1L]] <- x
  }

  # What is read from each call's result afterwards, until the name is reused
  set_lines <- function(v) vapply(Filter(function(y) y$type %in% c("call", "assign") &&
                                           identical(y$base, v), kept), `[[`, 0, "line")
  for (i in seq_along(kept)) {
    x <- kept[[i]]
    if (x$type != "call" || is.na(x$base) || !identical(x$base, x$target)) next
    later <- set_lines(x$base); later <- later[later > x$line]
    until <- if (length(later)) min(later) else Inf
    used <- Filter(function(a) a$var == x$base && a$line >= x$line && a$line < until, accesses)
    kept[[i]]$used_later <- I(sort(unique(vapply(used, `[[`, "", "field"))))
  }

  # Section headers: a comment line between two rule lines (# ---- or # ====)
  span <- lines[fn_start:fn_end]
  rule <- grepl("^\\s*#\\s*[-=]{8,}\\s*$", span)
  banners <- list()
  for (i in seq_len(max(0L, length(span) - 2L))) {
    if (rule[i] && !rule[i + 1L] && rule[i + 2L] && grepl("^\\s*#", span[i + 1L])) {
      title <- trimws(sub("^\\s*#+\\s*", "", span[i + 1L]))
      if (nzchar(title)) banners[[length(banners) + 1L]] <-
        list(type = "section", line = fn_start + i, title = title)
    }
  }
  all_items <- c(banners, kept)
  ord <- order(vapply(all_items, `[[`, 0, "line"),
               vapply(all_items, function(x) x$type != "section", TRUE), seq_along(all_items))

  # The result's parts: fields set on the returned object, or the names of a
  # list(...) handed back directly
  # Setting a part to NULL removes it, so it is not a part of the result. A
  # returned list's parts are its fields; a returned table's are its columns.
  ret_assigns <- Filter(function(x) x$type == "assign" && !is.na(x$base) &&
                          x$base %in% returned && !isTRUE(x$is_null), kept)
  fields <- unique(unlist(lapply(ret_assigns, function(x) {
    if (grepl("^[A-Za-z0-9_.]+\\$", x$target)) sub("^[A-Za-z0-9_.]+\\$", "", x$target)
    else if (identical(x$builds, "list")) unclass(x$list_names)
  })))
  result_columns <- unique(unlist(lapply(ret_assigns, function(x)
    if (identical(x$builds, "table")) unclass(x$list_names))))
  for (r in c(return_langs, list(last))) {
    r <- unwrap(r)
    if (bare_fn(r) %in% c("data.table", "data.frame")) result_columns <- union(result_columns, named_parts(r)$names)
  }
  # A list(...) handed back directly: its parts, and the variable behind each
  result_list <- NULL
  for (r in c(return_langs, list(last))) {
    r <- unwrap(r)
    if (head_name(r) == "list") {
      p <- named_parts(r)
      fields <- union(fields, p$names)
      result_list <- list(names = I(p$names), values = I(p$values))
    }
  }
  # A function that calls no other package function has no steps to show
  list(steps = if (has_calls) all_items[ord] else list(), returned = I(as.character(returned)),
       result_fields = I(sort(as.character(fields))), result_list = result_list,
       result_columns = I(as.character(result_columns)))
}

functions <- list(); constants <- list(); files_out <- list()

for (f in r_files) {
  path  <- file.path("R", f)
  lines <- readLines(path, warn = FALSE)
  exprs <- parse(path, keep.source = TRUE)
  refs  <- attr(exprs, "srcref")
  layout <- file_layout[file_layout$file == f, ]
  n_fun <- 0L

  for (k in seq_along(exprs)) {
    e <- exprs[[k]]
    if (!(is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") &&
          is.symbol(e[[2]]))) next
    name  <- as.character(e[[2]])
    start <- refs[[k]][1]; end <- refs[[k]][3]
    rhs   <- e[[3]]

    if (is.call(rhs) && identical(rhs[[1]], as.symbol("function"))) {
      n_fun <- n_fun + 1L
      fml  <- rhs[[2]]
      body <- rhs[[3]]
      code <- walk_code(body)
      defaults_code <- walk_code(as.pairlist(fml))
      rox  <- roxygen_above(lines, start)
      ret  <- c(lapply(code$returns, describe_return), list(describe_return(last_expression(body))))
      ext  <- table(sub("::.*$", "", code$external))

      functions[[length(functions) + 1L]] <- list(
        name         = name,
        file         = f,
        module       = layout$module,
        role         = layout$role,
        line_start   = start,
        line_end     = end,
        n_lines      = end - start + 1L,
        roxygen_lines = rox$n_lines,
        title        = rox$title,
        visibility   = if (name %in% s3_methods) "s3_method"
                       else if (name %in% exported) "exported"
                       else if (startsWith(name, ".")) "dot_internal"
                       else "internal",
        documented   = rox$n_lines > 0L && !rox$no_rd,
        args         = lapply(names(fml), function(a) list(name = a, default = deparse_default(fml[[a]]))),
        n_args       = length(fml),
        spec_keys    = sort(unique(code$spec_keys)),
        returns      = sort(unique(unlist(ret))),
        external_calls = as.list(setNames(as.integer(ext), names(ext))),
        n_stop       = sum(code$calls == "stop"),
        n_warning    = sum(code$calls == "warning"),
        n_message    = sum(code$calls == "message"),
        changes_by_reference = any(code$calls %in% by_reference_fns),
        copies       = any(code$calls == "copy") || any(code$external == "data.table::copy"),
        # Arguments changed with := or set*() and never reassigned first: the
        # caller's own table is changed
        changes_argument = sort(setdiff(intersect(code$changed_in_place, names(fml)), code$assigned)),
        local_functions = sum(code$calls == "function"),
        process      = trace_process(body, names(fml), lines, start, end),
        .calls       = c(code$calls, defaults_code$calls, code$strings),
        .refs        = c(code$refs, defaults_code$refs)
      )
    } else {
      constants[[length(constants) + 1L]] <- list(
        name = name, file = f, module = layout$module, line_start = start, line_end = end
      )
    }
  }
  files_out[[length(files_out) + 1L]] <- list(
    file = f, module = layout$module, role = layout$role,
    n_lines = length(lines), n_functions = n_fun
  )
}

fn_names    <- vapply(functions, `[[`, "", "name")

# A result built by another package function (out <- new_horizon(...)) has that
# function's parts too
result_of <- setNames(lapply(functions, function(f) unclass(f$process$result_fields)), fn_names)
for (i in seq_along(functions)) {
  p <- functions[[i]]$process
  if (is.null(p) || length(p$returned) == 0L) next
  makers <- Filter(function(s) s$type == "call" && identical(s$base, s$target) &&
                     s$base %in% unclass(p$returned), p$steps)
  extra <- unlist(lapply(makers, function(s) result_of[[s$fn]]))
  functions[[i]]$process$result_fields <- I(sort(unique(c(unclass(p$result_fields), extra))))
}
const_names <- vapply(constants, `[[`, "", "name")
duplicates  <- unique(fn_names[duplicated(fn_names)])

# ---------------------------------------------------------------------------
# Edges: who calls whom (a call), and who passes a function by name (a reference)
# ---------------------------------------------------------------------------
edges <- list()
for (fn in functions) {
  called <- table(fn$.calls[fn$.calls %in% fn_names & fn$.calls != fn$name])
  for (to in names(called)) {
    edges[[length(edges) + 1L]] <- list(from = fn$name, to = to, kind = "call", count = as.integer(called[[to]]))
  }
  # A function's name used as a value (lapply(x, fn)) but never called directly
  passed <- table(fn$.refs[fn$.refs %in% fn_names & fn$.refs != fn$name & !fn$.refs %in% names(called)])
  for (to in names(passed)) {
    edges[[length(edges) + 1L]] <- list(from = fn$name, to = to, kind = "reference", count = as.integer(passed[[to]]))
  }
  used <- table(fn$.refs[fn$.refs %in% const_names])
  for (to in names(used)) {
    edges[[length(edges) + 1L]] <- list(from = fn$name, to = to, kind = "constant", count = as.integer(used[[to]]))
  }
}
edge_from <- vapply(edges, `[[`, "", "from")
edge_to   <- vapply(edges, `[[`, "", "to")
edge_kind <- vapply(edges, `[[`, "", "kind")

# Which test files mention each function
test_files <- sort(list.files("tests/testthat", pattern = "^test.*\\.R$", full.names = TRUE))
test_text  <- vapply(test_files, function(p) paste(readLines(p, warn = FALSE), collapse = "\n"), "")
mentioned_in_tests <- function(name) {
  pattern <- paste0("(^|[^A-Za-z0-9_.])", gsub(".", "\\.", name, fixed = TRUE), "([^A-Za-z0-9_.]|$)")
  basename(test_files[grepl(pattern, test_text, perl = TRUE)])
}

# Which data-raw scripts (that build the bundled data) use each function
raw_files <- setdiff(sort(list.files("data-raw", pattern = "\\.R$", full.names = TRUE)),
                     "data-raw/schemas/build_code_map.R")
raw_text  <- vapply(raw_files, function(p) paste(readLines(p, warn = FALSE), collapse = "\n"), "")
mentioned_in_data_raw <- function(name) {
  pattern <- paste0("(^|[^A-Za-z0-9_.])", gsub(".", "\\.", name, fixed = TRUE), "([^A-Za-z0-9_.]|$)")
  basename(raw_files[grepl(pattern, raw_text, perl = TRUE)])
}

module_of <- setNames(vapply(functions, `[[`, "", "module"), fn_names)
for (i in seq_along(functions)) {
  nm <- functions[[i]]$name
  functions[[i]]$called_by   <- sort(unique(edge_from[edge_to == nm & edge_kind != "constant"]))
  functions[[i]]$calls       <- sort(unique(edge_to[edge_from == nm & edge_kind != "constant"]))
  functions[[i]]$constants_used <- sort(unique(edge_to[edge_from == nm & edge_kind == "constant"]))
  functions[[i]]$test_files  <- mentioned_in_tests(nm)
  functions[[i]]$data_raw_files <- mentioned_in_data_raw(nm)
  functions[[i]]$.calls <- NULL; functions[[i]]$.refs <- NULL
}
for (i in seq_along(constants)) {
  constants[[i]]$used_by <- sort(unique(edge_from[edge_to == constants[[i]]$name & edge_kind == "constant"]))
}

# ---------------------------------------------------------------------------
# Write the map. Sorted, so a rerun on unchanged code gives the same file.
# ---------------------------------------------------------------------------
fn_order <- order(vapply(functions, `[[`, "", "file"), vapply(functions, `[[`, 0L, "line_start"))
functions <- functions[fn_order]
edge_order <- order(edge_from, edge_to, edge_kind)

# The commit the map was built from, read from .git so git need not be on the path
git_commit <- function() {
  head <- tryCatch(readLines(".git/HEAD", warn = FALSE)[1], error = function(e) NA_character_)
  if (is.na(head)) return(NA_character_)
  if (!startsWith(head, "ref: ")) return(substr(head, 1L, 7L))
  ref <- sub("^ref: ", "", head)
  sha <- if (file.exists(file.path(".git", ref))) readLines(file.path(".git", ref), warn = FALSE)[1] else {
    packed <- tryCatch(readLines(".git/packed-refs", warn = FALSE), error = function(e) character(0))
    hit <- grep(paste0(" ", ref, "$"), packed, value = TRUE)
    if (length(hit) > 0L) sub(" .*$", "", hit[1]) else NA_character_
  }
  paste0(sub("^refs/heads/", "", ref), " @ ", substr(sha, 1L, 7L))
}
commit <- git_commit()
git_bin <- Sys.which("git")
dirty <- if (nzchar(git_bin)) {
  tryCatch(length(system2(git_bin, c("status", "--porcelain", "R"), stdout = TRUE)) > 0L,
           error = function(e) NA)
} else NA

code_map <- list(
  about = list(
    package    = "govhrcast",
    built_from = paste0("git ", commit,
                        if (isTRUE(dirty)) " plus uncommitted changes in R/"
                        else if (is.na(dirty)) " (uncommitted changes, if any, not checked)" else ""),
    generator  = "data-raw/schemas/build_code_map.R",
    notes = c(
      "Generated from the source: do not edit by hand, rerun the generator.",
      "edges$kind: call = called by name; reference = passed by name, e.g. lapply(x, fn); constant = reads a top-level constant.",
      "visibility: exported (in NAMESPACE), s3_method, internal (no dot, not exported), dot_internal.",
      "returns: the names of a returned list, or the returned object's name; read with the code, it is a hint.",
      "changes_by_reference: the function uses := or a data.table set*() function somewhere; copies: it calls copy().",
      "changes_argument: arguments changed with := or set*() that the function never reassigns, so the caller's table changes."
    )
  ),
  layout    = list(shared_modules = shared_modules),
  files     = files_out,
  functions = functions,
  constants = constants,
  edges     = edges[edge_order]
)

# Flow diagrams, one per function, from the process traces and the object
# catalogue (data-raw/schemas/object_catalogue.R)
source("data-raw/schemas/build_flows.R")
code_map$flows <- build_flows(functions)
write_flow_files(code_map$flows, functions)
# Lists stay lists in the JSON, even with one entry (otherwise jsonlite writes a
# one-entry list as a plain value)
list_fields <- c("spec_keys", "returns", "changes_argument", "called_by", "calls",
                 "constants_used", "test_files", "data_raw_files", "used_by")
as_lists <- function(x) {
  for (f in intersect(list_fields, names(x))) x[[f]] <- I(as.character(x[[f]]))
  x
}
code_map$functions <- lapply(code_map$functions, as_lists)
code_map$constants <- lapply(code_map$constants, as_lists)
jsonlite::write_json(code_map, "data-raw/schemas/code_map.json",
                     auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")

# ---------------------------------------------------------------------------
# Findings: lists to audit from
# ---------------------------------------------------------------------------
fn_df <- data.frame(
  name       = vapply(functions, `[[`, "", "name"),
  file       = vapply(functions, `[[`, "", "file"),
  module     = vapply(functions, `[[`, "", "module"),
  role       = vapply(functions, `[[`, "", "role"),
  line       = vapply(functions, `[[`, 0L, "line_start"),
  n_lines    = vapply(functions, `[[`, 0L, "n_lines"),
  n_args     = vapply(functions, `[[`, 0L, "n_args"),
  visibility = vapply(functions, `[[`, "", "visibility"),
  n_callers  = vapply(functions, function(x) length(x$called_by), 0L),
  n_tests    = vapply(functions, function(x) length(x$test_files), 0L),
  by_ref     = vapply(functions, `[[`, FALSE, "changes_by_reference"),
  copies     = vapply(functions, `[[`, FALSE, "copies"),
  stringsAsFactors = FALSE
)
loc <- function(d) sprintf("[%s:%d](../../R/%s#L%d)", d$file, d$line, d$file, d$line)
table_md <- function(d, cols, header) {
  if (nrow(d) == 0L) return("None.\n")
  rows <- apply(d[, cols, drop = FALSE], 1L, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(paste0("| ", paste(header, collapse = " | "), " |"),
          paste0("|", paste(rep("---", length(header)), collapse = "|"), "|"), rows), collapse = "\n")
}

out <- c(
  "# Code map findings",
  "",
  paste0("Generated by `data-raw/schemas/build_code_map.R` from ", code_map$about$built_from,
         ". Do not edit by hand: rerun the generator."),
  "",
  "These are leads to check, not verdicts. Calls are found by reading the code, so",
  "a function reached only through an unusual route (e.g. `get(\"name\")`) can look unused.",
  "",
  "## Overview",
  "",
  sprintf("%d files, %d lines; %d functions (%d exported, %d S3 methods, %d internal, %d dot-internal); %d top-level constants; %d call edges.",
          length(files_out), sum(vapply(files_out, `[[`, 0L, "n_lines")), nrow(fn_df),
          sum(fn_df$visibility == "exported"), sum(fn_df$visibility == "s3_method"),
          sum(fn_df$visibility == "internal"), sum(fn_df$visibility == "dot_internal"),
          length(constants), sum(edge_kind == "call")),
  ""
)

by_file <- do.call(rbind, lapply(files_out, as.data.frame))
by_file <- by_file[order(by_file$module, by_file$role), ]
out <- c(out, "### Files", "",
         table_md(by_file, c("module", "role", "file", "n_lines", "n_functions"),
                  c("Module", "Role", "File", "Lines", "Functions")), "")

if (length(duplicates) > 0L) {
  d <- fn_df[fn_df$name %in% duplicates, ]; d$where <- loc(d)
  out <- c(out, "## Defined more than once", "",
           "The later definition (in collation order) silently wins.", "",
           table_md(d, c("name", "where"), c("Function", "Where")), "")
}

unused <- fn_df[fn_df$n_callers == 0L & !fn_df$visibility %in% c("exported", "s3_method") &
                !fn_df$name %in% c(".onLoad", ".onAttach", ".onUnload"), ]
unused$where <- loc(unused)
in_raw <- vapply(functions, function(x) length(x$data_raw_files) > 0L, FALSE)[
  match(unused$name, fn_df$name)]
unused$tests <- ifelse(in_raw, "data-raw scripts",
                       ifelse(unused$n_tests > 0L, "tests only", "nowhere"))
unused <- unused[order(unused$tests, unused$file), ]
out <- c(out, "## Not exported and never called from the package", "",
         "\"data-raw scripts\": a script that builds the bundled data uses it; \"tests only\":",
         "something in `tests/` still uses it; \"nowhere\": nothing does.", "",
         table_md(unused, c("name", "where", "tests"), c("Function", "Where", "Used in")), "")

unused_const <- Filter(function(x) length(x$used_by) == 0L, constants)
if (length(unused_const) > 0L) {
  d <- data.frame(name = vapply(unused_const, `[[`, "", "name"),
                  file = vapply(unused_const, `[[`, "", "file"),
                  line = vapply(unused_const, `[[`, 0L, "line_start"), stringsAsFactors = FALSE)
  d$where <- loc(d)
  out <- c(out, "## Top-level objects no function reads", "",
           "Some are expected (e.g. `globalVariables()` calls, package docs).", "",
           table_md(d, c("name", "where"), c("Object", "Where")), "")
}

call_edges <- edges[edge_kind %in% c("call", "reference")]
cross <- do.call(rbind, lapply(call_edges, function(x) {
  from_mod <- module_of[[x$from]]; to_mod <- module_of[[x$to]]
  if (from_mod == to_mod || to_mod %in% shared_modules) return(NULL)
  # The orchestrators sit above the modules and may call into them
  if (from_mod == "horizon") return(NULL)
  data.frame(caller = x$from, caller_module = from_mod, callee = x$to,
             callee_module = to_mod, stringsAsFactors = FALSE)
}))
if (is.null(cross)) cross <- data.frame(caller = character(0))
if (nrow(cross) > 0L) cross <- cross[order(cross$callee_module, cross$callee, cross$caller_module), ]
out <- c(out, "## Calls that cross module lines", "",
         "A module calling into another module's files (not the shared utils/validation/adapters).",
         "The horizon orchestrators calling modules is expected and left out; a module calling",
         "into horizon files is not.", "",
         table_md(cross, c("caller", "caller_module", "callee", "callee_module"),
                  c("Caller", "Caller's module", "Calls", "Callee's module")), "")

long <- fn_df[fn_df$n_lines >= 150L, ]; long <- long[order(-long$n_lines), ]; long$where <- loc(long)
out <- c(out, "## Long functions (150+ lines, roxygen not counted)", "",
         table_md(long, c("name", "where", "n_lines"), c("Function", "Where", "Lines")), "")

wide <- fn_df[fn_df$n_args >= 8L, ]; wide <- wide[order(-wide$n_args), ]; wide$where <- loc(wide)
out <- c(out, "## Many arguments (8+)", "",
         table_md(wide, c("name", "where", "n_args", "visibility"), c("Function", "Where", "Arguments", "Visibility")), "")

fn_df$changes_argument <- vapply(functions, function(x) paste(x$changes_argument, collapse = ", "), "")
in_place <- fn_df[nzchar(fn_df$changes_argument), ]
in_place <- in_place[order(in_place$role != "core", in_place$file), ]
in_place$where <- loc(in_place)
out <- c(out, "## Functions that change a table they were given", "",
         "The argument is changed with `:=` or a `set*()` function and never reassigned (for",
         "example by `x <- copy(x)`), so the caller's own table changes. Core files are meant",
         "to leave their inputs alone; elsewhere it can be intended (an `*_update.R` helper",
         "working on a table its caller copied). Check each one, core first.", "",
         table_md(in_place, c("name", "role", "where", "changes_argument"),
                  c("Function", "Role", "Where", "Argument changed")), "")

untested <- fn_df[fn_df$visibility == "exported" & fn_df$n_tests == 0L, ]
untested$where <- loc(untested)
out <- c(out, "## Exported functions no test file mentions", "",
         table_md(untested, c("name", "where"), c("Function", "Where")), "")

writeLines(out, "data-raw/schemas/findings.md")

# ---------------------------------------------------------------------------
# The interactive page: the template with the map put inside it. "</" is
# escaped so nothing in the map can close the <script> tag that holds it.
# ---------------------------------------------------------------------------
template <- paste(readLines("data-raw/schemas/code_atlas_template.html", warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
map_json <- gsub("</", "<\\/", as.character(jsonlite::toJSON(
  code_map, auto_unbox = TRUE, null = "null", na = "null")), fixed = TRUE)
page <- sub("__CODE_MAP_JSON__", map_json, template, fixed = TRUE)
con <- file("data-raw/schemas/code_atlas.html", "wb")
writeLines(enc2utf8(page), con, sep = "\n", useBytes = TRUE)
close(con)
message("Wrote code_map.json, findings.md and code_atlas.html in data-raw/schemas: ",
        nrow(fn_df), " functions, ", sum(edge_kind == "call"), " call edges.")
