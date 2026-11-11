# Draws each function's process trace as a Mermaid flowchart: functions as boxes,
# tables with their unit and main columns, lists as an indented tree, and vectors
# as labelled arrows. Sourced by build_code_map.R, which passes in `functions`
# (the map's function records); writes data-raw/schemas/flows/*.md and returns the
# diagrams for the Code Atlas page.

source("data-raw/schemas/object_catalogue.R")

build_flows <- function(functions) {
  F <- setNames(functions, vapply(functions, `[[`, "", "name"))
  checks <- names(F)[vapply(F, function(f) f$file == "validation.R", TRUE)]
  skip <- union(flow_skip, checks)

  # ---- Labels -------------------------------------------------------------
  esc <- function(s) {
    s <- gsub("&", "#38;", s, fixed = TRUE); s <- gsub('"', "#quot;", s, fixed = TRUE)
    s <- gsub("<", "#lt;", s, fixed = TRUE);  gsub(">", "#gt;", s, fixed = TRUE)
  }
  sp <- function(n) strrep("#160;", n)
  wrap_cols <- function(cols, first, per_line = 3L) {
    chunks <- split(cols, ceiling(seq_along(cols) / per_line))
    lines <- vapply(chunks, function(x) paste(esc(x), collapse = ", "), "")
    c(paste0(first, lines[1]), if (length(lines) > 1L) paste0(sp(8), lines[-1]))
  }
  title_line <- function(name, kind) paste0("<b>", esc(name), "</b> · ", kind)
  table_label <- function(name, entry = NULL, columns = NULL) {
    unit <- entry$unit %||% "(not in the catalogue yet)"
    cols <- if (length(columns)) columns else entry$columns %||% "(not in the catalogue yet)"
    paste(c(title_line(name, "table"), paste0("unit = ", esc(unit)),
            wrap_cols(cols, "main columns = ")), collapse = "<br/>")
  }
  # A list as a tree: each part on its own line, nested parts indented
  tree_lines <- function(parts, unread = character(0), prefix = "") {
    n <- length(parts); out <- character(0)
    for (i in seq_len(n)) {
      nm <- names(parts)[i]; last <- i == n
      p <- parts[[i]]
      elbow <- if (last) "└─ " else "├─ "
      cont  <- if (last) sp(4) else paste0("│", sp(3))
      known <- object_catalogue[[nm]]
      desc <- if (is.list(p) && identical(p$kind, "list")) "list"
              else if (is.character(p) && nzchar(p[1])) esc(p[1])
              else if (!is.null(known) && identical(known$kind, "table")) paste0("table, ", esc(known$unit))
              else if (!is.null(known) && identical(known$kind, "list")) "list"
              else ""
      line <- paste0(prefix, elbow, "<b>", esc(nm), "</b>", if (nzchar(desc)) paste0(": ", desc) else "")
      if (nm %in% unread) line <- paste0(line, " <i>(not read here)</i>")
      out <- c(out, line)
      if (is.list(p) && identical(p$kind, "list") && length(p$parts))
        out <- c(out, tree_lines(p$parts, character(0), paste0(prefix, cont)))
    }
    out
  }
  list_label <- function(name, parts, unread = character(0)) {
    paste(c(title_line(name, "list"), tree_lines(parts, unread)), collapse = "<br/>")
  }
  parts_from_fields <- function(fields) setNames(as.list(rep("", length(fields))), fields)

  # Conditions in words where the pattern is common; otherwise the code itself
  cond_words <- function(c) {
    v <- "([A-Za-z0-9_.$]+)"
    c <- sub(paste0("^!is\\.null\\(", v, "\\)$"), "\\1 is given", c)
    c <- sub(paste0("^is\\.null\\(", v, "\\)$"), "there is no \\1", c)
    c <- sub(paste0("^", v, " %in% names\\(", v, "\\)$"), "\\2 has the \\1 column", c)
    c <- sub(paste0("^identical\\(", v, ", \"([^\"]+)\"\\)$"), "\\1 is \"\\2\"", c)
    c <- sub(paste0("^", v, " != 0$"), "\\1 is not 0", c)
    c
  }
  # Each part of a && condition in words, joined with "and"
  conds_words <- function(c) vapply(c, function(one)
    paste(cond_words(trimws(strsplit(one, " && ", fixed = TRUE)[[1]])), collapse = " and "), "")
  pretty_ctx <- function(x) {
    out <- x
    i <- grepl("^if \\(", x)
    out[i] <- paste("if", conds_words(sub("^if \\((.*)\\)$", "\\1", x[i])))
    o <- grepl("^otherwise, not \\(", x)
    out[o] <- paste0("otherwise (not: ", conds_words(sub("^otherwise, not \\((.*)\\)$", "\\1", x[o])), ")")
    out <- sub("^for each ([A-Za-z0-9_.]+) in seq_len\\((.+)\\)$", "for each \\1 from 1 to \\2", out)
    sub("^inside a local function$", "inside a helper", out)
  }
  kind_of <- function(name, callee = NULL) {
    e <- object_catalogue[[name]]
    if (!is.null(e)) return(e$kind)
    if (grepl("_col$", name)) return("hidden")
    if (!is.null(callee) && length(callee$process$result_columns)) return("table")
    if (!is.null(callee) && length(callee$process$result_fields)) return("list")
    if (grepl("(_dt|_register|_table)$", name)) return("table")
    if (grepl("_(policy|params|args|result)$", name)) return("list")
    "vector"
  }

  one_flow <- function(f) {
    steps <- f$process$steps
    if (!length(steps)) return(NULL)
    formals <- vapply(f$args, `[[`, "", "name")
    nodes <- list(); edges <- list(); links <- list(); cur <- list()
    group_titles <- character(0); section <- ""; calls_seen <- list(); n_fn <- 0L
    last_inline <- list(id = "", target = "", group = "")

    group_of <- function(ctx) {
      t <- paste(c(if (nzchar(section)) section, pretty_ctx(unlist(ctx))), collapse = " · ")
      if (!nzchar(t)) return("")
      if (!t %in% group_titles) group_titles[[length(group_titles) + 1L]] <<- t
      t
    }
    new_node <- function(label, cls, group, shape = "rect") {
      id <- paste0("N", length(nodes) + 1L)
      nodes[[id]] <<- list(label = label, cls = cls, group = group, shape = shape)
      id
    }
    add_edge <- function(from, to, label = "") {
      if (is.null(from) || is.null(to) || from == to) return(invisible())
      key <- paste(from, to)
      if (!is.null(edges[[key]])) {
        if (nzchar(label) && !label %in% edges[[key]]$labels) edges[[key]]$labels <<- c(edges[[key]]$labels, label)
      } else edges[[key]] <<- list(from = from, to = to, labels = if (nzchar(label)) label else character(0))
    }
    source_of <- function(v) {
      if (!is.null(cur[[v]])) return(cur[[v]])
      if (!v %in% formals) return(NULL)
      k <- kind_of(v)
      if (k == "hidden") return(NULL)
      id <- switch(k,
        table  = new_node(table_label(v, object_catalogue[[v]]), "tbl", "Comes in"),
        list   = new_node(if (!is.null(object_catalogue[[v]]$parts) && length(object_catalogue[[v]]$parts))
                            list_label(v, object_catalogue[[v]]$parts) else title_line(v, "list"),
                          "lst", "Comes in", "round"),
        new_node(title_line(v, "vector"), "vec", "Comes in", "stadium"))
      if (!"Comes in" %in% group_titles) group_titles <<- c("Comes in", group_titles)
      cur[[v]] <<- list(id = id, label = "")
      cur[[v]]
    }
    edges_from_refs <- function(refs, expr, to, own = NULL) {
      for (r in setdiff(unlist(refs), own)) {
        src <- source_of(r); if (is.null(src)) next
        m <- regmatches(expr, regexec(paste0("^", gsub(".", "\\.", r, fixed = TRUE), "\\$([A-Za-z0-9_.]+)"), expr))[[1]]
        add_edge(src$id, to, if (length(m) == 2L) paste0("$", m[2]) else src$label)
      }
    }
    # Is a result passed on whole later on, rather than only some of its parts?
    passed_whole <- function(v, after) {
      for (s in steps) {
        if (s$line < after) next
        if (s$type == "call") for (a in s$args)
          if (v %in% unlist(a$refs) && !startsWith(a$expr, paste0(v, "$"))) return(TRUE)
        if (s$type == "return" && identical(s$base, v)) return(TRUE)
      }
      v %in% unlist(f$process$returned) ||
        (!is.null(f$process$result_list) && v %in% unlist(f$process$result_list$values))
    }

    for (s in steps) {
      if (s$type == "section") { section <- s$title; next }
      if (s$type == "call") {
        if (s$fn %in% skip) {
          if (!is.null(s$target) && !is.na(s$base) && is.null(cur[[s$base]])) {
            for (a in s$args) for (r in unlist(a$refs)) { src <- source_of(r); if (!is.null(src)) { cur[[s$base]] <- list(id = src$id, label = s$base); break } }
          }
          next
        }
        g <- group_of(s$context)
        fid <- new_node(paste0("<b>", esc(s$fn), "()</b>"), "fn", g)
        links[[fid]] <- s$fn; n_fn <- n_fn + 1L
        for (a in s$args) edges_from_refs(a$refs, a$expr, fid)
        # A call written inside another call's arguments feeds that call
        if (is.null(s$target)) for (p in rev(calls_seen)) {
          if (p$line == s$line) {
            hit <- Filter(function(a) grepl(paste0(s$fn, "("), a$expr, fixed = TRUE), p$args)
            if (length(hit)) { add_edge(fid, p$id, hit[[1]]$name); break }
          }
        }
        calls_seen[[length(calls_seen) + 1L]] <- list(line = s$line, id = fid, args = s$args)
        if (!is.null(s$target) && !is.na(s$base)) {
          callee <- F[[s$fn]]
          if (s$target != s$base) {
            # Part of a list replaced: the list now comes from this call
            cur[[s$base]] <- list(id = fid, label = s$target)
          } else {
            k <- kind_of(s$base, callee)
            if (k == "table") {
              oid <- new_node(table_label(s$base, object_catalogue[[s$base]],
                                          unlist(callee$process$result_columns)), "tbl", g)
              add_edge(fid, oid); cur[[s$base]] <- list(id = oid, label = "")
            } else if (k == "list") {
              fields <- unlist(callee$process$result_fields)
              parts <- if (length(fields)) parts_from_fields(fields) else object_catalogue[[s$base]]$parts %||% list()
              unread <- if (length(unlist(s$used_later)) && !passed_whole(s$base, s$line + 1L))
                          setdiff(names(parts), unlist(s$used_later)) else character(0)
              oid <- new_node(list_label(s$base, parts, unread), "lst", g, "round")
              add_edge(fid, oid); cur[[s$base]] <- list(id = oid, label = "")
            } else if (k == "vector") {
              cur[[s$base]] <- list(id = fid, label = s$base)
            }
          }
        }
        next
      }
      if (s$type == "assign") {
        b <- s$base
        if (is.na(b) || isTRUE(s$is_null) || kind_of(b) == "hidden") next
        refs <- unlist(s$refs)
        known <- Filter(function(r) !is.null(cur[[r]]) || (r %in% formals && kind_of(r) != "hidden"), refs)
        others <- setdiff(known, b)
        whole <- regmatches(s$expr, regexec("^([A-Za-z0-9_.]+)\\$([A-Za-z0-9_.]+)$", s$expr))[[1]]
        # Parts set on the object handed back: out$x <- x
        if (b %in% unlist(f$process$returned) && s$target != b && !is.null(cur[[b]])) {
          for (r in others) { src <- source_of(r); if (!is.null(src)) add_edge(src$id, cur[[b]]$id, sub("^[^$]+\\$", "", s$target)) }
          next
        }
        if (length(whole) == 3L && !is.null(source_of(whole[2])) && s$target == b) {
          cur[[b]] <- list(id = source_of(whole[2])$id, label = paste0("$", whole[3])); next
        }
        if (s$expr %in% known && s$target == b) { cur[[b]] <- source_of(s$expr); next }
        g <- group_of(s$context)
        if (!is.na(s$builds) && length(unlist(s$list_names))) {
          names_ <- unlist(s$list_names); vals <- unlist(s$list_values)
          oid <- if (s$builds == "table")
                   new_node(table_label(b, object_catalogue[[b]] %||% list(unit = "one row"), names_), "tbl", g)
                 else new_node(list_label(b, parts_from_fields(names_)), "lst", g, "round")
          for (i in seq_along(names_)) {
            v <- if (nzchar(vals[i])) vals[i] else NA_character_
            if (!is.na(v)) { src <- source_of(v); if (!is.null(src)) add_edge(src$id, oid, names_[i]) }
          }
          if (!any(nzchar(vals))) edges_from_refs(refs, s$expr, oid, own = b)
          cur[[b]] <- list(id = oid, label = ""); next
        }
        k <- kind_of(b)
        if (k %in% c("table", "list") || (!is.null(cur[[b]]) && !nzchar(cur[[b]]$label))) {
          # A plain working copy of itself is not a step
          plain <- gsub("data\\.table::", "", s$expr)
          if (s$target == b && grepl(paste0("^(setDT|copy|as\\.data\\.table)\\((copy\\()?",
                                            gsub(".", "\\.", b, fixed = TRUE), "\\)\\)?$"), plain)) next
          if (!length(known)) next
          uid <- new_node(paste0("update <b>", esc(b), "</b>"), "inl", g)
          edges_from_refs(refs, s$expr, uid)
          cur[[b]] <- list(id = uid, label = ""); next
        }
        # A vector worked out from earlier results: it travels with its source
        if (length(known)) { src <- source_of(known[[1]]); cur[[b]] <- list(id = src$id, label = b) }
        next
      }
      if (s$type == "inline") {
        t <- s$target; src <- source_of(t)
        if (is.null(src)) next
        g <- group_of(s$context)
        if (last_inline$target == t && last_inline$group == g && identical(src$id, last_inline$id)) next
        iid <- new_node(paste0("change <b>", esc(t), "</b> in place"), "inl", g)
        add_edge(src$id, iid, src$label)
        cur[[t]] <- list(id = iid, label = "")
        last_inline <- list(id = iid, target = t, group = g)
        next
      }
    }

    # What the function hands back
    rl <- f$process$result_list
    ret <- unlist(f$process$returned)
    if (!is.null(rl) && length(unlist(rl$names))) {
      rid <- new_node(list_label("result", parts_from_fields(unlist(rl$names))), "res", "Hands back", "round")
      group_titles <- c(group_titles, "Hands back")
      for (i in seq_along(unlist(rl$names))) {
        v <- unlist(rl$values)[i]
        if (nzchar(v)) { src <- source_of(v); if (!is.null(src)) add_edge(src$id, rid, unlist(rl$names)[i]) }
      }
    } else if (length(ret) && !is.null(cur[[ret[1]]])) {
      node <- nodes[[cur[[ret[1]]]$id]]
      if (identical(node$cls, "lst") && length(unlist(f$process$result_fields)))
        nodes[[cur[[ret[1]]]$id]]$label <- list_label(ret[1], parts_from_fields(unlist(f$process$result_fields)))
      if (node$cls %in% c("lst", "tbl")) nodes[[cur[[ret[1]]]$id]]$cls <- "res"
    }
    if (n_fn == 0L) return(NULL)

    # ---- Mermaid text ----------------------------------------------------
    shape <- function(id, n) {
      l <- paste0('"', n$label, '"')
      switch(n$shape, round = paste0(id, "(", l, ")"), stadium = paste0(id, "([", l, "])"), paste0(id, "[", l, "]"))
    }
    out <- "flowchart TB"
    for (gi in seq_along(group_titles)) {
      gt <- group_titles[gi]
      ids <- names(nodes)[vapply(nodes, function(n) identical(n$group, gt), TRUE)]
      if (!length(ids)) next
      out <- c(out, sprintf('  subgraph G%d["%s"]', gi, esc(gt)))
      out <- c(out, paste0("    ", vapply(ids, function(i) shape(i, nodes[[i]]), "")), "  end")
    }
    loose <- names(nodes)[vapply(nodes, function(n) !nzchar(n$group), TRUE)]
    if (length(loose)) out <- c(out, paste0("  ", vapply(loose, function(i) shape(i, nodes[[i]]), "")))
    for (e in edges) {
      lab <- paste(e$labels, collapse = ", ")
      out <- c(out, if (nzchar(lab)) sprintf('  %s -->|"%s"| %s', e$from, esc(lab), e$to) else sprintf("  %s --> %s", e$from, e$to))
    }
    out <- c(out,
      "  classDef fn fill:#c9eef4,stroke:#2b7a87,color:#10232a",
      "  classDef tbl fill:#dfe8f7,stroke:#4a6a9e,color:#141c2b",
      "  classDef lst fill:#e3f1df,stroke:#4f8a45,color:#16240f",
      "  classDef vec fill:#f3ecd6,stroke:#9a7b25,color:#2a2108",
      "  classDef inl fill:#ffffff,stroke:#6b7280,stroke-dasharray:5 4,color:#1f2937",
      "  classDef res fill:#f6d3c8,stroke:#b4573a,color:#2b130b")
    for (cl in c("fn", "tbl", "lst", "vec", "inl", "res")) {
      ids <- names(nodes)[vapply(nodes, function(n) identical(n$cls, cl), TRUE)]
      if (length(ids)) out <- c(out, sprintf("  class %s %s", paste(ids, collapse = ","), cl))
    }
    list(mermaid = paste(out, collapse = "\n"), links = links, n_functions = n_fn)
  }

  flows <- list()
  for (f in functions) {
    fl <- tryCatch(one_flow(f), error = function(e) {
      message("build_flows: skipped ", f$name, ": ", conditionMessage(e)); NULL })
    if (!is.null(fl)) flows[[f$name]] <- fl
  }
  # Only link a box to a function that has a flow of its own
  for (n in names(flows)) flows[[n]]$links <- Filter(function(x) x %in% names(flows), flows[[n]]$links)
  flows
}

# One Markdown file per flow, which GitHub and VS Code render; the folder is
# rebuilt every run
write_flow_files <- function(flows, functions, dir = "data-raw/schemas/flows") {
  dir.create(dir, showWarnings = FALSE)
  unlink(list.files(dir, pattern = "\\.md$", full.names = TRUE))
  file_of <- function(n) paste0(sub("^\\.", "dot-", gsub("[^A-Za-z0-9_.-]", "_", n)), ".md")
  F <- setNames(functions, vapply(functions, `[[`, "", "name"))
  for (n in names(flows)) {
    f <- F[[n]]
    calls <- unique(unlist(flows[[n]]$links))
    writeLines(c(
      paste0("# `", n, "()`"), "",
      paste0("Generated by `data-raw/schemas/build_code_map.R` from `R/", f$file, "`. Do not edit by hand."), "",
      "```mermaid", flows[[n]]$mermaid, "```", "",
      if (length(calls)) c("Steps with their own flow: ",
                           paste0("- [`", calls, "()`](", vapply(calls, file_of, ""), ")"))
    ), file.path(dir, file_of(n)), useBytes = TRUE)
  }
  top <- intersect(c("simulate_horizon", "prepare_horizon_inputs", "simulate_scenario",
                     "simulate_retirement", "simulate_exits", "simulate_promotions_transfers",
                     "simulate_hiring", "simulate_allowances"), names(flows))
  writeLines(c("# Flow diagrams", "",
               "One diagram per function, generated from the code. Start at the top and follow the links.", "",
               paste0("- [`", top, "()`](", vapply(top, file_of, ""), ")"), "",
               "## All", "", paste0("- [`", sort(names(flows)), "()`](", vapply(sort(names(flows)), file_of, ""), ")")),
             file.path(dir, "README.md"), useBytes = TRUE)
}
