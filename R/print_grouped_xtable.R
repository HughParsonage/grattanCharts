#' Print grouped table
#' @description Writes a table as a LaTeX \code{tabular} where groups are separated by a vertical space and repeated entries of the same entry are omitted.
#' @param dt A \code{data.table} or coercible to such.
#' @param group_by The columns that identify groups. If \code{NULL}, the default, any columns with duplicate entries are used.
#' @param align The character vector passed to \code{xtable}.
#' @param vertical_gap A nominal numeric value for the narrowest vertical gap.
#' @param vertical_gap_units The units of \code{vertical_gap}.
#' @param out.file The file to divert the LaTeX code to.
#' @param overwrite (logical, default: \code{TRUE}) Should \code{out.file} be overwritten? If \code{FALSE}, text is appended to \code{out.file}.
#' @param booktabs (logical, default: \code{TRUE}) Should \code{\\usepackage} style be applied to the table be used?
#' @param tab.environment Which tabular environment should the table be enclosed in. By default \code{tabular}.
#' @param tabularx.width If \code{tab.environment = "tabularx"}, what should the total width of the table be (\emph{i.e.} the first argument of \code{tabularx})?
#' @param logical_fn How should logical columns be reformatted?
#' @param column_format How should column names be formatted? Defaults to the \code{xtable}
#' default, or boldface if that option is \code{NULL}.
#' @param no_space_grep If the value of the group (coerced to character) matches this perl 
#' regular expression, no group space is added.
#' @export print_grouped_xtable

print_grouped_xtable <- function(dt,
                                 group_by = NULL,
                                 align = NULL,
                                 vertical_gap = 0.5,
                                 vertical_gap_units = "baselineskip",
                                 out.file = NULL,
                                 overwrite = TRUE,
                                 booktabs = TRUE,
                                 tab.environment = c("tabular", "tabularx", "longtable"),
                                 tabularx.width = "\\linewidth",
                                 # usepackage{bbding}
                                 logical_fn = c("\\parbox[c]{0.9\\PositionColumnWidth}{\\centering\\XSolidBold}" = FALSE,
                                                "\\parbox[c]{0.9\\PositionColumnWidth}{\\centering\\CheckmarkBold}" = TRUE,
                                                " " = NA),
                                 column_format = getOption("xtable.sanitize.colnames.function", 
                                                           function(x) sprintf("\\textbf{%s}", x)),
                                 caption = NULL,
                                 label = NULL,
                                 no_space_grep = NULL) {
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  dt <- copy(dt)
  
  tab.environment <- match.arg(tab.environment)
  
  if ("_PHANTOM" %chin% names(dt)) {
    stop("`dt` contained a column called '_PHANTOM'. ", 
         "This conflicts with internal objects in `print_grouped_xtable()`. ",
         "Use a different column name.")
  }
  
  if ("_VSPACE" %chin% names(dt)) {
    stop("`dt` contained a column called '_VSPACE'. ", 
         "This conflicts with internal objects in `print_grouped_xtable()`. ",
         "Use a different column name.")
  }
  
  if (nzchar(out.file)) {
    if (overwrite) {
      if (file.remove(out.file) && file.create(out.file)) {
      } else {
        warning("`out.file` was not removed.")
      }
    }
  } else if (is.null(out.file)) {
    out.file <- ""
  }
  
  cat <- function(...) base::cat(..., file = out.file, sep = "", append = TRUE)
  
  dt_orig <- copy(dt)
  
  dt[, "_PHANTOM" := "\\phantom{.}"]
  hutils::set_cols_first(dt, "_PHANTOM")
  dt[, "_VSPACE" := 0]
  
  numeric_cols <- vapply(dt, is.numeric, logical(1L))
  
  headers <- names(dt)
  
  column_format <- match.fun(column_format)
  
  formatted_headers <-
    c("", column_format(headers[headers %notin% c("_PHANTOM", "_VSPACE")]), "")

  # Idea is to add vertical space between groups by placing more vspace 
  # at the start of every non duplicated group; less space at the
  # start of every duplicated group -- the more duplicated the less.
  if (is.null(group_by)) {
    uniqueNs <- vapply(dt, uniqueN, integer(1L))
    setcolorder(dt, names(sort(uniqueNs)))
    message("`dt` set to new column order: ")
    group_by <- headers[headers %notin% c("_PHANTOM", "_VSPACE")]
  }
  
  # Reverse because group_by should be in order
  for (group_j in rev(seq_along(group_by))) {
    
    if (group_j == 1L) {
      dt[duplicated(dt, by = group_by[group_j]), "_VSPACE" := eval(parse(text = "`_VSPACE`")) - 1L]
      dt[duplicated(dt, by = group_by[group_j]), (group_by[group_j]) := NA]
    } else {
      dt[,
         (group_by[group_j]) := replace(.SD[[1]], duplicated(.SD), NA),
         by = c(group_by[seq_len(group_j - 1L)]), 
         .SDcols = group_by[group_j]]
    }
  }
  dt[, "_VSPACE" := eval(parse(text = "`_VSPACE`")) - min(eval(parse(text = "`_VSPACE`")))]
  if (!is.null(no_space_grep)) {
    dt[, "_VSPACE" := rep_len(!grepl(no_space_grep, .BY[[1L]], perl = TRUE), .N) * `_VSPACE`,
       by = c(group_by)]
  }
  

  if (!is.null(caption) && tab.environment != "longtable") {
    # longtable captions are within the environment
    cat("\\caption{", caption, "}",
        if (!is.null(label)) {
          paste0("\\label{", label, "}")
        },
        "\n")
  }
  
  # begin
  switch(tab.environment,
         "tabular"   = cat("\\begin{tabular}"),
         "longtable" = cat("\\begin{longtable}"),
         "tabularx"  = cat("\\begin{tabularx}", "{", tabularx.width, "}"))
  
  cat("{", "@{}c@{}", align, "@{}c@{}", "}")
  cat("\n")
  
  if (!is.null(caption) && tab.environment == "longtable") {
    cat("\\caption{", caption, "}",
        if (!is.null(label)) {
          paste0("\\label{", label, "}")
        },
        "\\\\ \n")
  }    
  if (booktabs) {
    cat("\\toprule", "\n")
  }
  
  

  
  max_nchar <- function(x) {
    y <- coalesce(as.character(x), "")
    max(nchar(y))
  }
  
  format_widths <- vapply(dt, max_nchar, integer(1L)) + 2L # 1 either side
  
  for (j in seq_len(ncol(dt))) {
    if (j == 1L) {
      cat(" ")
    } else if (j == ncol(dt)) {
      cat("&\\\\", " ", "\n")
    } else {
      cat(" & ", formatC(formatted_headers[j], width = format_widths[j]))
    }
  }
  
  if (tab.environment == "longtable") {
    if (booktabs) {
      cat("\\midrule", "\n")
    } else {
      cat("\\hline", "\n")
    }
    # first head
    cat("\\endfirsthead", "\n")
    
    if (!is.null(caption)) {
      cat("\\caption*{", 
          "Table ", 
          if (!is.null(label)) {
            paste0("\\label{", label, "}")
          },
          caption, "}",
          "\\\\ \n")
    }
    cat("\\toprule", "\n")
    for (j in seq_len(ncol(dt))) {
      if (j == 1L) {
        cat(" ")
      } else if (j == ncol(dt)) {
        cat("&\\\\", " ", "\n")
      } else {
        cat(" & ", formatC(formatted_headers[j], width = format_widths[j]))
      }
    }
    if (booktabs) {
      cat("\\midrule", "\n")
    } else {
      cat("\\hline", "\n")
    }
    cat("\\endhead", "\n")
  }
  
  
  
  if (tab.environment == "longtable") {
    if (booktabs) {
      cat("\\bottomrule", "\n")
    }
    cat("\\endfoot", "\n")
  }
  
  
  
  
  for (i in seq_len(nrow(dt))) {
    for (j in seq_len(ncol(dt))) {
      cell <- .subset2(dt, j)[[i]]
      if (j == 1L) {
        VAL <- .subset2(dt, "_VSPACE")[i]
        if (VAL > 0 && i > 1L) {
          cat("\\phantom{.} &")
          for (k in seq_len(ncol(dt) - 2L)) {
            cat(formatC("&", width = format_widths[j]))
          }
          rm(k)
          cat("\\tabularnewline[", formatC(VAL * vertical_gap), "\\", vertical_gap_units, "]")
          cat("\n")
        } 
        cat("\\phantom{.} & ")
        
      } else if (j < ncol(dt)) {
        cell_char <- coalesce(as.character(cell), "")
        cat(formatC(cell_char, width = format_widths[j]), " &")
      } else if (j == ncol(dt)) {
        # Don't add a new row at the end of the table
        if (i < nrow(dt)) {
          cat("\\tabularnewline", "\\relax", " ", "\n")
        } else {
          cat("\n")
        }
      }
    }
  }
  
  # With longtable, the bottomrule is allocated by \endfoot
  if (tab.environment != "longtable") {
    cat("\\bottomrule\n")
  }
  
  
  switch(tab.environment,
         "tabular"   = cat("\\end{tabular}"),
         "longtable" = cat("\\end{longtable}"),
         "tabularx"  = cat("\\end{tabularx}"))
  cat("\n")
  # Needs to be invisible if knitting to avoid 
  # printing the 'asis' output (which will likely
  # be invalid LaTeX anyway)
  if_knitting(invisible(dt), dt)
  
}



