#!/usr/bin/env bash
# PostToolUse hook: after Claude edits a file, catch mistakes that otherwise
# only show up in R CMD check or the pkgdown build.
#   *.R           -> must parse; in R/, code (not comments) must be ASCII
#   DESCRIPTION   -> must parse as DCF
#   _pkgdown.yml  -> must parse as YAML
# Exit 2 feeds stderr back to Claude; anything else is silent.
set -u

input=$(cat)
command -v Rscript >/dev/null 2>&1 || exit 0
file=$(printf '%s' "$input" | Rscript -e '
  x <- jsonlite::fromJSON(file("stdin"))
  f <- x$tool_input$file_path
  cat(if (is.null(f)) "" else f)' 2>/dev/null)

[ -z "${file:-}" ] && exit 0
[ -f "$file" ] || exit 0

case "$file" in
  *.R|*.r)
    if ! out=$(Rscript -e 'invisible(parse(file = commandArgs(TRUE)[1]))' "$file" 2>&1); then
      echo "R parse error in $file:" >&2
      echo "$out" >&2
      exit 2
    fi
    case "$file" in
      */R/*.R)
        # R CMD check warns on non-ASCII characters in package code.
        # Comments (and roxygen) may contain them; strings and code may not.
        if out=$(Rscript -e '
          lines <- readLines(commandArgs(TRUE)[1], warn = FALSE, encoding = "UTF-8")
          code <- sub("#.*$", "", lines)
          bad <- which(grepl("[^\\x01-\\x7F]", code, perl = TRUE))
          if (length(bad)) {
            cat(sprintf("line %d: %s", bad, trimws(lines[bad])), sep = "\n")
            quit(status = 1)
          }' "$file" 2>&1); then
          :
        else
          echo "Non-ASCII characters in R code in $file (use \\uXXXX escapes):" >&2
          echo "$out" >&2
          exit 2
        fi
        ;;
    esac
    ;;
  */DESCRIPTION)
    if ! out=$(Rscript -e 'invisible(read.dcf(commandArgs(TRUE)[1]))' "$file" 2>&1); then
      echo "DESCRIPTION does not parse:" >&2
      echo "$out" >&2
      exit 2
    fi
    ;;
  */_pkgdown.yml)
    if ! out=$(Rscript -e 'invisible(yaml::read_yaml(commandArgs(TRUE)[1]))' "$file" 2>&1); then
      echo "_pkgdown.yml does not parse:" >&2
      echo "$out" >&2
      exit 2
    fi
    ;;
esac
exit 0
