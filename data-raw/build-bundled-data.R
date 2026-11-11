# Rebuild the tax tables bundled in inst/extdata from the calcbloc-data repository.
#
# Usage, from the package root:
#   Rscript data-raw/build-bundled-data.R [path/to/calcbloc-data/tables]
#
# The source directory defaults to the CALCBLOC_DATA_DIR environment variable, then to
# ../calcbloc-data/tables (a sibling clone of https://github.com/itmarkerz/calcbloc-data).
#
# What it writes:
#   inst/extdata/manifest.json.gz                  countries, years, regions, sources, dates
#   inst/extdata/tables/<country>/<year>.json.gz   one national table per country and tax year
#
# Only national tables are bundled, to keep the package small (CRAN asks for less than 5 MB).
# Regional tables (US states, Canadian provinces, ...) are fetched on demand from the API with
# cb_tax_table(..., region = , source = "api").
#
# Only countries that calcbloc.com serves are bundled, so every page_url in the package works.
# The live list comes from the public API (GET /api/v1/countries, one request). If the API is
# unreachable, each country's tax-table page is checked with a HEAD request instead.
#
# Each table keeps every top-level field and every row of the published JSON; the "sections"
# array keeps only the section metadata (id, title, group, kind, basis, verified), because the
# same figures are in "rows".

local({
  args <- commandArgs(trailingOnly = TRUE)
  src <- if (length(args) >= 1) args[[1]] else Sys.getenv("CALCBLOC_DATA_DIR", "../calcbloc-data/tables")
  if (!file.exists(file.path(src, "manifest.json"))) {
    stop("No manifest.json in '", src, "'. Pass the calcbloc-data 'tables' directory as the first argument.")
  }
  if (!file.exists("DESCRIPTION") || !any(grepl("^Package: calcbloc$", readLines("DESCRIPTION")))) {
    stop("Run this script from the calcbloc package root.")
  }

  read_json <- function(path) {
    txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
    jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
  }
  write_json_gz <- function(x, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    txt <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
    con <- gzfile(path, open = "wb", compression = 9)
    on.exit(close(con))
    writeLines(enc2utf8(as.character(txt)), con, useBytes = TRUE)
  }

  manifest <- read_json(file.path(src, "manifest.json"))
  codes <- vapply(manifest$countries, function(c) c$country, "")

  live_from_api <- function() {
    h <- curl::new_handle(useragent = "calcbloc-r data-raw (+https://github.com/itmarkerz/calcbloc-r)")
    res <- curl::curl_fetch_memory("https://calcbloc.com/api/v1/countries", handle = h)
    if (res$status_code != 200) stop("HTTP ", res$status_code)
    body <- jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
    vapply(body$countries, function(c) c$code, "")
  }
  live_from_pages <- function() {
    ok <- vapply(manifest$countries, function(c) {
      h <- curl::new_handle(nobody = TRUE, followlocation = TRUE,
                            useragent = "calcbloc-r data-raw (+https://github.com/itmarkerz/calcbloc-r)")
      res <- tryCatch(curl::curl_fetch_memory(c$pageUrl, handle = h), error = function(e) NULL)
      !is.null(res) && res$status_code == 200
    }, logical(1))
    codes[ok]
  }
  live <- tryCatch(live_from_api(), error = function(e) {
    message("API country list unavailable (", conditionMessage(e), "); checking tax-table pages instead.")
    live_from_pages()
  })
  keep <- codes %in% live
  excluded <- codes[!keep]
  if (length(excluded)) message("Not served on calcbloc.com yet, so not bundled: ", paste(excluded, collapse = ", "))

  out_dir <- file.path("inst", "extdata")
  unlink(file.path(out_dir, "tables"), recursive = TRUE)

  bundled <- list()
  for (c in manifest$countries[keep]) {
    for (year in unlist(c$years)) {
      tab <- read_json(file.path(src, c$country, paste0(year, ".json")))
      if (!is.null(tab$region)) stop("Expected a national table for ", c$country, " ", year)
      tab$sections <- lapply(tab$sections, function(s) {
        s[intersect(c("id", "title", "group", "kind", "basis", "verified"), names(s))]
      })
      write_json_gz(tab, file.path(out_dir, "tables", c$country, paste0(year, ".json.gz")))
    }
    c$files <- NULL
    bundled[[length(bundled) + 1]] <- c
  }

  manifest$countries <- bundled
  manifest$not_bundled <- lapply(excluded, function(x) list(country = x, reason = "Not yet served on calcbloc.com"))
  manifest$bundledAt <- format(Sys.Date())
  write_json_gz(manifest, file.path(out_dir, "manifest.json.gz"))

  files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  message(sprintf("Wrote %d files, %.0f KB, for %d countries (data generated %s).",
                  length(files), sum(file.size(files)) / 1024, length(bundled), manifest$generatedAt))
})
