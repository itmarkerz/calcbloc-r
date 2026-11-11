cb_table_attrs <- c("country", "country_name", "tax_year", "year_label", "region", "region_name",
                    "currency", "effective_from", "effective_to", "data_status", "page_url",
                    "data_source", "retrieved")

#' Tax tables for a country and tax year
#'
#' Returns every income tax band, threshold, allowance, employee contribution
#' and employer rate CalcBloc publishes for a country and tax year, one row per
#' band or value, each with its official source, the date it was retrieved and
#' its verification status.
#'
#' Rates are decimals (`0.2` is 20%). Money amounts are in the table's
#' currency (`attr(x, "currency")`) and, unless the section's `basis` says
#' otherwise, per year.
#'
#' @param country Country code or name, as in [cb_countries()] (for example
#'   `"us"`, `"uk"`, `"new-zealand"` or `"Germany"`).
#' @param year Tax year. `NULL` (default) or `"latest"` gives the current year.
#'   Give either the year as listed in [cb_countries()] (`"2026-27"`) or the
#'   calendar year it starts in (`2026`).
#' @param region Optional region code, slug or name (see [cb_regions()]): a US
#'   state, Canadian province, UK nation (Scotland, Wales) and so on. Regional
#'   tables hold only the regional figures and are fetched from the API, so
#'   `region` needs `source = "api"`.
#' @param source `"bundled"` (default) reads the tables shipped with the package
#'   (offline, national tables only). `"api"` fetches the latest version from
#'   the CalcBloc API; see [cb_api_get()] for rate limits and keys.
#' @param api_key API key for `source = "api"`; see [cb_api_key()]. Optional.
#'
#' @return A data frame of class `cb_tax_table` with columns:
#' \describe{
#'   \item{country, region}{Country code and region code (`NA` for national tables).}
#'   \item{tax_year}{Tax year of the row.}
#'   \item{section_id, section, group}{Section the row belongs to; `group` is
#'     `"income-tax"`, `"employee"`, `"employer"` or `"other"`.}
#'   \item{kind}{`"bands"` for a progressive schedule (use `from`, `to`,
#'     `rate`), `"values"` for single figures (use `value`, `rate` or `text`).}
#'   \item{label}{What the figure is.}
#'   \item{from, to, rate}{Band edges and rate; `to` is `NA` for the top band.}
#'   \item{value, text}{A numeric figure, or a text figure where `unit` is `"text"`.}
#'   \item{unit}{`"rate"`, `"money"`, `"number"` or `"text"`.}
#'   \item{basis, note}{What the band edges or value apply to, and notes.}
#'   \item{effective_from, effective_to}{Dates the figure applies (Date; `NA`
#'     `effective_to` means in force until changed).}
#'   \item{source_title, source_url}{The official publication the figure comes from.}
#'   \item{retrieved_at}{Date the source was retrieved (Date).}
#'   \item{status}{`"verified"`, `"under_review"` or `"not_applicable"`.}
#' }
#' Attributes hold the country name, currency, page URL on calcbloc.com, the
#' table's overall status and where it came from.
#'
#' @section Credit:
#' Data by CalcBloc (calcbloc.com/data), CC BY 4.0. Printing a table shows the
#' credit and the link to its page on calcbloc.com.
#'
#' @seealso [cb_sections()] to list a table's sections, [cb_tax_from_bands()]
#'   to apply a band section to incomes.
#' @export
#' @examples
#' uk <- cb_tax_table("uk")
#' uk
#' cb_sections(uk)
#'
#' # One section, as a plain data frame
#' us <- cb_tax_table("us", 2026)
#' us[us$section_id == "federal-income-tax-single", c("from", "to", "rate", "source_url")]
#'
#' # The previous UK tax year is bundled too
#' cb_tax_table("uk", "2025-26")
#'
#' @examplesIf interactive() && curl::has_internet()
#' # Regional tables come from the API (counts towards the rate limit)
#' cb_tax_table("us", region = "ca", source = "api")
cb_tax_table <- function(country, year = NULL, region = NULL, source = c("bundled", "api"),
                         api_key = cb_api_key()) {
  source <- match.arg(source)
  if (source == "api") return(api_tax_table(country, year, region, api_key))
  info <- manifest_country(country)
  y <- resolve_year(info, year)
  if (!is.null(region)) {
    code <- resolve_region(info, region)
    stop("Regional tables are not bundled with the package, to keep it small. Fetch them from the API: ",
         sprintf("cb_tax_table(\"%s\", \"%s\", region = \"%s\", source = \"api\")", info$country, y, code),
         call. = FALSE)
  }
  raw <- read_json_gz(extdata_path("tables", info$country, paste0(y, ".json.gz")))
  as_cb_tax_table(raw, data_source = "bundled", retrieved = chr1(bundled_manifest()$generatedAt))
}

# Converts a parsed tax-table JSON document (bundled file or API response) to a cb_tax_table.
as_cb_tax_table <- function(raw, data_source, retrieved) {
  rows <- or_else(raw$rows, list())
  sections <- or_else(raw$sections, list())
  sec_ids <- vapply(sections, function(s) chr1(s$id), "")
  sec_kind <- vapply(sections, function(s) chr1(s$kind), "")
  value_raw <- lapply(rows, function(r) r[["value"]])
  region_code <- if (is.list(raw$region)) chr1(raw$region$code) else chr1(raw$region)
  region_name <- if (is.list(raw$region)) chr1(raw$region$name) else NA_character_
  n <- length(rows)

  df <- data.frame(
    country = rep(chr1(raw$country), n),
    region = rep(region_code, n),
    tax_year = pluck_chr(rows, "tax_year"),
    section_id = pluck_chr(rows, "section_id"),
    section = pluck_chr(rows, "section"),
    group = pluck_chr(rows, "group"),
    kind = rep(NA_character_, n),
    label = pluck_chr(rows, "label"),
    from = pluck_num(rows, "from"),
    to = pluck_num(rows, "to"),
    rate = pluck_num(rows, "rate"),
    value = vapply(value_raw, function(v) if (is.numeric(v)) as.numeric(v) else NA_real_, numeric(1)),
    text = vapply(value_raw, function(v) if (is.character(v)) v else NA_character_, character(1)),
    unit = pluck_chr(rows, "unit"),
    basis = pluck_chr(rows, "basis"),
    note = pluck_chr(rows, "note"),
    effective_from = as_date(pluck_chr(rows, "effective_from")),
    effective_to = as_date(pluck_chr(rows, "effective_to")),
    source_title = pluck_chr(rows, "source_title"),
    source_url = pluck_chr(rows, "source_url"),
    retrieved_at = as_date(pluck_chr(rows, "retrieved_at")),
    status = pluck_chr(rows, "status"),
    stringsAsFactors = FALSE
  )
  if (n) {
    df$kind <- sec_kind[match(df$section_id, sec_ids)]
    # Fall back on the rows themselves if a section is missing from the metadata.
    missing <- is.na(df$kind)
    if (any(missing)) {
      has_band <- tapply(!is.na(df$from), df$section_id, any)
      df$kind[missing] <- ifelse(has_band[df$section_id[missing]], "bands", "values")
    }
  }
  structure(
    df,
    class = c("cb_tax_table", "data.frame"),
    country = chr1(raw$country),
    country_name = chr1(raw$countryName),
    tax_year = chr1(raw$year),
    year_label = chr1(or_else(raw$yearLabel, raw$year)),
    region = region_code,
    region_name = region_name,
    currency = chr1(raw$currency),
    effective_from = as_date(chr1(raw$effectiveFrom)),
    effective_to = as_date(chr1(raw$effectiveTo)),
    data_status = chr1(raw$status),
    page_url = chr1(raw$pageUrl),
    data_source = data_source,
    retrieved = retrieved
  )
}

#' @export
`[.cb_tax_table` <- function(x, ...) {
  out <- NextMethod()
  if (!is.data.frame(out)) return(out)
  if (all(c("section_id", "label") %in% names(out))) {
    for (a in cb_table_attrs) attr(out, a) <- attr(x, a)
    class(out) <- class(x)
  } else {
    class(out) <- "data.frame"
  }
  out
}

#' @export
as.data.frame.cb_tax_table <- function(x, ...) {
  for (a in cb_table_attrs) attr(x, a) <- NULL
  class(x) <- "data.frame"
  x
}

#' @export
format.cb_tax_table <- function(x, ...) {
  a <- function(name) or_else(attr(x, name, exact = TRUE), NA)
  place <- a("country_name")
  if (!is.na(a("region_name"))) place <- paste0(place, ", ", a("region_name"))
  nsec <- length(unique(x$section_id))
  status <- a("data_status")
  src <- switch(as.character(a("data_source")),
                bundled = sprintf("bundled with calcbloc (data of %s)", a("retrieved")),
                api = sprintf("CalcBloc API (fetched %s)", a("retrieved")),
                "unknown")
  c(
    sprintf("<CalcBloc tax table> %s %s (%s)", place, a("year_label"), a("currency")),
    sprintf("%d rows in %d sections; table status: %s; source: %s", nrow(x), nsec, status, src),
    if (identical(status, "unverified")) "Some figures in this table are still being checked against official sources: see the status column.",
    sprintf("%s, CC BY 4.0: %s", cb_credit_text, cb_utm(if (is.na(a("page_url"))) cb_data_url else a("page_url")))
  )
}

#' @export
print.cb_tax_table <- function(x, n = 10, ...) {
  header <- format(x)
  cat(header[-length(header)], sep = "\n")
  cols <- intersect(c("section_id", "label", "from", "to", "rate", "value", "text", "unit", "status"), names(x))
  shown <- as.data.frame(x)[seq_len(min(n, nrow(x))), cols, drop = FALSE]
  # Leave out columns that are empty in the rows shown, and shorten long strings.
  for (col in intersect(c("from", "to", "rate", "value", "text"), cols)) {
    if (all(is.na(shown[[col]]))) shown[[col]] <- NULL
  }
  clip <- function(v, width) {
    long <- !is.na(v) & nchar(v) > width
    v[long] <- paste0(substr(v[long], 1, width - 1), "~")
    v
  }
  if ("section_id" %in% names(shown)) shown$section_id <- clip(shown$section_id, 24)
  if ("label" %in% names(shown)) shown$label <- clip(shown$label, 34)
  if ("text" %in% names(shown)) shown$text <- clip(shown$text, 20)
  if (nrow(shown)) print(shown, row.names = FALSE)
  if (nrow(x) > n) cat(sprintf("# ... %d more rows. All columns: as.data.frame(x); sections: cb_sections(x)\n", nrow(x) - n))
  cat(header[length(header)], "\n", sep = "")
  invisible(x)
}

#' Sections of a tax table
#'
#' Summarises a [cb_tax_table()] result: one row per section with its kind,
#' number of rows, basis and official source.
#'
#' @param table A `cb_tax_table` (or a data frame with the same columns).
#' @param kind Optional filter: `"bands"` or `"values"`.
#' @return A data frame with `section_id`, `section`, `group`, `kind`, `rows`,
#'   `basis` and `source_url`.
#' @export
#' @examples
#' cb_sections(cb_tax_table("australia"), kind = "bands")
cb_sections <- function(table, kind = NULL) {
  if (!is.data.frame(table) || !all(c("section_id", "section", "kind") %in% names(table))) {
    stop("`table` must be a tax table from cb_tax_table().", call. = FALSE)
  }
  t <- as.data.frame(table)
  first <- !duplicated(t$section_id)
  out <- data.frame(
    section_id = t$section_id[first],
    section = t$section[first],
    group = t$group[first],
    kind = t$kind[first],
    rows = as.integer(table(factor(t$section_id, levels = t$section_id[first]))),
    basis = t$basis[first],
    source_url = t$source_url[first],
    stringsAsFactors = FALSE
  )
  if (!is.null(kind)) out <- out[out$kind %in% kind, , drop = FALSE]
  rownames(out) <- NULL
  out
}
