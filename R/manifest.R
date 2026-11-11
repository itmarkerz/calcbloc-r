cb_env <- new.env(parent = emptyenv())

extdata_path <- function(...) {
  path <- system.file("extdata", ..., package = "calcbloc")
  if (!nzchar(path)) stop("Bundled file not found: ", file.path(...), call. = FALSE)
  path
}

bundled_manifest <- function() {
  if (is.null(cb_env$manifest)) cb_env$manifest <- read_json_gz(extdata_path("manifest.json.gz"))
  cb_env$manifest
}

#' The manifest of the bundled CalcBloc data
#'
#' Lists every bundled country with its tax years, regions, tax-table page,
#' API URL, date last verified and the official sources behind it, as published
#' in `tables/manifest.json` of the CalcBloc open dataset.
#'
#' @return A list of class `cb_manifest` with elements `generatedAt` (date of
#'   the data), `licence`, `credit`, `countries` (one list per country),
#'   `pending` (countries held back while their figures are checked) and
#'   `not_bundled` (countries in the dataset that calcbloc.com does not serve
#'   yet).
#' @seealso [cb_countries()] for the same information as a data frame.
#' @export
#' @examples
#' m <- cb_manifest()
#' m
#' m$generatedAt
#' m$countries[[1]]$sourceUrls[1:3]
cb_manifest <- function() {
  structure(bundled_manifest(), class = c("cb_manifest", "list"))
}

#' @export
format.cb_manifest <- function(x, ...) {
  countries <- vapply(x$countries, function(c) c$country, "")
  pending <- vapply(or_else(x$pending, list()), function(p) chr1(p$country), "")
  c(
    sprintf("<CalcBloc data manifest> generated %s, %d countries", chr1(x$generatedAt), length(countries)),
    paste("Countries:", collapse(countries, 30)),
    if (length(pending)) paste("Pending (not yet published):", paste(pending, collapse = ", ")),
    paste0(cb_credit_text, ", ", chr1(x$licence), ": ", cb_data_url)
  )
}

#' @export
print.cb_manifest <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

manifest_country <- function(country) {
  country <- tolower(check_string(country, "country"))
  m <- bundled_manifest()
  for (c in m$countries) {
    if (identical(c$country, country) || identical(tolower(c$name), country)) return(c)
  }
  for (c in or_else(m$pending, list())) {
    if (identical(c$country, country)) {
      stop("No published tables for '", country, "' yet: ", chr1(c$reason), call. = FALSE)
    }
  }
  codes <- vapply(m$countries, function(c) c$country, "")
  stop("No bundled tables for '", country, "'. Countries: ", paste(codes, collapse = ", "),
       ". See cb_countries(); cb_countries(source = \"api\") lists what the API serves.", call. = FALSE)
}

resolve_year <- function(info, year) {
  years <- unlist(info$years)
  if (is.null(year)) return(info$current_year)
  if (length(year) != 1 || is.na(year)) stop("`year` must be a single value, such as 2026 or \"2026-27\".", call. = FALSE)
  y <- as.character(year)
  if (identical(tolower(y), "latest")) return(info$current_year)
  if (y %in% years) return(y)
  if (grepl("^[0-9]{4}$", y)) {
    hit <- years[startsWith(years, paste0(y, "-"))]
    if (length(hit) == 1) return(hit)
  }
  stop("No ", info$name, " tables for year '", y, "'. Years: ", paste(years, collapse = ", "), ".", call. = FALSE)
}

resolve_region <- function(info, region) {
  region <- tolower(check_string(region, "region"))
  regions <- or_else(info$regions, list())
  if (!length(regions)) stop(info$name, " tax tables have no regional variants.", call. = FALSE)
  for (r in regions) {
    if (region %in% tolower(c(r$code, r$slug, r$name))) return(r$code)
  }
  stop("Unknown region '", region, "' for ", info$name, ". Regions: ",
       paste(vapply(regions, function(r) r$code, ""), collapse = ", "), ".", call. = FALSE)
}

#' Countries with CalcBloc tax tables
#'
#' One row per country: currency, tax years, number of regional tables, the
#' date the figures were last verified against official sources and the
#' matching page on calcbloc.com.
#'
#' @param source `"bundled"` (default) reads the manifest shipped with the
#'   package and works offline. `"api"` asks the CalcBloc API
#'   (`GET /countries`), which also says which countries support the
#'   calculation endpoints.
#' @return A data frame. For `source = "bundled"`: `country`, `name`,
#'   `currency`, `current_year`, `years`, `regions`, `rows`,
#'   `last_verified` (Date), `page_url`, `api_url`. For `source = "api"`:
#'   `country`, `name`, `currency`, `current_year`, `years`, `regions`,
#'   `tables_only`, `endpoints`, `page_url`.
#' @export
#' @examples
#' cb_countries()
#' \donttest{
#' # Online: what the API serves today (counts towards the API rate limit)
#' if (curl::has_internet()) try(cb_countries(source = "api"))
#' }
cb_countries <- function(source = c("bundled", "api")) {
  source <- match.arg(source)
  if (source == "api") return(api_countries())
  cs <- bundled_manifest()$countries
  data.frame(
    country = vapply(cs, function(c) c$country, ""),
    name = vapply(cs, function(c) c$name, ""),
    currency = vapply(cs, function(c) chr1(c$currency), ""),
    current_year = vapply(cs, function(c) chr1(c$current_year), ""),
    years = vapply(cs, function(c) paste(unlist(c$years), collapse = ", "), ""),
    regions = vapply(cs, function(c) length(or_else(c$regions, list())), integer(1)),
    rows = vapply(cs, function(c) as.integer(or_else(c$rowCount, NA_integer_)), integer(1)),
    last_verified = as_date(vapply(cs, function(c) chr1(c$lastVerified), "")),
    page_url = vapply(cs, function(c) chr1(c$pageUrl), ""),
    api_url = vapply(cs, function(c) chr1(c$apiUrl), ""),
    stringsAsFactors = FALSE
  )
}

#' Regions with their own tax tables
#'
#' US states, Canadian provinces and territories, Spanish autonomous
#' communities, Italian regions, Swiss cantons and so on. Regional tables are
#' not bundled; fetch them with `cb_tax_table(country, region = code,
#' source = "api")`.
#'
#' @param country Country code or name, as in [cb_countries()].
#' @return A data frame with `country`, `code`, `name` and `slug`; zero rows
#'   when the country has no regional tables.
#' @export
#' @examples
#' head(cb_regions("canada"))
cb_regions <- function(country) {
  info <- manifest_country(country)
  rs <- or_else(info$regions, list())
  data.frame(
    country = rep(info$country, length(rs)),
    code = vapply(rs, function(r) chr1(r$code), ""),
    name = vapply(rs, function(r) chr1(r$name), ""),
    slug = vapply(rs, function(r) chr1(r$slug), ""),
    stringsAsFactors = FALSE
  )
}
