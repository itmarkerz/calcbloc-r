cb_user_agent <- function() {
  sprintf("calcbloc-r/%s (+https://github.com/itmarkerz/calcbloc-r)", utils::packageVersion("calcbloc"))
}

#' CalcBloc API key
#'
#' Reads the API key from the `CALCBLOC_API_KEY` environment variable. A key is
#' optional for the tax-table and country endpoints: without one, the API
#' allows 50 requests a day and 5 a minute per IP address; a free key
#' (<https://calcbloc.com/developers>) raises that to 5,000 a day and 60 a
#' minute. Keep the key out of scripts: put `CALCBLOC_API_KEY=...` in your
#' `.Renviron` (for example with `usethis::edit_r_environ()`).
#'
#' @return The key as a string, or `NULL` when the variable is unset or empty.
#' @export
#' @examples
#' is.null(cb_api_key())
cb_api_key <- function() {
  key <- Sys.getenv("CALCBLOC_API_KEY", unset = "")
  if (nzchar(trimws(key))) trimws(key) else NULL
}

# The one place that touches the network, so tests can mock it.
cb_http_get <- function(url, headers, timeout) {
  h <- curl::new_handle(timeout = timeout, connecttimeout = min(timeout, 15), followlocation = TRUE,
                        useragent = cb_user_agent())
  curl::handle_setheaders(h, .list = headers)
  res <- curl::curl_fetch_memory(url, handle = h)
  hdr <- curl::parse_headers_list(res$headers)
  list(status = res$status_code, headers = hdr, body = rawToChar(res$content))
}

api_condition <- function(message, class, ...) {
  structure(class = c(class, "calcbloc_api_error", "error", "condition"),
            list(message = message, call = NULL, ...))
}

header_int <- function(headers, name) {
  v <- headers[[name]]
  if (is.null(v)) return(NA_integer_)
  suppressWarnings(as.integer(v[[1]]))
}

build_query <- function(query) {
  query <- query[!vapply(query, is.null, logical(1))]
  if (!length(query)) return("")
  if (is.null(names(query)) || any(!nzchar(names(query)))) stop("All query parameters must be named.", call. = FALSE)
  vals <- vapply(query, function(v) {
    if (length(v) != 1 || is.na(v)) stop("Query parameters must be single, non-missing values.", call. = FALSE)
    if (is.logical(v)) return(if (v) "true" else "false")
    if (is.numeric(v)) return(format(v, scientific = FALSE, trim = TRUE, digits = 15))
    as.character(v)
  }, character(1))
  paste0("?", paste(curl::curl_escape(names(query)), curl::curl_escape(vals), sep = "=", collapse = "&"))
}

throttle <- function() {
  gap <- getOption("calcbloc.min_interval", 1)
  last <- cb_env$last_request
  if (!is.null(last) && is.numeric(gap) && gap > 0) {
    wait <- gap - as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (wait > 0) Sys.sleep(wait)
  }
  cb_env$last_request <- Sys.time()
}

#' Call the CalcBloc API
#'
#' Low-level GET request to the CalcBloc public API
#' (<https://calcbloc.com/developers>), returning the parsed JSON. Most users
#' want [cb_tax_table()] with `source = "api"`, [cb_countries()],
#' [cb_net_salary()], [cb_employer_cost()] or [cb_compare()] instead.
#'
#' @section Rate limits and polite use:
#' Without a key the API allows 50 requests a day and 5 a minute per IP
#' address; a free key allows 5,000 a day and 60 a minute. The client
#' \itemize{
#'   \item keeps at least `getOption("calcbloc.min_interval", 1)` seconds
#'     between requests;
#'   \item caches successful responses for the R session (clear with
#'     [cb_cache_clear()]), so repeating a call does not use up the quota;
#'   \item on HTTP 429 or 503 waits for the `Retry-After` the server sends and
#'     retries, up to `getOption("calcbloc.max_retries", 2)` times, as long as
#'     the wait is at most `getOption("calcbloc.max_wait", 60)` seconds;
#'     otherwise it stops with an error of class `calcbloc_rate_limited`;
#'   \item records the `X-RateLimit-*` headers of the last response, which
#'     [cb_rate_limit()] returns.
#' }
#' API errors are raised as conditions of class `calcbloc_api_error` (with
#' fields `status`, `code` and `docs`); network failures as
#' `calcbloc_network_error`.
#'
#' @param path Endpoint path below the base URL, for example `"/countries"`.
#' @param query Named list of query parameters; `NULL` entries are dropped and
#'   logical values are sent as `true`/`false`.
#' @param api_key API key, sent as the `X-API-Key` header; see [cb_api_key()].
#' @param cache Use the session cache (default `TRUE`).
#' @return The parsed JSON as a list (JSON `null` becomes `NULL`). Every
#'   response includes an `attribution` element; show "Data by CalcBloc" with a
#'   link wherever you display the figures.
#' @export
#' @examplesIf interactive() && curl::has_internet()
#' res <- cb_api_get("/tax-tables/new-zealand/latest")
#' res$yearLabel
#' cb_rate_limit()
cb_api_get <- function(path, query = list(), api_key = cb_api_key(), cache = TRUE) {
  path <- check_string(path, "path")
  if (!startsWith(path, "/")) path <- paste0("/", path)
  base <- sub("/+$", "", getOption("calcbloc.base_url", "https://calcbloc.com/api/v1"))
  url <- paste0(base, path, build_query(query))
  headers <- c(Accept = "application/json")
  if (!is.null(api_key)) headers <- c(headers, `X-API-Key` = check_string(api_key, "api_key"))
  cache_key <- paste(url, !is.null(api_key))
  if (isTRUE(cache) && !is.null(cb_env$cache[[cache_key]])) return(cb_env$cache[[cache_key]])

  timeout <- getOption("calcbloc.timeout", 30)
  max_wait <- getOption("calcbloc.max_wait", 60)
  retries <- getOption("calcbloc.max_retries", 2)
  attempt <- 0
  repeat {
    throttle()
    res <- tryCatch(cb_http_get(url, headers, timeout), error = function(e) {
      stop(structure(class = c("calcbloc_network_error", "error", "condition"),
                     list(message = paste0("Could not reach the CalcBloc API (", conditionMessage(e), ")."), call = NULL)))
    })
    hdr <- res$headers
    names(hdr) <- tolower(names(hdr))
    cb_env$rate_limit <- list(
      limit = header_int(hdr, "x-ratelimit-limit"),
      remaining = header_int(hdr, "x-ratelimit-remaining"),
      reset_seconds = header_int(hdr, "x-ratelimit-reset"),
      retry_after = header_int(hdr, "retry-after"),
      status = res$status,
      time = Sys.time()
    )
    if (res$status %in% c(429, 503) && attempt < retries) {
      wait <- header_int(hdr, "retry-after")
      if (is.na(wait)) wait <- if (res$status == 503) 30 else 60
      if (wait <= max_wait) {
        attempt <- attempt + 1
        message(sprintf("CalcBloc API %s; waiting %d s before retrying (attempt %d of %d).",
                        if (res$status == 429) "rate limit reached" else "temporarily unavailable",
                        wait, attempt, retries))
        Sys.sleep(wait)
        next
      }
    }
    break
  }

  body <- tryCatch(jsonlite::fromJSON(res$body, simplifyVector = FALSE), error = function(e) NULL)
  if (res$status >= 400) {
    err <- if (is.list(body)) body$error else NULL
    code <- chr_or(err$code, paste0("http_", res$status))
    msg <- chr_or(err$message, paste("HTTP", res$status))
    docs <- chr_or(err$docs, "https://calcbloc.com/developers")
    if (res$status == 429) {
      ra <- header_int(hdr, "retry-after")
      hint <- if (is.null(api_key)) " A free API key raises the limit: https://calcbloc.com/developers (set CALCBLOC_API_KEY)." else ""
      stop(api_condition(sprintf("CalcBloc API rate limit reached: %s%s%s", msg,
                                 if (!is.na(ra)) sprintf(" Retry after %d seconds.", ra) else "", hint),
                         c("calcbloc_rate_limited"), status = res$status, code = code, docs = docs, retry_after = ra))
    }
    stop(api_condition(sprintf("CalcBloc API error %d (%s): %s See %s", res$status, code, msg, docs),
                       paste0("calcbloc_", gsub("[^a-z0-9]+", "_", tolower(code))),
                       status = res$status, code = code, docs = docs))
  }
  if (!is.list(body)) stop(api_condition("The CalcBloc API returned a response that is not JSON.", "calcbloc_bad_response",
                                         status = res$status, code = "bad_response", docs = NA_character_))
  rem <- cb_env$rate_limit$remaining
  if (!is.na(rem) && rem <= 5) {
    message(sprintf("CalcBloc API: %d requests left in today's quota%s.", rem,
                    if (is.null(api_key)) " (a free API key raises the limit: https://calcbloc.com/developers)" else ""))
  }
  if (isTRUE(cache)) {
    if (is.null(cb_env$cache)) cb_env$cache <- list()
    cb_env$cache[[cache_key]] <- body
  }
  body
}

#' Rate-limit status of the last API response
#'
#' @return `NULL` before the first request of the session, otherwise a list
#'   with `limit` (requests a day), `remaining`, `reset_seconds` (until the
#'   daily window resets), `retry_after` (after a 429), the HTTP `status` and
#'   the `time` of the response.
#' @export
#' @examples
#' cb_rate_limit()
cb_rate_limit <- function() cb_env$rate_limit

#' Clear the session cache of API responses
#'
#' @return `NULL`, invisibly.
#' @export
#' @examples
#' cb_cache_clear()
cb_cache_clear <- function() {
  cb_env$cache <- NULL
  invisible(NULL)
}

api_tax_table <- function(country, year, region, api_key) {
  country <- tolower(check_string(country, "country"))
  y <- if (is.null(year)) "latest" else as.character(year)
  info <- tryCatch(manifest_country(country), error = function(e) NULL)
  if (!is.null(info)) {
    country <- info$country
    if (!is.null(year)) y <- tryCatch(resolve_year(info, year), error = function(e) y)
    if (!is.null(region)) region <- resolve_region(info, region)
  } else if (!is.null(region)) {
    region <- tolower(check_string(region, "region"))
  }
  raw <- cb_api_get(sprintf("/tax-tables/%s/%s", curl::curl_escape(country), curl::curl_escape(y)),
                    list(region = region), api_key = api_key)
  as_cb_tax_table(raw, data_source = "api", retrieved = format(Sys.Date()))
}

api_countries <- function(api_key = cb_api_key()) {
  cs <- cb_api_get("/countries", api_key = api_key)$countries
  data.frame(
    country = vapply(cs, function(c) chr1(c$code), ""),
    name = vapply(cs, function(c) chr1(c$name), ""),
    currency = vapply(cs, function(c) chr1(c$currency), ""),
    current_year = vapply(cs, function(c) chr1(c$current_year), ""),
    years = vapply(cs, function(c) paste(unlist(c$years_available), collapse = ", "), ""),
    regions = vapply(cs, function(c) length(or_else(c$endpoints[["tax-tables"]]$regions, list())), integer(1)),
    tables_only = vapply(cs, function(c) isTRUE(c$tables_only), logical(1)),
    endpoints = vapply(cs, function(c) paste(unlist(c$available_endpoints), collapse = ", "), ""),
    page_url = vapply(cs, function(c) chr1(c$endpoints[["tax-tables"]]$pageUrl), ""),
    stringsAsFactors = FALSE
  )
}

calc_params <- function(...) {
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Extra parameters in `...` must be named, for example `region = \"ca\"`.", call. = FALSE)
  }
  dots
}

check_salary <- function(salary) {
  if (!is.numeric(salary) || length(salary) != 1 || is.na(salary) || salary < 0) {
    stop("`salary` must be a single non-negative number (one API request per salary).", call. = FALSE)
  }
  salary
}

new_cb_result <- function(body) structure(body, class = c("cb_result", "list"))

#' Take-home pay from the CalcBloc API
#'
#' Calls `GET /net-salary`, which runs the same engine as the take-home pay
#' calculators on calcbloc.com. Supported for about a dozen countries
#' (`cb_countries(source = "api")` lists them in `endpoints`); for the others
#' the API returns an `unsupported_country` error pointing to their tax tables.
#'
#' The API documents the calculation endpoints as needing a free key; set
#' `CALCBLOC_API_KEY` (see [cb_api_key()]).
#'
#' @param country Country code, for example `"uk"` or `"us"`.
#' @param salary Gross annual salary in the country's currency (one value per
#'   call: each call is one API request).
#' @param ... Further named parameters for the endpoint, such as `region`,
#'   `filing_status`, `pension_pct` or `frequency`. The accepted names and
#'   values depend on the country and are listed by
#'   `cb_api_get("/countries")` and at <https://calcbloc.com/developers>.
#' @param year Tax year; `NULL` for the current one.
#' @param api_key API key; see [cb_api_key()].
#' @return A list of class `cb_result` holding the parsed response:
#'   `summary`, `results`, `breakdown`, `notes`, `inputs`, `calculatorUrl` and
#'   `attribution`. `as.data.frame()` flattens `summary` and `results` into
#'   one row, so several calls can be bound with `rbind()`.
#' @seealso [cb_employer_cost()], [cb_compare()]
#' @export
#' @examplesIf interactive() && curl::has_internet()
#' pay <- cb_net_salary("uk", 50000, region = "scotland")
#' pay
#' as.data.frame(pay)
cb_net_salary <- function(country, salary, ..., year = NULL, api_key = cb_api_key()) {
  q <- c(list(country = tolower(check_string(country, "country")), salary = check_salary(salary), year = year),
         calc_params(...))
  new_cb_result(cb_api_get("/net-salary", q, api_key = api_key))
}

#' Employer cost from the CalcBloc API
#'
#' Calls `GET /employer-cost`: the total cost of employing someone on a salary
#' (employer social security, payroll taxes and similar), as on the employer
#' cost calculators on calcbloc.com.
#'
#' @inheritParams cb_net_salary
#' @param ... Further named parameters for the endpoint, such as `region`,
#'   `size` or `pension_pct`; see <https://calcbloc.com/developers>.
#' @return A list of class `cb_result`; see [cb_net_salary()].
#' @export
#' @examplesIf interactive() && curl::has_internet()
#' cb_employer_cost("germany", 60000)
cb_employer_cost <- function(country, salary, ..., year = NULL, api_key = cb_api_key()) {
  q <- c(list(country = tolower(check_string(country, "country")), salary = check_salary(salary), year = year),
         calc_params(...))
  new_cb_result(cb_api_get("/employer-cost", q, api_key = api_key))
}

#' Compare take-home pay in two countries
#'
#' Calls `GET /compare`: converts the salary at the European Central Bank
#' reference rate and runs each country's take-home pay engine, as on
#' <https://calcbloc.com/compare/salary>.
#'
#' @param a,b Country codes of the two countries.
#' @param salary Gross annual salary, in `currency`.
#' @param currency Currency of `salary`: the currency of `a` (default) or `b`.
#' @param ... Further named parameters: `us_state`, `uk_region`,
#'   `ca_province`, `in_state`.
#' @inheritParams cb_net_salary
#' @return A list of class `cb_result` with elements `a`, `b`,
#'   `exchangeRate`, `difference` and `interpretation`.
#' @export
#' @examplesIf interactive() && curl::has_internet()
#' cb_compare("uk", "germany", 60000)
cb_compare <- function(a, b, salary, currency = NULL, ..., api_key = cb_api_key()) {
  q <- c(list(a = tolower(check_string(a, "a")), b = tolower(check_string(b, "b")),
              salary = check_salary(salary), currency = currency), calc_params(...))
  new_cb_result(cb_api_get("/compare", q, api_key = api_key))
}

fmt_num <- function(x, digits = 2) {
  if (is.null(x) || !is.numeric(x)) return("NA")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

#' @export
format.cb_result <- function(x, ...) {
  credit <- sprintf("%s: %s", cb_credit_text, cb_utm(chr_or(x$attribution$url, "https://calcbloc.com")))
  if (identical(x$endpoint, "compare")) {
    side <- function(s) sprintf("  %s: gross %s %s, net %s, effective rate %s%%", toupper(chr1(s$country)),
                                fmt_num(s$gross), chr1(s$currency), fmt_num(s$net), fmt_num(100 * num1(s$effectiveRate), 1))
    return(c("<CalcBloc compare>", side(x$a), side(x$b),
             if (!is.null(x$interpretation)) paste0("  ", chr1(x$interpretation)), credit))
  }
  s <- x$summary
  lines <- c(
    sprintf("<CalcBloc %s> %s %s (%s)", chr_or(x$endpoint, "result"),
            chr_or(x$countryName, chr1(x$country)), chr_or(x$year, ""), chr_or(x$currency, ""))
  )
  if (is.list(s)) {
    for (nm in names(s)) {
      v <- s[[nm]]
      if (is.numeric(v)) lines <- c(lines, sprintf("  %-18s %s", nm, if (grepl("Rate$", nm)) paste0(fmt_num(100 * v, 2), "%") else fmt_num(v)))
    }
  }
  if (!is.null(x$calculatorUrl)) lines <- c(lines, paste("  Calculator:", cb_utm(chr1(x$calculatorUrl))))
  c(lines, credit)
}

#' @export
print.cb_result <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
as.data.frame.cb_result <- function(x, ...) {
  flat <- function(l, prefix) {
    if (!is.list(l)) return(list())
    keep <- l[vapply(l, function(v) is.atomic(v) && length(v) == 1, logical(1))]
    if (length(keep)) names(keep) <- paste0(prefix, names(keep))
    keep
  }
  if (identical(x$endpoint, "compare")) {
    row <- c(flat(x$a, "a_"), flat(x$b, "b_"), flat(x$difference, "difference_"))
  } else {
    row <- c(list(country = chr1(x$country), year = chr1(x$year), currency = chr1(x$currency)),
             flat(x$summary, ""), flat(x$results, "results_"))
  }
  as.data.frame(row, stringsAsFactors = FALSE)
}
