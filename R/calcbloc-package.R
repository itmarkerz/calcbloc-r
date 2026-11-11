#' calcbloc: income tax brackets and social security rates from official sources
#'
#' Tidy data frames of income tax brackets, social security contribution rates,
#' thresholds and allowances, with the official source, retrieval date and
#' verification status on every row.
#'
#' * Offline: national tables for the current tax year ship with the package.
#'   Start with [cb_countries()] and [cb_tax_table()].
#' * Online: [cb_tax_table()] with `source = "api"` fetches the latest tables,
#'   including regional ones (US states, Canadian provinces, Swiss cantons, ...),
#'   from the CalcBloc public API. [cb_net_salary()], [cb_employer_cost()] and
#'   [cb_compare()] call its calculation endpoints.
#' * Arithmetic: [cb_tax_from_bands()], [cb_band_breakdown()] and
#'   [cb_marginal_rate()] apply a progressive band schedule to a vector of incomes.
#'
#' @section Data licence and credit:
#' The tax data is published by CalcBloc at <https://calcbloc.com/data> under the
#' Creative Commons Attribution 4.0 International licence (CC BY 4.0). When you
#' publish the data or results based on it, credit
#' "Data by CalcBloc (calcbloc.com/data)" ([cb_credit()] returns this text).
#' The package code is MIT licensed.
#'
#' @section Not tax advice:
#' The tables report published rates and thresholds. The band helpers apply a
#' schedule and nothing else: allowances, credits, tapers, surcharges and the
#' authorities' rounding rules are not applied.
#'
#' @section Options:
#' * `calcbloc.base_url`: API base URL (default `"https://calcbloc.com/api/v1"`).
#' * `calcbloc.timeout`: request timeout in seconds (default 30).
#' * `calcbloc.min_interval`: minimum seconds between API requests (default 1).
#' * `calcbloc.max_wait`: longest `Retry-After` wait, in seconds, the client
#'   accepts before giving up on a rate-limited request (default 60).
#' * `calcbloc.max_retries`: retries after HTTP 429 or 503 (default 2).
#'
#' @keywords internal
"_PACKAGE"
