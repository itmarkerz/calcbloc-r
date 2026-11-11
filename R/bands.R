band_schedule <- function(table, section) {
  if (!is.data.frame(table)) stop("`table` must be a data frame from cb_tax_table() or a data frame of bands.", call. = FALSE)
  if (!all(c("from", "rate") %in% names(table))) {
    stop("`table` needs `from` and `rate` columns (and optionally `to`).", call. = FALSE)
  }
  t <- as.data.frame(table)
  has_sections <- "section_id" %in% names(t)
  if (!is.null(section)) {
    section <- check_string(section, "section")
    if (!has_sections) stop("`table` has no section_id column; leave `section` out.", call. = FALSE)
    if (!section %in% t$section_id) {
      band_ids <- unique(t$section_id[!is.na(t$from) & !is.na(t$rate)])
      stop("No section '", section, "' in this table. Band sections: ",
           if (length(band_ids)) paste(band_ids, collapse = ", ") else "none", ".", call. = FALSE)
    }
    t <- t[t$section_id == section, , drop = FALSE]
  } else if (has_sections && length(unique(t$section_id)) > 1) {
    band_ids <- unique(t$section_id[!is.na(t$from) & !is.na(t$rate)])
    stop("`table` has several sections; choose one with `section`. Band sections: ",
         paste(band_ids, collapse = ", "), ".", call. = FALSE)
  }
  if ("kind" %in% names(t) && any(!is.na(t$kind) & t$kind != "bands")) {
    stop("Section '", t$section_id[1], "' holds single values, not bands; only \"bands\" sections can be applied to incomes.",
         call. = FALSE)
  }
  t <- t[!is.na(t$from) & !is.na(t$rate), , drop = FALSE]
  if (!nrow(t)) stop("No bands (rows with `from` and `rate`) in this section.", call. = FALSE)
  to <- if ("to" %in% names(t)) t$to else rep(NA_real_, nrow(t))
  if (any(!is.na(to) & to < t$from)) stop("A band has `to` below `from`.", call. = FALSE)
  o <- order(t$from)
  list(
    label = if ("label" %in% names(t)) t$label[o] else rep(NA_character_, nrow(t)),
    from = as.numeric(t$from[o]),
    to = as.numeric(to[o]),
    rate = as.numeric(t$rate[o])
  )
}

recycle_income <- function(income, above) {
  if (!is.numeric(income)) stop("`income` must be numeric.", call. = FALSE)
  if (!is.numeric(above)) stop("`above` must be numeric.", call. = FALSE)
  if (any(above < 0, na.rm = TRUE)) stop("`above` must not be negative.", call. = FALSE)
  n <- max(length(income), length(above))
  if (!length(income) || !length(above)) return(list(income = numeric(0), above = numeric(0)))
  if (!(length(income) %in% c(1, n)) || !(length(above) %in% c(1, n))) {
    stop("`income` and `above` must have the same length, or length 1.", call. = FALSE)
  }
  list(income = rep_len(as.numeric(income), n), above = rep_len(as.numeric(above), n))
}

#' Apply a progressive band schedule to incomes
#'
#' Each slice of income is taxed at the rate of the band it falls in, and the
#' slices are added up: the arithmetic of progressive income tax brackets.
#' Vectorised over `income` (and `above`).
#'
#' This applies the bands and nothing else. A country's allowances, credits,
#' tapers, surcharges, filing rules and rounding are not applied, so read the
#' section's `basis` (shown by [cb_sections()]) to see which amount the bands
#' apply to: for example "Taxable income above the Personal Allowance, per
#' year" for the UK. The result is not rounded. It is not tax advice; the API's
#' [cb_net_salary()] runs the full calculation for supported countries.
#'
#' @param income Numeric vector of amounts the bands apply to, in the table's
#'   currency and period. Zero or negative amounts give zero; `NA` gives `NA`.
#' @param table A [cb_tax_table()] result, or any data frame with `from`, `to`
#'   (`NA` for an open top band) and `rate` columns.
#' @param section The `section_id` of a band section, for example
#'   `"federal-income-tax-single"`. Can be left out when `table` holds a
#'   single section.
#' @param above Income already counted below `income`, which uses up the lower
#'   bands first (default 0). Use it to stack one kind of income on top of
#'   another, as UK dividends sit on top of other taxable income.
#' @return A numeric vector the length of `income` (names kept).
#' @seealso [cb_band_breakdown()] for the amount and tax in each band,
#'   [cb_marginal_rate()] for the rate of the band an income falls in.
#' @export
#' @examples
#' us <- cb_tax_table("us", 2026)
#' cb_tax_from_bands(c(25000, 50000, 75000, 150000), us, "federal-income-tax-single")
#'
#' # UK: bands apply to income above the Personal Allowance (GOV.UK example:
#' # GBP 35,000 with the standard allowance pays 20% on GBP 22,430 = GBP 4,486)
#' uk <- cb_tax_table("uk", "2026-27")
#' cb_tax_from_bands(35000 - 12570, uk, "income-tax-england")
#'
#' # Stacking: GBP 3,000 of dividends on GBP 17,000 of other taxable income, after
#' # the GBP 500 dividend allowance (GOV.UK "Tax on dividends" example: GBP 268.75)
#' cb_tax_from_bands(2500, uk, "dividends", above = 17000 + 500)
#'
#' # Any data frame of bands works
#' bands <- data.frame(from = c(0, 10000), to = c(10000, NA), rate = c(0, 0.25))
#' cb_tax_from_bands(c(5000, 20000), bands)
cb_tax_from_bands <- function(income, table, section = NULL, above = 0) {
  b <- band_schedule(table, section)
  x <- recycle_income(income, above)
  start <- x$above
  end <- x$above + pmax(x$income, 0)
  tax <- numeric(length(end))
  for (i in seq_along(b$from)) {
    upper <- if (is.na(b$to[i])) Inf else b$to[i]
    tax <- tax + pmax(0, pmin(end, upper) - pmax(start, b$from[i])) * b$rate[i]
  }
  if (length(tax) == length(income)) names(tax) <- names(income)
  tax
}

#' Split incomes across the bands of a schedule
#'
#' For each income, the amount that falls in each band and the tax on it.
#'
#' @inheritParams cb_tax_from_bands
#' @return A data frame with one row per income and band: `income`, `band`
#'   (band number), `label`, `from`, `to`, `rate`, `amount` (income in the
#'   band) and `tax`. Bands an income does not reach have `amount` 0.
#' @export
#' @examples
#' ie <- cb_tax_table("ireland")
#' # Revenue's "Calculating your USC" example 2: EUR 50,000 pays EUR 1,032.82
#' usc <- cb_band_breakdown(50000, ie, "usc")
#' usc
#' sum(usc$tax)
cb_band_breakdown <- function(income, table, section = NULL, above = 0) {
  b <- band_schedule(table, section)
  x <- recycle_income(income, above)
  nb <- length(b$from)
  ni <- length(x$income)
  start <- rep(x$above, each = nb)
  end <- rep(x$above + pmax(x$income, 0), each = nb)
  lower <- rep(b$from, times = ni)
  upper <- rep(ifelse(is.na(b$to), Inf, b$to), times = ni)
  amount <- pmax(0, pmin(end, upper) - pmax(start, lower))
  rate <- rep(b$rate, times = ni)
  data.frame(
    income = rep(x$income, each = nb),
    band = rep(seq_len(nb), times = ni),
    label = rep(b$label, times = ni),
    from = lower,
    to = rep(b$to, times = ni),
    rate = rate,
    amount = amount,
    tax = amount * rate,
    stringsAsFactors = FALSE
  )
}

#' Marginal rate at an income
#'
#' The rate of the band each income falls in. A band covers amounts above its
#' `from` up to and including its `to` (the usual "over X but not over Y"
#' wording); amounts at or below the first band's lower edge get the first
#' band's rate.
#'
#' @inheritParams cb_tax_from_bands
#' @return A numeric vector of rates (decimals) the length of `income`.
#' @export
#' @examples
#' au <- cb_tax_table("australia")
#' cb_marginal_rate(c(18200, 18201, 60000, 250000), au, "resident-rates")
cb_marginal_rate <- function(income, table, section = NULL) {
  b <- band_schedule(table, section)
  if (!is.numeric(income)) stop("`income` must be numeric.", call. = FALSE)
  out <- rep(b$rate[1], length(income))
  for (i in seq_along(b$from)[-1]) out[!is.na(income) & income > b$from[i]] <- b$rate[i]
  out[is.na(income)] <- NA_real_
  names(out) <- names(income)
  out
}
