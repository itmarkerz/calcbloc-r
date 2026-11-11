# Worked examples published by tax authorities, reproduced from the bundled tables. The same
# examples are used by the calcbloc.com test suite; each test cites its official source.

value_of <- function(table, section, label) {
  r <- table[table$section_id == section & table$label == label, ]
  stopifnot(nrow(r) == 1)
  if (!is.na(r$value)) r$value else r$rate
}

test_that("Ireland USC, Revenue 'Calculating your USC' examples 1, 2 and 4 (2026)", {
  # https://www.revenue.ie/en/jobs-and-pensions/usc/calculating-usc.aspx
  ie <- cb_tax_table("ireland", 2026)

  # Example 1, Jacob, EUR 25,000: 12,012 x 0.5% = 60.06; 12,988 x 2% = 259.76; total 319.82
  jacob <- cb_band_breakdown(25000, ie, "usc")
  expect_equal(round(jacob$tax, 2), c(60.06, 259.76, 0, 0))
  expect_equal(round(cb_tax_from_bands(25000, ie, "usc"), 2), 319.82)

  # Example 2, Sadhbh, EUR 50,000: 60.06 + 333.76 + 21,300 x 3% = 639.00; total 1,032.82
  sadhbh <- cb_band_breakdown(50000, ie, "usc")
  expect_equal(round(sadhbh$tax, 2), c(60.06, 333.76, 639, 0))
  expect_equal(round(sum(sadhbh$tax), 2), 1032.82)

  # Example 4, Cian (75), EUR 75,000: over EUR 60,000, so standard rates; total 2,030.62
  cian <- cb_band_breakdown(75000, ie, "usc")
  expect_equal(round(cian$tax, 2), c(60.06, 333.76, 1240.32, 396.48))
  expect_equal(round(sum(cian$tax), 2), 2030.62)

  # Vectorised over all three
  expect_equal(round(cb_tax_from_bands(c(25000, 50000, 75000), ie, "usc"), 2), c(319.82, 1032.82, 2030.62))
})

test_that("Ireland USC, Revenue example 3: full medical card, reduced rates (2026)", {
  # https://www.revenue.ie/en/jobs-and-pensions/usc/calculating-usc.aspx
  # Donnchadh, EUR 50,000 with a full medical card: 60.06 + 37,988 x 2% = 759.76; total 819.82
  ie <- cb_tax_table("ireland", 2026)
  expect_equal(round(cb_band_breakdown(50000, ie, "usc-reduced")$tax, 2), c(60.06, 759.76))
  expect_equal(round(cb_tax_from_bands(50000, ie, "usc-reduced"), 2), 819.82)
})

test_that("UK Income Tax, GOV.UK example: GBP 35,000 pays 20% on GBP 22,430 = GBP 4,486 (2026-27)", {
  # https://www.gov.uk/income-tax-rates
  uk <- cb_tax_table("uk", "2026-27")
  pa <- value_of(uk, "personal-allowance", "Personal Allowance")
  expect_equal(pa, 12570)
  expect_equal(cb_tax_from_bands(35000 - pa, uk, "income-tax-england"), 4486)
})

test_that("UK dividend tax, GOV.UK 'Tax on dividends' example (2026-27)", {
  # https://www.gov.uk/tax-on-dividends
  # GBP 29,570 wages + GBP 3,000 dividends. Taxable wages 29,570 - 12,570 = 17,000 at 20% = 3,400.
  # The first GBP 500 of dividends is the dividend allowance (taxed at 0% but it uses up the band);
  # the other GBP 2,500 is taxed at the 10.75% ordinary rate = 268.75. Total 3,668.75.
  uk <- cb_tax_table("uk", "2026-27")
  pa <- value_of(uk, "personal-allowance", "Personal Allowance")
  wages_tax <- cb_tax_from_bands(29570 - pa, uk, "income-tax-england")
  dividend_tax <- cb_tax_from_bands(3000 - 500, uk, "dividends", above = (29570 - pa) + 500)
  expect_equal(wages_tax, 3400)
  expect_equal(dividend_tax, 268.75)
  expect_equal(wages_tax + dividend_tax, 3668.75)
})

test_that("US federal income tax, IRS 2026 Tax Table rows (Publication 1040 draft)", {
  # https://www.irs.gov/pub/irs-dft/p1040--dft.pdf (2026 Tax Table, early-release draft)
  # The Tax Table gives the tax on the midpoint of each $50 row, rounded to the whole dollar.
  # Its own example: married filing jointly, taxable income $25,300 -> $2,543.
  us <- cb_tax_table("us", 2026)
  irs_round <- function(x) floor(x + 0.5 + 1e-6)
  table_tax <- function(at_least, status) {
    irs_round(cb_tax_from_bands(at_least + 25, us, paste0("federal-income-tax-", status)))
  }
  expect_equal(table_tax(25300, "mfj"), 2543)

  # Rows of the printed table: at least, single, married filing jointly, separately, head of household.
  rows <- rbind(
    c(21900, 2383, 2193, 2383, 2277),
    c(46900, 5383, 5135, 5383, 5277),
    c(71900, 10536, 8135, 10536, 8725),
    c(96900, 16036, 11135, 16036, 14225),
    c(99950, 16707, 11501, 16707, 14896)
  )
  for (i in seq_len(nrow(rows))) {
    got <- vapply(c("single", "mfj", "mfs", "hoh"), function(s) table_tax(rows[i, 1], s), numeric(1))
    expect_equal(unname(got), rows[i, 2:5], info = paste("row", rows[i, 1]))
  }
  # Qualifying surviving spouses use the married-filing-jointly column.
  expect_equal(table_tax(71900, "qss"), 8135)
})

test_that("Canada federal tax, CRA T4127 (January 2026) chapter 4 bonus example", {
  # https://www.canada.ca/content/dam/cra-arc/formspubs/pub/t4127-jan/t4127-01-26e.pdf
  # Weekly pay $1,000 with a $2,500 bonus. Basic federal tax T3 = R x A - K - K1 - K2 - K4, where
  # R x A - K is the band arithmetic on annual taxable income A.
  ca <- cb_tax_table("canada", 2026)
  credit_rate <- value_of(ca, "federal-credits", "Credit rate on personal amounts (lowest rate)")
  k1 <- credit_rate * value_of(ca, "federal-credits", "Basic personal amount: maximum")
  k4 <- credit_rate * value_of(ca, "federal-credits", "Canada employment amount")
  expect_equal(round(k1, 2), 2303.28)
  expect_equal(round(k4, 2), 210.14)

  # Step 1: A = $55,450.76, K2 = $491.63 -> T3 = $4,758.06
  t3_step1 <- cb_tax_from_bands(55450.76, ca, "federal-income-tax") - k1 - 491.63 - k4
  # Step 2: A = $52,975.28, K2 = $468.60 -> T3 = $4,434.52
  t3_step2 <- cb_tax_from_bands(52975.28, ca, "federal-income-tax") - k1 - 468.60 - k4
  expect_equal(round(t3_step1, 2), 4758.06)
  expect_equal(round(t3_step2, 2), 4434.52)
  # Tax on the bonus TB = $323.54
  expect_equal(round(t3_step1 - t3_step2, 2), 323.54)
})
