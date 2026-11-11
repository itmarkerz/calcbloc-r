bands <- data.frame(from = c(0, 10000, 40000), to = c(10000, 40000, NA), rate = c(0, 0.2, 0.4))

test_that("cb_tax_from_bands() is vectorised and handles edge cases", {
  expect_equal(cb_tax_from_bands(c(0, 5000, 10000, 20000, 40000, 50000), bands),
               c(0, 0, 0, 2000, 6000, 10000))
  expect_equal(cb_tax_from_bands(-100, bands), 0)
  expect_equal(cb_tax_from_bands(c(a = 20000, b = NA), bands), c(a = 2000, b = NA))
  expect_equal(cb_tax_from_bands(numeric(0), bands), numeric(0))
})

test_that("`above` stacks income on top of other income", {
  # 5,000 on top of 38,000: 2,000 at 20% + 3,000 at 40%
  expect_equal(cb_tax_from_bands(5000, bands, above = 38000), 2000 * 0.2 + 3000 * 0.4)
  expect_equal(cb_tax_from_bands(c(5000, 5000), bands, above = c(0, 38000)), c(0, 1600))
  # Stacking equals the difference of the totals.
  expect_equal(cb_tax_from_bands(5000, bands, above = 38000),
               cb_tax_from_bands(43000, bands) - cb_tax_from_bands(38000, bands))
  expect_error(cb_tax_from_bands(1:3, bands, above = 1:2), "same length")
  expect_error(cb_tax_from_bands(1, bands, above = -1), "negative")
  expect_error(cb_tax_from_bands("1", bands), "numeric")
})

test_that("bands are sorted and validated", {
  shuffled <- bands[c(3, 1, 2), ]
  expect_equal(cb_tax_from_bands(50000, shuffled), 10000)
  expect_error(cb_tax_from_bands(1, data.frame(from = 10, to = 5, rate = 0.1)), "below")
  expect_error(cb_tax_from_bands(1, data.frame(a = 1)), "from")
  expect_error(cb_tax_from_bands(1, list(from = 0, rate = 0.1)), "data frame")
})

test_that("section selection works on tax tables", {
  us <- cb_tax_table("us", 2026)
  expect_error(cb_tax_from_bands(1000, us), "several sections")
  expect_error(cb_tax_from_bands(1000, us, "nope"), "federal-income-tax-single")
  expect_error(cb_tax_from_bands(1000, us, "standard-deduction"), "single values")
  single <- us[us$section_id == "federal-income-tax-single", ]
  expect_equal(cb_tax_from_bands(75000, single), cb_tax_from_bands(75000, us, "federal-income-tax-single"))
})

test_that("cb_band_breakdown() adds up to cb_tax_from_bands()", {
  br <- cb_band_breakdown(c(20000, 50000), bands)
  expect_equal(nrow(br), 6)
  expect_named(br, c("income", "band", "label", "from", "to", "rate", "amount", "tax"))
  expect_equal(as.numeric(tapply(br$tax, br$income, sum)), c(2000, 10000))
  expect_equal(br$amount[br$income == 50000], c(10000, 30000, 10000))
})

test_that("cb_marginal_rate() returns the rate of the band an income falls in", {
  expect_equal(cb_marginal_rate(c(0, 10000, 10000.01, 40000, 40001, NA), bands), c(0, 0, 0.2, 0.2, 0.4, NA))
  au <- cb_tax_table("australia")
  expect_equal(cb_marginal_rate(c(18200, 18201, 250000), au, "resident-rates"), c(0, 0.15, 0.45))
})
