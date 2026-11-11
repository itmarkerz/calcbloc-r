# Live checks against https://calcbloc.com/api/v1. They are skipped on CRAN, offline, and unless
# CALCBLOC_LIVE_TESTS=true, because anonymous use of the API is limited to 50 requests a day per IP.

skip_live <- function() {
  skip_on_cran()
  skip_if_offline("calcbloc.com")
  skip_if_not(identical(Sys.getenv("CALCBLOC_LIVE_TESTS"), "true"), "Set CALCBLOC_LIVE_TESTS=true to run live API tests")
}

# A used-up daily quota is not a package failure: skip instead.
live <- function(expr) {
  tryCatch(expr, calcbloc_rate_limited = function(e) skip(paste("API rate limit reached:", conditionMessage(e))))
}

test_that("the API serves every bundled country", {
  skip_live()
  api <- live(cb_countries(source = "api"))
  expect_true(all(cb_countries()$country %in% api$country))
})

test_that("the live UK table has the same income tax bands as the bundled one", {
  skip_live()
  online <- live(cb_tax_table("uk", "2026-27", source = "api"))
  bundled <- cb_tax_table("uk", "2026-27")
  sec <- "income-tax-england"
  expect_equal(as.data.frame(online[online$section_id == sec, c("from", "to", "rate")]),
               as.data.frame(bundled[bundled$section_id == sec, c("from", "to", "rate")]),
               ignore_attr = TRUE)
})

test_that("a regional table can be fetched", {
  skip_live()
  ca <- live(cb_tax_table("us", region = "ca", source = "api"))
  expect_true(all(ca$region == "ca"))
  expect_gt(nrow(ca), 0)
})
