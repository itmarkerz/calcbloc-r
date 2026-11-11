# API client tests against recorded responses (tests/testthat/fixtures); no network is used.

test_that("cb_api_key() reads CALCBLOC_API_KEY", {
  local_api()
  expect_null(cb_api_key())
  Sys.setenv(CALCBLOC_API_KEY = "  abc123  ")
  expect_equal(cb_api_key(), "abc123")
  Sys.setenv(CALCBLOC_API_KEY = "")
  expect_null(cb_api_key())
})

test_that("cb_tax_table(source = 'api') builds the URL, resolves the region and parses the rows", {
  local_api()
  seen <- NULL
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    seen <<- list(url = url, headers = headers)
    fake_response(fixture("tax-tables-uk-latest-scotland.json"))
  })
  t <- cb_tax_table("uk", region = "Scotland", source = "api")
  expect_equal(seen$url, "https://calcbloc.com/api/v1/tax-tables/uk/latest?region=scotland")
  expect_false("X-API-Key" %in% names(seen$headers))
  expect_s3_class(t, "cb_tax_table")
  expect_equal(nrow(t), 9)
  expect_true(all(t$region == "scotland"))
  expect_equal(attr(t, "data_source"), "api")
  expect_equal(attr(t, "region_name"), "Scotland")
  expect_equal(attr(t, "page_url"), "https://calcbloc.com/uk/tax-tables/2026-27/scotland")
  expect_true(all(t$kind %in% c("bands", "values")))
  expect_output(print(t), "United Kingdom, Scotland 2026/27 (GBP)", fixed = TRUE)
  expect_output(print(t), "https://calcbloc.com/uk/tax-tables/2026-27/scotland?utm_source=calcbloc-r&utm_medium=app&utm_campaign=itmarkerz-oss", fixed = TRUE)
})

test_that("a year given as a start year is resolved before calling the API", {
  local_api()
  seen <- NULL
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    seen <<- url
    fake_response(fixture("tax-tables-uk-latest-scotland.json"))
  })
  cb_tax_table("uk", 2026, region = "scotland", source = "api")
  expect_equal(seen, "https://calcbloc.com/api/v1/tax-tables/uk/2026-27?region=scotland")
})

test_that("the API key is sent as X-API-Key", {
  local_api()
  Sys.setenv(CALCBLOC_API_KEY = "test-key")
  seen <- NULL
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    seen <<- headers
    fake_response(fixture("countries.json"))
  })
  cb_api_get("/countries")
  expect_equal(unname(seen["X-API-Key"]), "test-key")
  expect_equal(unname(seen["Accept"]), "application/json")
})

test_that("responses are cached for the session", {
  local_api()
  calls <- 0
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    calls <<- calls + 1
    fake_response(fixture("countries.json"))
  })
  cb_api_get("/countries")
  cb_api_get("/countries")
  expect_equal(calls, 1)
  cb_api_get("/countries", cache = FALSE)
  expect_equal(calls, 2)
  cb_cache_clear()
  cb_api_get("/countries")
  expect_equal(calls, 3)
})

test_that("cb_countries(source = 'api') returns a data frame", {
  local_api()
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) fake_response(fixture("countries.json")))
  cs <- cb_countries(source = "api")
  expect_equal(sort(cs$country), c("norway", "uk"))
  expect_true(cs$tables_only[cs$country == "norway"])
  expect_false(cs$tables_only[cs$country == "uk"])
  expect_match(cs$endpoints[cs$country == "uk"], "net-salary")
  expect_equal(cs$regions[cs$country == "uk"], 2L)
})

test_that("HTTP 429 with a short Retry-After is retried", {
  local_api()
  calls <- 0
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    calls <<- calls + 1
    if (calls == 1) {
      return(fake_response('{"error":{"code":"rate_limited","message":"Slow down."}}', 429,
                           list(`retry-after` = "0")))
    }
    fake_response(fixture("countries.json"))
  })
  expect_message(res <- cb_api_get("/countries"), "rate limit reached; waiting 0 s")
  expect_equal(calls, 2)
  expect_type(res$countries, "list")
})

test_that("HTTP 429 with a long Retry-After stops with calcbloc_rate_limited", {
  local_api()
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    fake_response('{"error":{"code":"rate_limited","message":"Daily limit reached.","docs":"https://calcbloc.com/developers#limits"}}',
                  429, list(`retry-after` = "3600", `x-ratelimit-remaining` = "0"))
  })
  err <- expect_error(cb_api_get("/countries"), class = "calcbloc_rate_limited")
  expect_s3_class(err, "calcbloc_api_error")
  expect_equal(err$retry_after, 3600L)
  expect_equal(err$status, 429)
  expect_match(conditionMessage(err), "free API key")
  expect_equal(cb_rate_limit()$remaining, 0L)
})

test_that("HTTP 429 without a key: the free allowance is used up, with the keys link and how to set the key", {
  local_api()
  seen <- NULL
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    seen <<- headers
    fake_response('{"error":{"code":"rate_limited","message":"Daily limit of 50 requests reached."}}',
                  429, list(`retry-after` = "36000", `x-ratelimit-remaining` = "0"))
  })
  err <- expect_error(cb_net_salary("uk", 30000), class = "calcbloc_rate_limited")
  expect_false("X-API-Key" %in% names(seen))
  msg <- conditionMessage(err)
  expect_true(startsWith(msg, "The free allowance of 50 requests a day without a key is used up. Add your free API key (5,000 requests a day): "))
  expect_true(grepl("https://calcbloc.com/developers/keys?utm_source=calcbloc-r&utm_medium=app&utm_campaign=itmarkerz-oss", msg, fixed = TRUE))
  expect_match(msg, "CALCBLOC_API_KEY", fixed = TRUE)
  expect_match(msg, "cb_api_key()", fixed = TRUE)
  expect_false(err$api_key_set)
})

test_that("HTTP 429 without a key on the per-minute limit says so once retries run out", {
  local_api()
  withr::local_options(calcbloc.max_retries = 0)
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    fake_response('{"error":{"code":"rate_limited","message":"Too many requests this minute (limit 5)."}}',
                  429, list(`retry-after` = "20"))
  })
  err <- expect_error(cb_api_get("/countries"), class = "calcbloc_rate_limited")
  expect_true(startsWith(conditionMessage(err), "The free allowance without a key (5 requests a minute, 50 a day) is used up for this minute."))
  expect_match(conditionMessage(err), "developers/keys?utm_source=calcbloc-r", fixed = TRUE)
})

test_that("HTTP 429 with a key (from CALCBLOC_API_KEY) keeps the keyed-limit message", {
  local_api()
  Sys.setenv(CALCBLOC_API_KEY = "env-key")
  seen <- NULL
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    seen <<- headers
    fake_response('{"error":{"code":"rate_limited","message":"Daily limit of 5000 requests reached."}}',
                  429, list(`retry-after` = "36000", `x-ratelimit-remaining` = "0"))
  })
  err <- expect_error(cb_api_get("/countries"), class = "calcbloc_rate_limited")
  expect_equal(unname(seen["X-API-Key"]), "env-key")
  expect_equal(conditionMessage(err), "CalcBloc API rate limit reached: Daily limit of 5000 requests reached. Retry after 36000 seconds.")
  expect_true(err$api_key_set)
})

test_that("API errors become classed conditions with the API's message", {
  local_api()
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    fake_response(paste0('{"error":{"code":"unsupported_country","message":"Net salary is not available for Norway yet.",',
                         '"docs":"https://calcbloc.com/developers"}}'), 400)
  })
  err <- expect_error(cb_net_salary("norway", 600000), class = "calcbloc_unsupported_country")
  expect_s3_class(err, "calcbloc_api_error")
  expect_equal(err$code, "unsupported_country")
  expect_match(conditionMessage(err), "not available for Norway")
})

test_that("network failures and non-JSON bodies are reported clearly", {
  local_api()
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) stop("Could not resolve host"))
  expect_error(cb_api_get("/countries"), class = "calcbloc_network_error")

  local_mocked_bindings(cb_http_get = function(url, headers, timeout) fake_response("<html>oops</html>"))
  expect_error(cb_api_get("/countries"), class = "calcbloc_bad_response")
})

test_that("rate-limit headers are recorded and a low quota is announced", {
  local_api()
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    fake_response(fixture("countries.json"), headers = list(`x-ratelimit-remaining` = "3"))
  })
  expect_message(cb_api_get("/countries"), "3 requests left")
  rl <- cb_rate_limit()
  expect_equal(rl$limit, 50L)
  expect_equal(rl$remaining, 3L)
  expect_equal(rl$status, 200)
})

test_that("cb_net_salary() sends the parameters and returns a printable result", {
  local_api()
  seen <- NULL
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    seen <<- url
    fake_response(fixture("net-salary-uk-50000.json"))
  })
  pay <- cb_net_salary("UK", 50000, region = "england", postgraduate_loan = FALSE)
  expect_equal(seen, "https://calcbloc.com/api/v1/net-salary?country=uk&salary=50000&region=england&postgraduate_loan=false")
  expect_s3_class(pay, "cb_result")
  expect_equal(pay$results$incomeTax, 7486)
  expect_output(print(pay), "netAnnual")
  expect_output(print(pay), "Data by CalcBloc (calcbloc.com/data): https://calcbloc.com/uk/take-home-pay-calculator?utm_source=calcbloc-r&utm_medium=app&utm_campaign=itmarkerz-oss",
                fixed = TRUE)
  df <- as.data.frame(pay)
  expect_equal(nrow(df), 1)
  expect_equal(df$netAnnual, 39520.12)
  expect_equal(df$results_employeeNi, 2993.88)
})

test_that("calculation helpers validate their input before any request", {
  local_api()
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) stop("should not be called"))
  expect_error(cb_net_salary("uk", c(1, 2)), "single non-negative number")
  expect_error(cb_net_salary("uk", -1), "single non-negative number")
  expect_error(cb_employer_cost("uk", 50000, "england"), "must be named")
  expect_error(cb_compare("uk", "germany", NA), "single non-negative number")
})

test_that("large numbers are not sent in scientific notation", {
  local_api()
  seen <- NULL
  local_mocked_bindings(cb_http_get = function(url, headers, timeout) {
    seen <<- url
    fake_response('{"endpoint":"employer-cost","country":"us"}')
  })
  cb_employer_cost("us", 1e6)
  expect_match(seen, "salary=1000000", fixed = TRUE)
})
