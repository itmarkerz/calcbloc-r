test_that("cb_countries() lists the bundled countries from the manifest", {
  cs <- cb_countries()
  expect_s3_class(cs, "data.frame")
  m <- cb_manifest()
  expect_equal(nrow(cs), length(m$countries))
  expect_gt(nrow(cs), 20)
  expect_named(cs, c("country", "name", "currency", "current_year", "years", "regions", "rows",
                     "last_verified", "page_url", "api_url"))
  expect_false(anyDuplicated(cs$country) > 0)
  expect_true(all(startsWith(cs$page_url, "https://calcbloc.com/")))
  expect_true(all(startsWith(cs$api_url, "https://calcbloc.com/api/v1/tax-tables/")))
  expect_s3_class(cs$last_verified, "Date")
  expect_true(all(c("us", "uk", "canada", "australia", "germany") %in% cs$country))
})

test_that("cb_manifest() prints the credit", {
  m <- cb_manifest()
  expect_s3_class(m, "cb_manifest")
  expect_match(m$credit, "Data by CalcBloc (calcbloc.com/data)", fixed = TRUE)
  expect_output(print(m), "Data by CalcBloc (calcbloc.com/data)", fixed = TRUE)
})

test_that("cb_regions() returns region codes", {
  ca <- cb_regions("canada")
  expect_true(all(c("on", "qc", "bc") %in% ca$code))
  expect_equal(nrow(cb_regions("germany")), 0)
})

test_that("every bundled table loads with the documented columns", {
  cols <- c("country", "region", "tax_year", "section_id", "section", "group", "kind", "label",
            "from", "to", "rate", "value", "text", "unit", "basis", "note", "effective_from",
            "effective_to", "source_title", "source_url", "retrieved_at", "status")
  cs <- cb_countries()
  for (i in seq_len(nrow(cs))) {
    for (y in strsplit(cs$years[i], ", ", fixed = TRUE)[[1]]) {
      t <- cb_tax_table(cs$country[i], y)
      expect_s3_class(t, "cb_tax_table")
      expect_named(t, cols)
      expect_gt(nrow(t), 0)
      expect_true(all(t$country == cs$country[i]))
      expect_true(all(is.na(t$region)))
      expect_true(all(t$kind %in% c("bands", "values")))
      expect_true(all(t$unit %in% c("rate", "money", "number", "text")))
      expect_true(all(t$status %in% c("verified", "under_review", "not_applicable")))
      expect_true(all(grepl("^https?://", t$source_url)))
      expect_s3_class(t$retrieved_at, "Date")
      expect_false(anyNA(t$retrieved_at))
      if (y == cs$current_year[i]) expect_equal(nrow(t), cs$rows[i])
      # Band rows have a lower edge and a rate.
      b <- t[t$kind == "bands", ]
      expect_false(anyNA(b$from))
      expect_false(anyNA(b$rate))
    }
  }
})

test_that("year resolution accepts NULL, latest, a start year and the label", {
  cs <- cb_countries()
  expect_equal(attr(cb_tax_table("uk"), "tax_year"), cs$current_year[cs$country == "uk"])
  expect_equal(attr(cb_tax_table("uk", "latest"), "tax_year"), attr(cb_tax_table("uk"), "tax_year"))
  expect_equal(attr(cb_tax_table("uk", 2025), "tax_year"), "2025-26")
  expect_equal(attr(cb_tax_table("uk", "2025-26"), "tax_year"), "2025-26")
  expect_equal(attr(cb_tax_table("us", 2026), "tax_year"), "2026")
  expect_error(cb_tax_table("us", 1999), "Years:")
})

test_that("countries can be given by name, and errors are informative", {
  expect_equal(attr(cb_tax_table("Germany"), "country"), "germany")
  expect_error(cb_tax_table("atlantis"), "No bundled tables")
  expect_error(cb_tax_table(c("us", "uk")), "single non-empty string")
  expect_error(cb_tax_table("us", region = "ca"), "source = \"api\"", fixed = TRUE)
  expect_error(cb_tax_table("us", region = "zz"), "Unknown region")
  expect_error(cb_tax_table("germany", region = "by"), "no regional variants")
})

test_that("subsetting keeps the class and attributes; dropping key columns gives a data frame", {
  us <- cb_tax_table("us")
  s <- us[us$section_id == "federal-income-tax-single", ]
  expect_s3_class(s, "cb_tax_table")
  expect_equal(attr(s, "currency"), "USD")
  expect_equal(nrow(s), 7)
  plain <- us[, c("from", "to", "rate")]
  expect_false(inherits(plain, "cb_tax_table"))
  expect_s3_class(plain, "data.frame")
  expect_false(inherits(as.data.frame(us), "cb_tax_table"))
  expect_null(attr(as.data.frame(us), "currency"))
})

test_that("print and format show the table, the credit and the page link", {
  uk <- cb_tax_table("uk", "2026-27")
  f <- format(uk)
  expect_match(f[1], "United Kingdom 2026/27 (GBP)", fixed = TRUE)
  expect_match(f[length(f)],
               "Data by CalcBloc (calcbloc.com/data), CC BY 4.0: https://calcbloc.com/uk/tax-tables/2026-27?utm_source=calcbloc-r&utm_medium=app&utm_campaign=itmarkerz-oss",
               fixed = TRUE)
  expect_equal(attr(uk, "page_url"), "https://calcbloc.com/uk/tax-tables/2026-27")
  expect_output(print(uk), "Data by CalcBloc (calcbloc.com/data)", fixed = TRUE)
  expect_output(print(uk, n = 3), "more rows")
})

test_that("cb_sections() summarises sections", {
  s <- cb_sections(cb_tax_table("australia"))
  expect_true("resident-rates" %in% s$section_id)
  expect_equal(s$kind[s$section_id == "resident-rates"], "bands")
  expect_equal(s$rows[s$section_id == "resident-rates"], 5L)
  expect_true(all(cb_sections(cb_tax_table("australia"), kind = "bands")$kind == "bands"))
  expect_error(cb_sections(data.frame(x = 1)), "cb_tax_table")
})

test_that("cb_credit() returns the CC BY credit", {
  expect_equal(cb_credit(), "Data by CalcBloc (calcbloc.com/data)")
  expect_match(cb_credit(url = TRUE), "https://calcbloc.com/data", fixed = TRUE)
})

test_that("printed calcbloc.com links carry the referral tag", {
  tag <- "utm_source=calcbloc-r&utm_medium=app&utm_campaign=itmarkerz-oss"
  expect_equal(cb_utm("https://calcbloc.com"), paste0("https://calcbloc.com/?", tag))
  expect_equal(cb_utm("https://calcbloc.com/data"), paste0("https://calcbloc.com/data?", tag))
  expect_equal(cb_utm("https://calcbloc.com/a?x=1#s"), paste0("https://calcbloc.com/a?x=1&", tag, "#s"))
  expect_equal(cb_utm(paste0("https://calcbloc.com/data?", tag)), paste0("https://calcbloc.com/data?", tag))
  expect_equal(cb_utm("https://example.org/x"), "https://example.org/x")
  expect_true(is.na(cb_utm(NA_character_)))
  expect_output(print(cb_manifest()), paste0("https://calcbloc.com/data?", tag), fixed = TRUE)
})
