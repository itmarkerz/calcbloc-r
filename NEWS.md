# calcbloc 0.1.0

First version, for review (not yet on CRAN).

* Bundled national tax tables for 24 countries from the CalcBloc open dataset
  (data of 2026-11-11), with the official source, retrieval date and status on
  every row. The UK has its 2026-27 and 2025-26 tables.
* `cb_countries()`, `cb_regions()` and `cb_manifest()` describe the data.
* `cb_tax_table()` returns a tidy data frame of a country's tables, from the
  bundled data or, with `source = "api"`, from the CalcBloc API (including
  regional tables).
* `cb_sections()` summarises a table's sections.
* `cb_tax_from_bands()`, `cb_band_breakdown()` and `cb_marginal_rate()` apply a
  progressive band schedule to a vector of incomes; `above` stacks one kind of
  income on another.
* API client: `cb_api_get()`, `cb_net_salary()`, `cb_employer_cost()`,
  `cb_compare()`, with `cb_api_key()` reading `CALCBLOC_API_KEY`, request
  spacing, a session cache (`cb_cache_clear()`), `Retry-After` handling and
  `cb_rate_limit()`. Works without a key (50 requests a day); when that runs
  out, the `calcbloc_rate_limited` error says so and links to a free API key
  (5,000 a day).
* `cb_credit()` returns the CC BY 4.0 credit line; print methods show it.
* Vignette: "Comparing tax schedules across countries".
