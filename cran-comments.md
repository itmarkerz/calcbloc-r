## Submission

This is a new submission: calcbloc 0.1.0.

## Test environments

* Local: Windows 11 x64, R 4.6.2 (ucrt), `R CMD check --as-cran`.
* To do before submitting: win-builder (R-devel and R-release), mac-builder, R-hub
  (linux, windows, macos-arm64).

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Binu Mathew <binumathew1988@gmail.com>'
  New submission

  This is the first submission of the package.

The local check also gave "checking HTML version of manual ... NOTE: Skipping
checking HTML validation: no command 'tidy' found", which is about the local
machine (HTML Tidy is not installed), not the package. The PDF manual, the
examples (including `--run-donttest`), the tests and the vignette rebuild all
passed.

## Notes for the reviewer

* **Internet resources.** The package works offline with the tables bundled in
  `inst/extdata`. An optional client for the CalcBloc web API
  (<https://calcbloc.com/developers>) is only used when the user asks for it
  (`source = "api"` or the `cb_net_salary()`, `cb_employer_cost()`,
  `cb_compare()` and `cb_api_get()` functions). Network failures and API errors
  raise classed conditions with an informative message. Examples that contact
  the API are wrapped in `@examplesIf interactive() && curl::has_internet()`;
  tests that contact it use `skip_on_cran()`, `skip_if_offline()` and are
  opt-in; all other tests use recorded responses. The vignette uses bundled
  data only.
* **Polite use of the API.** Requests are spaced by at least one second, cached
  for the session in memory, and `Retry-After` is honoured (at most two
  retries, at most 60 seconds of waiting by default).
* **Licences.** The R code is MIT licensed (`LICENSE` in CRAN's format). The
  bundled tax data is published by CalcBloc under CC BY 4.0; this is stated in
  the Description, `inst/COPYRIGHTS` and the help pages, and print methods show
  the required credit.
* **Files written.** None. The package does not write to the user's file space.
* **Size.** The tarball is about 0.27 MB; the bundled data is 26 gzip-compressed
  JSON files (about 0.2 MB).

## Reverse dependencies

None (new package).
