
<!-- README.md is generated from README.Rmd. Please edit that file and re-render with rmarkdown::render("README.Rmd"). -->

# calcbloc: income tax tables and social security rates for R

calcbloc gives statisticians and economists tidy data frames of income
tax brackets, social security contribution rates, thresholds and
allowances for 24 countries, with the official source, retrieval date
and verification status on every row. National tables ship with the
package and work offline; regional tables and the latest revisions come
from the [CalcBloc public API](https://calcbloc.com/developers).

CalcBloc is a product of [itmarkerz
technologies](https://itmarkerz.co.in).

> **Status:** version 0.1.0, in review. Not on CRAN yet.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("itmarkerz/calcbloc-r")
```

calcbloc is pure R. It imports only
[jsonlite](https://cran.r-project.org/package=jsonlite) (JSON parsing)
and [curl](https://cran.r-project.org/package=curl) (HTTP for the
optional API client), both available as binaries on every CRAN platform.

## What is in it

| Function | What it does |
|----|----|
| `cb_countries()` | Countries, tax years, regions and the date each was last verified |
| `cb_tax_table(country, year, region, source)` | A tidy data frame: one row per band or value, with `source_url`, `retrieved_at` and `status` |
| `cb_sections()` | The sections of a table (income tax bands, employee and employer contributions, …) |
| `cb_tax_from_bands()`, `cb_band_breakdown()`, `cb_marginal_rate()` | Apply a progressive band schedule to a vector of incomes |
| `cb_manifest()`, `cb_regions()` | The dataset manifest and the regions with their own tables |
| `cb_net_salary()`, `cb_employer_cost()`, `cb_compare()` | Take-home pay, employer cost and two-country comparisons from the API |
| `cb_api_get()`, `cb_api_key()`, `cb_rate_limit()`, `cb_cache_clear()` | Low-level API access, key from `CALCBLOC_API_KEY`, quota status, session cache |
| `cb_credit()` | The credit line for plots and tables |

## Three examples

All output below is real, produced from the data bundled with this
version.

### 1. US federal income tax brackets, applied to a vector of incomes

``` r
library(calcbloc)

us <- cb_tax_table("us", 2026)
single <- us[us$section_id == "federal-income-tax-single", c("from", "to", "rate")]
single
#>     from     to rate
#> 1      0  12400 0.10
#> 2  12400  50400 0.12
#> 3  50400 105700 0.22
#> 4 105700 201775 0.24
#> 5 201775 256225 0.32
#> 6 256225 640600 0.35
#> 7 640600     NA 0.37

income <- c(25000, 50000, 75000, 150000, 500000)
data.frame(income,
           tax = cb_tax_from_bands(income, us, "federal-income-tax-single"),
           marginal = cb_marginal_rate(income, us, "federal-income-tax-single"))
#>   income      tax marginal
#> 1  25000   2752.0     0.12
#> 2  50000   5752.0     0.12
#> 3  75000  11212.0     0.22
#> 4 150000  28598.0     0.24
#> 5 500000 143769.2     0.35
unique(us$source_url[us$section_id == "federal-income-tax-single"])
#> [1] "https://www.irs.gov/pub/irs-drop/rp-25-32.pdf"
```

The tax on \$75,000 matches the IRS rate schedule in Rev. Proc. 2025-32:
\$5,800 plus 22% of the excess over \$50,400.

### 2. Ireland’s Universal Social Charge, band by band

``` r
ie <- cb_tax_table("ireland")
usc <- cb_band_breakdown(50000, ie, "usc")
usc
#>   income band label  from    to  rate amount    tax
#> 1  50000    1  <NA>     0 12012 0.005  12012  60.06
#> 2  50000    2  <NA> 12012 28700 0.020  16688 333.76
#> 3  50000    3  <NA> 28700 70044 0.030  21300 639.00
#> 4  50000    4  <NA> 70044    NA 0.080      0   0.00
sum(usc$tax)
#> [1] 1032.82
```

EUR 1,032.82 is the figure in Revenue’s own worked example (Sadhbh, EUR
50,000) on [Calculating your
USC](https://www.revenue.ie/en/jobs-and-pensions/usc/calculating-usc.aspx).
Note what the bands do not know: USC is not charged at all on incomes of
EUR 13,000 or less, a rule outside the band schedule.

### 3. The highest national band rate in every bundled country

``` r
top_rates <- do.call(rbind, lapply(cb_countries()$country, function(cc) {
  tab <- cb_tax_table(cc)
  b <- tab[tab$kind == "bands" & tab$group == "income-tax" &
             !grepl("withholding|^pas-", tab$section_id), ]   # leave out withholding grids
  if (!nrow(b)) return(NULL)
  b <- b[b$rate == max(b$rate), ][1, ]
  data.frame(country = cc, currency = attr(tab, "currency"), section = b$section_id,
             top_rate = b$rate, from = b$from)
}))
top_rates[order(-top_rates$top_rate), ]
#>        country currency                   section top_rate    from
#> 16     austria      EUR                income-tax   0.5500 1000000
#> 8  netherlands      EUR                      box1   0.4950   78426
#> 12    portugal      EUR                income-tax   0.4800   86634
#> 2           uk      GBP        income-tax-england   0.4500  125140
#> 3    australia      AUD            resident-rates   0.4500  190000
#> 9       france      EUR                income-tax   0.4500  181917
#> 11       italy      EUR                     irpef   0.4300   50000
#> 18  luxembourg      EUR                income-tax   0.4200  234870
#> 4  new-zealand      NZD                income-tax   0.3900  180000
#> 21     finland      EUR                 state-tax   0.3750   52100
#> 1           us      USD federal-income-tax-single   0.3700  640600
#> 5       canada      CAD        federal-income-tax   0.3300  258482
#> 14     denmark      DKK                 state-tax   0.3201 2592700
#> 17      poland      PLN                income-tax   0.3200  120000
#> 6        india      INR                new-regime   0.3000 2400000
#> 7        spain      EUR                state-irpf   0.2450  300000
#> 10   singapore      SGD                income-tax   0.2400 1000000
#> 19     czechia      CZK                income-tax   0.2300 1762812
#> 15      sweden      SEK                 state-tax   0.2000  643000
#> 20      norway      NOK                trinnskatt   0.1780 1467200
#> 13 switzerland      CHF               federal-tax   0.1320  185100
```

These are national (central government) schedules only. Where states,
provinces, regions, cantons or municipalities levy their own income tax
(the US, Canada, Spain, Switzerland and the Nordic countries, for
example) it comes on top, so the figures are not combined top rates.
Germany is missing because its tariff is a formula, published as single
values; see `cb_sections(cb_tax_table("germany"))`.

## Each row says where it comes from

``` r
uk <- cb_tax_table("uk")
uk
#> <CalcBloc tax table> United Kingdom 2026/27 (GBP)
#> 78 rows in 17 sections; table status: verified; source: bundled with calcbloc (data of 2026-11-11)
#>          section_id                              label   from     to   rate  value  unit   status
#>  income-tax-england                         Basic rate      0  37700 0.2000     NA  rate verified
#>  income-tax-england                        Higher rate  37700 125140 0.4000     NA  rate verified
#>  income-tax-england                    Additional rate 125140     NA 0.4500     NA  rate verified
#>  personal-allowance                 Personal Allowance     NA     NA     NA  12570 money verified
#>  personal-allowance Income limit for the Personal All~     NA     NA     NA 100000 money verified
#>  personal-allowance Allowance taper above the income ~     NA     NA 0.5000     NA  rate verified
#>           dividends             Dividend ordinary rate      0  37700 0.1075     NA  rate verified
#>           dividends                Dividend upper rate  37700 125140 0.3575     NA  rate verified
#>           dividends           Dividend additional rate 125140     NA 0.3935     NA  rate verified
#>       ni-thresholds         Lower Earnings Limit (LEL)     NA     NA     NA   6708 money verified
#> # ... 68 more rows. All columns: as.data.frame(x); sections: cb_sections(x)
#> Data by CalcBloc (calcbloc.com/data), CC BY 4.0: https://calcbloc.com/uk/tax-tables/2026-27
str(as.data.frame(uk)[1, c("section", "label", "rate", "basis", "effective_from", "effective_to",
                            "source_title", "source_url", "retrieved_at", "status")])
#> 'data.frame':    1 obs. of  10 variables:
#>  $ section       : chr "Income Tax bands: England and Northern Ireland"
#>  $ label         : chr "Basic rate"
#>  $ rate          : num 0.2
#>  $ basis         : chr "Taxable income above the Personal Allowance, per year"
#>  $ effective_from: Date, format: "2026-04-06"
#>  $ effective_to  : Date, format: "2027-04-05"
#>  $ source_title  : chr "HMRC: Income Tax rates and allowances for current and previous tax years (updated 6 April 2026)"
#>  $ source_url    : chr "https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past"
#>  $ retrieved_at  : Date, format: "2026-10-16"
#>  $ status        : chr "verified"
```

## Online: regional tables and calculations

Regional tables (US states, Canadian provinces and territories, Scotland
and Wales, Spanish autonomous communities, Italian regions, Swiss
cantons, Danish, Swedish, Finnish and Portuguese regional rates) are
fetched from the API:

``` r
cb_regions("us")                                  # codes, offline
ca <- cb_tax_table("us", region = "ca", source = "api")
cb_countries(source = "api")                      # what the API serves today

# Calculation endpoints (about a dozen countries; free key recommended)
cb_net_salary("uk", 50000, region = "scotland")
cb_employer_cost("germany", 60000)
cb_compare("uk", "germany", 60000)
```

Without a key the API allows 50 requests a day and 5 a minute per IP
address. A free key from
[calcbloc.com/developers](https://calcbloc.com/developers) raises this
to 5,000 a day; put it in `.Renviron` as `CALCBLOC_API_KEY=...` and
`cb_api_key()` picks it up. The client keeps at least a second between
requests, caches responses for the session, waits for `Retry-After` when
the server asks it to (up to `getOption("calcbloc.max_wait", 60)`
seconds) and reports the remaining quota with `cb_rate_limit()`.

## Coverage

Bundled national tables, from the package manifest (data of 2026-11-11):

| Country | Code | Tax years | Regional tables (API) | Rows | Last verified |
|:---|:---|:---|:---|---:|:---|
| [Australia](https://calcbloc.com/australia/tax-tables/2026-27) | `australia` | 2026-27 | \- | 88 | 2026-11-09 |
| [Austria](https://calcbloc.com/austria/tax-tables/2026) | `austria` | 2026 | \- | 95 | 2026-11-10 |
| [Canada](https://calcbloc.com/canada/tax-tables/2026) | `canada` | 2026 | 13 | 95 | 2026-11-09 |
| [Czechia](https://calcbloc.com/czechia/tax-tables/2026) | `czechia` | 2026 | \- | 104 | 2026-11-11 |
| [Denmark](https://calcbloc.com/denmark/tax-tables/2026) | `denmark` | 2026 | 5 | 87 | 2026-11-09 |
| [Finland](https://calcbloc.com/finland/tax-tables/2026) | `finland` | 2026 | 19 | 124 | 2026-11-11 |
| [France](https://calcbloc.com/france/tax-tables/2026) | `france` | 2026 | \- | 158 | 2026-11-09 |
| [Germany](https://calcbloc.com/germany/tax-tables/2026) | `germany` | 2026 | \- | 85 | 2026-11-09 |
| [India](https://calcbloc.com/india/tax-tables/2026-27) | `india` | 2026-27 | \- | 190 | 2026-11-11 |
| [Ireland](https://calcbloc.com/ireland/tax-tables/2026) | `ireland` | 2026 | \- | 50 | 2026-11-06 |
| [Italy](https://calcbloc.com/italy/tax-tables/2026) | `italy` | 2026 | 21 | 101 | 2026-11-09 |
| [Luxembourg](https://calcbloc.com/luxembourg/tax-tables/2026) | `luxembourg` | 2026 | \- | 126 | 2026-11-11 |
| [Netherlands](https://calcbloc.com/netherlands/tax-tables/2026) | `netherlands` | 2026 | \- | 61 | 2026-11-09 |
| [New Zealand](https://calcbloc.com/new-zealand/tax-tables/2026-27) | `new-zealand` | 2026-27 | \- | 55 | 2026-11-09 |
| [Norway](https://calcbloc.com/norway/tax-tables/2026) | `norway` | 2026 | \- | 98 | 2026-11-11 |
| [Poland](https://calcbloc.com/poland/tax-tables/2026) | `poland` | 2026 | \- | 93 | 2026-11-10 |
| [Portugal](https://calcbloc.com/portugal/tax-tables/2026) | `portugal` | 2026 | 2 | 76 | 2026-11-09 |
| [Singapore](https://calcbloc.com/singapore/tax-tables/2026) | `singapore` | 2026 | \- | 94 | 2026-11-09 |
| [Spain](https://calcbloc.com/spain/tax-tables/2026) | `spain` | 2026 | 21 | 90 | 2026-11-09 |
| [Sweden](https://calcbloc.com/sweden/tax-tables/2026) | `sweden` | 2026 | 21 | 140 | 2026-11-10 |
| [Switzerland](https://calcbloc.com/switzerland/tax-tables/2026) | `switzerland` | 2026 | 26 | 148 | 2026-11-09 |
| [United Arab Emirates](https://calcbloc.com/uae/tax-tables/2026) | `uae` | 2026 | \- | 56 | 2026-11-09 |
| [United Kingdom](https://calcbloc.com/uk/tax-tables/2026-27) | `uk` | 2026-27, 2025-26 | 2 | 78 | 2026-11-09 |
| [United States](https://calcbloc.com/us/tax-tables/2026) | `us` | 2026 | 51 | 70 | 2026-11-09 |

## Data, licence and credit

The tax data comes from the CalcBloc open dataset at
[calcbloc.com/data](https://calcbloc.com/data): every figure is taken
from the official government publication named next to it
(`source_url`), with the date it was retrieved and its verification
status. Tables whose overall status is `unverified` contain figures
still being checked; filter on `status` if you need verified rows only.

- **Data licence:** CC BY 4.0. Credit “Data by CalcBloc
  (calcbloc.com/data)” wherever you publish the data or results based on
  it; `cb_credit()` returns the text, for example
  `labs(caption = cb_credit())`.
- **Code licence:** MIT.
- Not tax advice: the band helpers apply the published schedule and
  nothing else (no allowances, credits, tapers or rounding rules).

To cite the package: `citation("calcbloc")`.

## Links

- Website: [calcbloc.com](https://calcbloc.com)
- Tax tables and downloads:
  [calcbloc.com/data](https://calcbloc.com/data)
- API documentation:
  [calcbloc.com/developers](https://calcbloc.com/developers)
- Vignette: `vignette("comparing-tax-schedules", package = "calcbloc")`

## About

[CalcBloc](https://calcbloc.com) is built and maintained by [itmarkerz
technologies](https://itmarkerz.co.in), a web and mobile app development
company in Kerala, India. The open data behind this package is at
[calcbloc.com/data](https://calcbloc.com/data) and the API documentation
at [calcbloc.com/developers](https://calcbloc.com/developers). The other
CalcBloc packages (Python, JavaScript, PHP, Google Sheets, the MCP
server and the dataset itself) are in the [itmarkerz GitHub
organization](https://github.com/itmarkerz).
