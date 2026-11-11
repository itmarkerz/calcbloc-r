# calcbloc: income tax tables and social security rates for R

An R package that gives statisticians and economists tidy data frames of 2026 income tax brackets, social security contribution rates and allowances for 29+ countries, with the official source and verification date on every row. Works offline with bundled tables or online through CalcBloc's public API.

> **Status:** in development. This repository will hold the source code; nothing has been released yet.

## Planned features

- `cb_countries()`: countries, tax years and regions available
- `cb_tax_table(country, year)`: a tidy data frame of every row with its official source URL
- `cb_tax_from_bands()`: apply a progressive bracket schedule to incomes (vectorised)
- Bundled data for offline use; optional API client for the latest tables

## Data and licence

Tax data comes from [CalcBloc](https://calcbloc.com/data): every figure is taken from the official government source named next to it, with the date it was last checked. Data licence: CC BY 4.0, credit "Data by CalcBloc (calcbloc.com/data)". Code licence: MIT.

## Links

- Website: [calcbloc.com](https://calcbloc.com)
- Tax tables and downloads: [calcbloc.com/data](https://calcbloc.com/data)
- API documentation: [calcbloc.com/developers](https://calcbloc.com/developers)
