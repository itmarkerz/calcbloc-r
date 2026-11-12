# Publishing checklist: calcbloc (R package)

Nothing has been submitted. This is the list of steps for a first CRAN release, in order.

## 0. Decisions before submitting

- [ ] Maintainer: Binu Mathew <binumathew1988@gmail.com> (`cre`). CRAN emails this address to
      confirm the submission and for every future problem report; it must stay valid.
- [ ] Data licence: the code is MIT, the bundled data CC BY 4.0. This is stated in `LICENSE.md`,
      `inst/COPYRIGHTS` and the Description, and `cran-comments.md` explains it to the reviewer.
      If CRAN asks for a single licence for everything, the fallback is to license the whole
      package as CC BY 4.0 (`License: CC BY 4.0`), which CRAN accepts; that needs the owner's agreement.
- [ ] Countries: only countries that calcbloc.com serves are bundled (33 on 2026-11-12). Lithuania,
      Malta, Cyprus and Iceland are in calcbloc-data but have no live page yet;
      re-run `data-raw/build-bundled-data.R` once they are live.
- [ ] Regional tables are not bundled (API only). Bundling all 204 of them would add about
      0.3 MB (compressed JSON); the national tables are about 0.2 MB. A small change to the data-raw
      script and `cb_tax_table()` if the owner prefers offline regional tables.

## 1. Refresh the data

```sh
# from the package root, with calcbloc-data cloned next to it
Rscript data-raw/build-bundled-data.R ../calcbloc-data/tables
```

- [ ] Check the message: number of countries, files and size, and the list of countries not bundled.
- [ ] Update the country count and data date in `NEWS.md` if they changed.
- [ ] Re-render the README: `Rscript -e 'rmarkdown::render("README.Rmd")'` (needs the package installed:
      `R CMD INSTALL .`).

## 2. Local checks

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
Rscript data-raw/build-package.R   # R CMD build on a copy whose README.md says utm_medium=cran
R CMD check --as-cran calcbloc_0.1.0.tar.gz
```

- [ ] Build with `data-raw/build-package.R`, not plain `R CMD build .`: CRAN shows the README.md inside the
      tarball on the package page, and the script retags its calcbloc.com and itmarkerz.co.in links from
      `utm_medium=readme` (GitHub) to `utm_medium=cran`, then checks the tarball. The committed README.md
      keeps `utm_medium=readme`.

- [ ] 0 errors, 0 warnings. Expected NOTE: "New submission" (and, offline, the URL/time checks).
- [ ] Optional live API tests (use about 4 of the 50 anonymous requests a day):
      `CALCBLOC_LIVE_TESTS=true Rscript -e 'devtools::test()'`.
- [ ] `urlchecker::url_check()` (install `urlchecker` first): every URL in the docs resolves.
- [ ] Spelling: `spelling::spell_check_package()`.

## 3. Remote checks (CRAN asks for these)

- [ ] win-builder, R-devel and R-release:
      `devtools::check_win_devel()` and `devtools::check_win_release()`. Results arrive by email to the
      maintainer in about 30 minutes.
- [ ] macOS: `devtools::check_mac_release()` (mac-builder).
- [ ] R-hub v2 (GitHub Actions): `rhub::rhub_setup()` once, commit the workflow, push, then
      `rhub::rhub_check()` and pick at least `linux`, `windows`, `macos-arm64` and `atlas` or `clang-asan`.
- [ ] Update `cran-comments.md` with the results.

## 4. Reverse dependencies

- [ ] None: this is a new package. (For later releases: `revdepcheck::revdep_check()`.)

## 5. CRAN policies that apply to this package

- **Internet resources.** "Packages which use Internet resources should fail gracefully with an
  informative message if the resource is not available or has changed (and not give a check
  warning nor error)." Done: every network call goes through `cb_api_get()`, which raises classed
  conditions (`calcbloc_network_error`, `calcbloc_api_error`, `calcbloc_rate_limited`) with a clear
  message; examples that call the API use `@examplesIf interactive() && curl::has_internet()`;
  live tests use `skip_on_cran()`, `skip_if_offline()` and an opt-in variable; the vignette and
  README examples use only bundled data.
- **No writing to the user's home filespace.** The package writes nothing to disk; the API cache
  lives in memory for the session only.
- **Size.** Tarball about 0.27 MB, installed about 0.6 MB (limit 5 MB).
- **Rate of requests.** The client spaces requests (1 s), honours `Retry-After`, caches responses
  and never retries more than twice.
- **Licence.** MIT in CRAN's format (`LICENSE` with YEAR and COPYRIGHT HOLDER only); data licence
  explained in `inst/COPYRIGHTS`.
- **Description.** Software and API names in single quotes ('CalcBloc'), web services in angle
  brackets, no "This package" opening, Title in title case without a full stop.
- **Examples.** Run in well under 5 s each; no `\dontrun{}`.

## 6. Submit

- [ ] Bump nothing (first release is 0.1.0), make sure `NEWS.md` starts with `# calcbloc 0.1.0`.
- [ ] Upload the tarball built by `Rscript data-raw/build-package.R` at
      <https://cran.r-project.org/submit.html>. (`devtools::release()` builds its own tarball, whose README
      would keep `utm_medium=readme`; use it only if that does not matter.)
- [ ] Confirm the submission from the email CRAN sends to the maintainer.
- [ ] If the reviewer asks for changes, fix them, note each fix under "Resubmission" in
      `cran-comments.md` and resubmit. A package that has not been accepted yet keeps version 0.1.0
      unless the reviewer asks otherwise.

## 7. After acceptance

- [ ] Tag the release on GitHub: `git tag v0.1.0 && git push --tags`; create a GitHub release.
- [ ] Change the README install section to `install.packages("calcbloc")` and remove the
      "Not on CRAN yet" line.
- [ ] Optionally add a pkgdown site and an r-universe listing (not done; out of scope for this review).
- [ ] Add the CRAN link to calcbloc.com/developers and calcbloc.com/data if wanted.

## Not done in this review (on purpose)

- No CRAN submission, no win-builder or R-hub run (both send results to the maintainer email).
- No r-universe registration, no pkgdown deployment.
