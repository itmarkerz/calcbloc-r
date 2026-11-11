# Build the package tarball for CRAN with a README.md whose calcbloc.com and itmarkerz.co.in links say
# utm_medium=cran, so visits from the README that CRAN shows are told apart from GitHub. The committed README.md
# (rendered from README.Rmd) keeps utm_medium=readme; it is never rewritten.
#
#   Rscript data-raw/build-package.R [extra R CMD build arguments]
#
# Run from the package root. The package is copied to a temporary directory, README.md is retagged there, and
# `R CMD build` runs on the copy; the tarball lands in the package root, as with `R CMD build .`. The script then
# checks the README inside the tarball and stops if any utm_medium=readme link is left.

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(".", winslash = "/")
if (!file.exists(file.path(root, "DESCRIPTION")) || read.dcf("DESCRIPTION", "Package")[[1]] != "calcbloc") {
  stop("Run this script from the calcbloc package root.", call. = FALSE)
}

pattern <- "(utm_source=calcbloc-r&utm_medium=)readme\\b"

cran_readme <- function(text) {
  n <- sum(vapply(gregexpr(pattern, text, perl = TRUE), function(m) sum(m > 0), numeric(1)))
  if (n == 0) stop("README.md has no utm_source=calcbloc-r&utm_medium=readme links to retag.", call. = FALSE)
  list(text = gsub(pattern, "\\1cran", text, perl = TRUE), n = n)
}

read_utf8 <- function(path) readLines(path, encoding = "UTF-8", warn = FALSE)
write_utf8 <- function(text, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(text), con, sep = "\n", useBytes = TRUE)
}

readme <- cran_readme(read_utf8(file.path(root, "README.md")))

stage_parent <- tempfile("calcbloc-build-")
stage <- file.path(stage_parent, "calcbloc")
dir.create(stage, recursive = TRUE)
on.exit(unlink(stage_parent, recursive = TRUE), add = TRUE)

skip <- c(".git", ".Rproj.user", "calcbloc.Rcheck")
entries <- list.files(root, all.files = TRUE, no.. = TRUE)
entries <- entries[!(entries %in% skip) & !grepl("^calcbloc_.*\\.tar\\.gz$", entries)]
ok <- file.copy(file.path(root, entries), stage, recursive = TRUE, copy.date = TRUE)
if (!all(ok)) stop("Could not copy: ", paste(entries[!ok], collapse = ", "), call. = FALSE)
write_utf8(readme$text, file.path(stage, "README.md"))

r_bin <- file.path(R.home("bin"), "R")
status <- system2(r_bin, c("CMD", "build", args, shQuote(stage)))
if (status != 0) stop("R CMD build failed.", call. = FALSE)

version <- read.dcf(file.path(root, "DESCRIPTION"), "Version")[[1]]
tarball <- file.path(root, sprintf("calcbloc_%s.tar.gz", version))
if (!file.exists(tarball)) stop("Expected ", tarball, " after R CMD build.", call. = FALSE)

check_dir <- tempfile("calcbloc-readme-")
utils::untar(tarball, files = "calcbloc/README.md", exdir = check_dir)
shipped <- read_utf8(file.path(check_dir, "calcbloc", "README.md"))
unlink(check_dir, recursive = TRUE)
left <- sum(grepl(pattern, shipped, perl = TRUE))
tagged <- sum(vapply(gregexpr("utm_source=calcbloc-r&utm_medium=cran\\b", shipped, perl = TRUE), function(m) sum(m > 0), numeric(1)))
if (left > 0 || tagged != readme$n) {
  stop(sprintf("README.md in %s: %d utm_medium=cran links (expected %d), %d lines still with utm_medium=readme.",
               basename(tarball), tagged, readme$n, left), call. = FALSE)
}
message(sprintf("Built %s; its README.md has %d links with utm_medium=cran.", basename(tarball), tagged))
