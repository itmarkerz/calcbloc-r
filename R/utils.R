cb_credit_text <- "Data by CalcBloc (calcbloc.com/data)"
cb_data_url <- "https://calcbloc.com/data"

or_else <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Credit line for CalcBloc data
#'
#' Returns the attribution required by the CC BY 4.0 data licence, ready to use
#' as a plot caption or table footnote.
#'
#' @param url Include the link to the data page.
#' @return A character string.
#' @export
#' @examples
#' cb_credit()
#' cb_credit(url = TRUE)
cb_credit <- function(url = FALSE) {
  if (isTRUE(url)) paste0(cb_credit_text, ", CC BY 4.0: ", cb_data_url) else cb_credit_text
}

read_json_gz <- function(path) {
  con <- gzfile(path, open = "rt", encoding = "UTF-8")
  on.exit(close(con))
  txt <- readLines(con, warn = FALSE, encoding = "UTF-8")
  jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
}

# Scalar accessors on parsed JSON (lists with NULLs for JSON null).
chr1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) x <- unlist(x)
  x <- as.character(x[[1]])
  if (identical(x, "")) NA_character_ else x
}
num1 <- function(x) {
  if (is.null(x) || length(x) == 0 || !is.numeric(x)) return(NA_real_)
  as.numeric(x[[1]])
}
pluck_chr <- function(rows, field) vapply(rows, function(r) chr1(r[[field]]), character(1), USE.NAMES = FALSE)
pluck_num <- function(rows, field) vapply(rows, function(r) num1(r[[field]]), numeric(1), USE.NAMES = FALSE)
as_date <- function(x) as.Date(x, format = "%Y-%m-%d")

check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    stop("`", arg, "` must be a single non-empty string.", call. = FALSE)
  }
  trimws(x)
}

collapse <- function(x, n = 12) {
  x <- as.character(x)
  if (length(x) > n) paste0(paste(x[seq_len(n)], collapse = ", "), ", ... (", length(x), " in all)") else paste(x, collapse = ", ")
}
chr_or <- function(x, default) {
  v <- chr1(x)
  if (is.na(v)) default else v
}
