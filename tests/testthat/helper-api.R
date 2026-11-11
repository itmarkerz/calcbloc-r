# Helpers for the API tests: no network is used, cb_http_get() is replaced by a fake.

fixture <- function(name) {
  paste(readLines(test_path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

fake_response <- function(body, status = 200, headers = list()) {
  hdr <- utils::modifyList(list(`x-ratelimit-limit` = "50", `x-ratelimit-remaining` = "40", `x-ratelimit-reset` = "3600"), headers)
  list(status = status, headers = hdr, body = body)
}

# Fresh cache, no throttling and no API key for the duration of a test.
local_api <- function(env = parent.frame()) {
  withr::local_options(calcbloc.min_interval = 0, calcbloc.max_wait = 5, calcbloc.max_retries = 2, .local_envir = env)
  withr::local_envvar(CALCBLOC_API_KEY = NA, .local_envir = env)
  cb_cache_clear()
  withr::defer(cb_cache_clear(), envir = env)
}
