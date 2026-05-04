test_that("parse_date_bound() accepts Date and YYYY-MM-DD", {
  ts <- as.integer(as.POSIXct(as.Date("2024-01-15"), tz = "UTC"))
  expect_identical(parse_date_bound("2024-01-15", "x"), ts)
  expect_identical(parse_date_bound(as.Date("2024-01-15"), "x"), ts)
})

test_that("parse_date_bound() end_of_day adds 23:59:59", {
  ts <- as.integer(as.POSIXct(as.Date("2024-01-15"), tz = "UTC"))
  expect_identical(
    parse_date_bound("2024-01-15", "x", end_of_day = TRUE),
    ts + 86399L
  )
})

test_that("parse_date_bound() rejects malformed input", {
  expect_error(parse_date_bound("2024/01/15", "x"), class = "invalid_date")
  expect_error(parse_date_bound("not a date", "x"), class = "invalid_date")
  expect_error(parse_date_bound(42, "x"), class = "invalid_date")
})

test_that("parse_date_bound() returns NULL for NULL input", {
  expect_null(parse_date_bound(NULL, "x"))
})

test_that("build_created_filter() omits absent bounds", {
  expect_identical(build_created_filter(), list())
  expect_named(build_created_filter(gte = "2024-01-01"), "created[gte]")
  expect_named(build_created_filter(lte = "2024-01-31"), "created[lte]")
})

test_that("build_created_filter() includes both bounds when given", {
  q <- build_created_filter(gte = "2024-01-01", lte = "2024-01-31")
  expect_named(q, c("created[gte]", "created[lte]"))
  expect_lt(q[["created[gte]"]], q[["created[lte]"]])
})

test_that("build_created_filter() rejects inverted range", {
  expect_error(
    build_created_filter(gte = "2024-02-01", lte = "2024-01-01"),
    class = "invalid_date_range"
  )
})

test_that("build_req() threads query params through", {
  client <- Rstripe(api_key = "sk_test_x", mode = "test")
  req <- build_req(
    client,
    endpoint = "balance_transactions",
    limit = 10L,
    query = list("created[gte]" = 1704067200L)
  )
  expect_match(req$url, "created%5Bgte%5D=1704067200")
  expect_match(req$url, "limit=10")
})
