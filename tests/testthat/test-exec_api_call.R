test_that("exec_api_call() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  dat <- exec_api_call("invoices", mode = "test", limit = 1L)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 1L)

  dat_inf <- exec_api_call("invoices", mode = "test", limit = Inf)
  expect_s3_class(dat_inf, "data.frame")
  expect_gt(nrow(dat_inf), 1L)
})

test_that("build_req() works", {
  expect_snapshot(build_req(api_key = "TEST", endpoint = "test", limit = 1L))
})

test_that("get_api_key() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  expect_type(get_api_key(mode = "test"), "character")
  expect_length(get_api_key(mode = "test"), 1L)
})
