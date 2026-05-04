test_that("fetch() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  client <- rstripe("test")
  dat <- fetch(client, "invoices", 1L)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 1L)

  dat_inf <- fetch(client, "invoices", Inf)
  expect_s3_class(dat_inf, "data.frame")
  expect_gt(nrow(dat_inf), 1L)
})

test_that("build_req() works", {
  client <- Rstripe(api_key = "sk_test_abc123", mode = "test")
  expect_snapshot(build_req(client, endpoint = "test", limit = 1L))
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
