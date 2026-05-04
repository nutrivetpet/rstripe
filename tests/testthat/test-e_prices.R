test_that("list_prices() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  client <- rstripe("test")
  dat <- list_prices(client, limit = 1L)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 1L)
})
