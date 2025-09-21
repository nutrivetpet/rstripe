test_that("list_charges() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  dat <- list_charges(mode = "test", limit = 1L)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 1L)
})
