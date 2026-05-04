test_that("constants have expected values", {
  expect_equal(STRIPE_BASE_URL, "https://api.stripe.com/v1")
  expect_equal(STRIPE_MIN_LIMIT, 1L)
  expect_equal(STRIPE_MAX_LIMIT, 100L)
  expect_equal(STRIPE_DEFAULT_LIMIT, 10L)
  expect_type(STRIPE_DEFAULT_TIMEOUT, "double")
})

test_that("ply helpers behave like typed vapply", {
  expect_identical(chr_ply(1:3, as.character), c("1", "2", "3"))
  expect_identical(lgl_ply(1:3, function(x) x > 1), c(FALSE, TRUE, TRUE))
  expect_identical(int_ply(1:3, function(x) x + 1L), 2:4)
})
