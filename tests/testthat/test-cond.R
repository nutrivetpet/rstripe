test_that("stripe_abort() signals with stripe_error class", {
  expect_error(stripe_abort("boom"), class = "stripe_error")
})

test_that("typed helpers preserve legacy subclasses", {
  expect_error(stripe_abort_missing_api_key("test"), class = "missing_api_key")
  expect_error(
    stripe_abort_incorrect_api_key("test"),
    class = "incorrect_api_key"
  )
  expect_error(stripe_abort_api_error(401L), class = "stripe_api_error")
  expect_error(stripe_abort_api_error(401L), class = "stripe_401_error")
  expect_error(stripe_abort_empty_response(), class = "empty_response")
  expect_error(stripe_abort_missing_columns("id"), class = "missing_columns")
  expect_error(
    stripe_abort_missing_types("foo", "type"),
    class = "missing_types"
  )
  expect_error(stripe_abort_incorrect_has_more(), class = "incorrect_has_more")
})

test_that("all typed helpers inherit stripe_error", {
  expect_error(stripe_abort_missing_api_key("test"), class = "stripe_error")
  expect_error(stripe_abort_api_error(500L), class = "stripe_error")
})
