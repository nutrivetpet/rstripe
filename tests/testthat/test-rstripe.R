test_that("Rstripe() constructs with valid inputs", {
  client <- Rstripe(api_key = "sk_test_abc123", mode = "test")
  expect_true(S7::S7_inherits(client, Rstripe))
  expect_equal(client@mode, "test")
  expect_equal(client@base_url, STRIPE_BASE_URL)
  expect_equal(client@timeout, STRIPE_DEFAULT_TIMEOUT)
})

test_that("Rstripe() rejects malformed api_key", {
  expect_error(Rstripe(api_key = "bad_key", mode = "test"))
})

test_that("Rstripe() rejects mode/key mismatch", {
  expect_error(Rstripe(api_key = "sk_live_abc123", mode = "test"))
})

test_that("rstripe() pulls key from env var", {
  withr::with_envvar(
    c(STRIPE_API_KEY_TEST = "sk_test_fromenv"),
    {
      client <- rstripe("test")
      expect_equal(client@api_key, "sk_test_fromenv")
    }
  )
})

test_that("rstripe() warns when api_key is passed explicitly", {
  expect_warning(
    rstripe("test", api_key = "sk_test_explicit"),
    class = "rstripe_explicit_key"
  )
})

test_that("rstripe() with explicit bad-format key gives incorrect_api_key", {
  expect_error(
    suppressWarnings(rstripe("test", api_key = "bad_key")),
    class = "incorrect_api_key"
  )
})

test_that("rstripe() with env-var bad-format key gives incorrect_api_key mentioning env var", {
  withr::with_envvar(
    c(STRIPE_API_KEY_TEST = "bad_key"),
    {
      err <- tryCatch(rstripe("test"), error = function(e) e)
      expect_s3_class(err, "incorrect_api_key")
      expect_match(conditionMessage(err), "STRIPE_API_KEY_TEST")
    }
  )
})

test_that("print(Rstripe) redacts the api_key", {
  client <- Rstripe(api_key = "sk_test_abcdef123456", mode = "test")
  out <- capture.output(print(client))
  expect_true(any(grepl("sk_test_\\*+", out)))
  expect_false(any(grepl("abcdef123456", out)))
})
