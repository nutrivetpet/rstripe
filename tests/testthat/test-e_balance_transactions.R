# Fixed historical window used by the fixture and the idempotency test.
# Hardcoded so the set of matching transactions can never change.
FIXTURE_DATE_FROM <- as.Date("2026-03-01")
FIXTURE_DATE_TO <- as.Date("2026-04-30")

test_that("list_balance_transactions() works", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  client <- rstripe("test")
  dat <- list_balance_transactions(client, limit = 1L)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 1L)
})

test_that("list_balance_transactions() returns expected column names", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  client <- rstripe("test")
  dat <- list_balance_transactions(client, limit = 1L)
  expect_named(
    dat,
    c(
      "id",
      "object",
      "amount",
      "available_on",
      "balance_type",
      "created",
      "currency",
      "description",
      "exchange_rate",
      "fee",
      "fee_details",
      "net",
      "reporting_category",
      "source",
      "status",
      "type"
    ),
    ignore.order = FALSE
  )
})

test_that("list_balance_transactions() returns correct column types", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )
  client <- rstripe("test")
  dat <- list_balance_transactions(client, limit = 1L)
  expect_type(dat$id, "character")
  expect_type(dat$object, "character")
  expect_type(dat$currency, "character")
  expect_type(dat$reporting_category, "character")
  expect_type(dat$source, "character")
  expect_type(dat$status, "character")
  expect_type(dat$type, "character")
  expect_type(dat$amount, "double")
  expect_type(dat$fee, "double")
  expect_type(dat$net, "double")
  expect_s3_class(dat$available_on, "Date")
  expect_s3_class(dat$created, "Date")
  expect_type(dat$fee_details, "list")
})

test_that("list_balance_transactions() baseline fixture exists and is valid", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )

  fixture_path <- testthat::test_path(
    "fixtures",
    "balance_transactions_baseline.rds"
  )

  if (!file.exists(fixture_path)) {
    client <- rstripe("test")
    dat <- list_balance_transactions(
      client,
      limit = Inf,
      created_gte = FIXTURE_DATE_FROM,
      created_lte = FIXTURE_DATE_TO
    )
    dir.create(dirname(fixture_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(dat, fixture_path)
  }

  baseline <- readRDS(fixture_path)
  expect_s3_class(baseline, "data.frame")
  expect_gte(nrow(baseline), 1L)
  expect_true(all(c("id", "amount", "created") %in% names(baseline)))
})

test_that("date-filtered result matches unfiltered full fetch subset", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )

  client <- rstripe("test")

  # Fetch everything with no date params — identical code path to main branch.
  all_dat <- list_balance_transactions(client, limit = Inf)

  # Manually subset to the fixed window, replicating what the API-side filter should do.
  expected <- all_dat[
    all_dat$created >= FIXTURE_DATE_FROM & all_dat$created <= FIXTURE_DATE_TO,
  ]
  row.names(expected) <- NULL

  actual <- list_balance_transactions(
    client,
    limit = Inf,
    created_gte = FIXTURE_DATE_FROM,
    created_lte = FIXTURE_DATE_TO
  )

  expect_equal(actual, expected)
})

test_that("date-filtered result matches baseline fixture", {
  skip_on_cran()
  skip_if_not(
    nzchar(Sys.getenv("STRIPE_API_KEY_TEST")),
    "Stripe API Key is missing"
  )

  fixture_path <- testthat::test_path(
    "fixtures",
    "balance_transactions_baseline.rds"
  )
  skip_if_not(
    file.exists(fixture_path),
    "Baseline fixture not found — run tests once to generate it"
  )

  baseline <- readRDS(fixture_path)

  client <- rstripe("test")
  dat <- list_balance_transactions(
    client,
    limit = Inf,
    created_gte = FIXTURE_DATE_FROM,
    created_lte = FIXTURE_DATE_TO
  )

  expect_equal(dat, baseline)
})
