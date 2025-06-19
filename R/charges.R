#' Fetch Charges from Stripe API
#'
#' Retrieves charges data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#'
#' @return A data frame (tibble if available) containing charges
#'   data.
#'
#' @section API Documentation: For more information about Stripe charges see:
#' \url{https://docs.stripe.com/api/charges/}
#'
#' @examples
#' \dontrun{
#' # Fetch test mode balance transactions
#' test_charges <- list_charges("test")
#'
#' # Fetch live mode balance transactions
#' live_charges <- list_charges("live")
#' }
#'
#' @export
list_charges <- function(mode = c("test", "live"), limit = 10L) {
  mode <- arg_match(mode, mode)
  stopifnot((limit >= 1L && limit <= 100L) || is.infinite(limit))

  if (is.infinite(limit)) {
    limit <- 100L
  }

  api_key <- get_api_key(mode)
  req <- build_req(
    api_key,
    endpoint = "charges",
    limit
  )

  resps <- req_perform_iterative(
    req,
    next_req = next_req,
    max_reqs = Inf,
    on_error = "return" # error objects are stored at the end
  )

  if (!is_installed("vctrs")) {
    abort("`resps_data()` requires the {vctrs} package to be installed.")
  }

  resps_successes_dat <-
    resps |>
    resps_successes() |>
    resps_data(
      function(resp) {
        out <- resp_body_json(resp, simplifyVector = TRUE)
        out[["data"]]
      }
    )

  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(resps_successes_dat)
  }

  resps_failures <-
    resps |>
    resps_failures()

  if (length(resps_failures)) {
    errors <- sapply(
      # can return NULL
      resps_failures,
      `[[`,
      "status"
    )
    statuses <- unlist(Filter(
      function(x) is_scalar_integerish(x, finite = TRUE),
      errors
    ))

    others <- Filter(
      function(x) !is_scalar_integerish(x, finite = TRUE),
      errors
    )

    if (length(statuses)) {
      msgs <- vapply(
        statuses,
        \(x) get_error_msg(x),
        FUN.VALUE = character(length(statuses))
      )
      abort(
        c("API Error(s)!", set_names(msgs, "x")),
        class = "stripe_api_error"
      )
    }

    if (length(others)) {
      abort("%s of non API related errors encountered.", length(others))
    }
  }

  cols <- get_charges_cols()
  missing_cols <- setdiff(
    cols,
    colnames(dat)
  )

  if (length(missing_cols)) {
    abort(
      sprintf("The following columns are missing: %s.", missing_cols),
      class = "missing_columns"
    )
  }

  dat[["amount"]] <- convert_stripe_amount_to_decimal(dat[["amount"]])
  dat[["amount_captured"]] <- convert_stripe_amount_to_decimal(dat[[
    "amount_captured"
  ]])
  dat[["amount_refunded"]] <- convert_stripe_amount_to_decimal(dat[[
    "amount_refunded"
  ]])

  dat[["created"]] <- lubridate::date(lubridate::as_datetime(dat[["created"]]))

  dat
}

get_charges_cols <- function() {
  c(
    "id",
    "object",
    "amount",
    "amount_captured",
    "amount_refunded",
    "application",
    "application_fee",
    "application_fee_amount",
    "balance_transaction",
    "billing_details",
    "calculated_statement_descriptor",
    "captured",
    "created",
    "currency",
    "customer",
    "description",
    "disputed",
    "failure_balance_transaction",
    "failure_code",
    "failure_message",
    "fraud_details",
    "livemode",
    "metadata",
    "on_behalf_of",
    "outcome",
    "paid",
    "payment_intent",
    "payment_method",
    "payment_method_details",
    "receipt_email",
    "receipt_number",
    "receipt_url",
    "refunded",
    "review",
    "shipping",
    "source_transfer",
    "statement_descriptor",
    "statement_descriptor_suffix",
    "status",
    "transfer_data",
    "transfer_group"
  )
}
