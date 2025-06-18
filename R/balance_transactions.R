#' Fetch Balance Transactions from Stripe API
#'
#' Retrieves balance transaction data from the Stripe API.
#'
#' @param mode Character string specifying the API mode. Must be either "test"
#'   for test mode or "live" for live mode. The API key will be automatically
#'   retrieved based on this mode.
#'
#' @return A data frame (tibble if available) containing balance transaction
#'   data.
#'
#' @section API Documentation: For more information about Stripe balance
#'   transactions, see: \url{https://docs.stripe.com/api/balance_transactions/}
#'
#' @examples
#' \dontrun{
#' # Fetch test mode balance transactions
#' test_transactions <- fetch_balance_transactions("test")
#'
#' # Fetch live mode balance transactions
#' live_transactions <- fetch_balance_transactions("live")
#' }
#'
#' @export
fetch_balance_transactions <- function(mode = c("test", "live")) {
  mode <- arg_match(mode, mode)
  api_key <- get_api_key(mode)

  type <- NULL

  req <- build_req(api_key, endpoint = "balance_transactions")

  resp <- req_perform(req)

  if (resp_is_error(resp)) {
    status <- resp_status(resp)
    msg <- get_error_msg(status)
    abort(
      msg,
      class = c("stripe_api_error", paste0("stripe_", status, "_error"))
    )
  }

  resp_body <- resp_body_json(resp, simplifyVector = TRUE)

  dat <- resp_body[["data"]]

  if (is_null(dat) || (!is.data.frame(dat) && !nrow(dat))) {
    abort("Response returned empty data.", class = "empty_response")
  }

  if (rlang::is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }

  cols <- get_balance_transactions_cols()
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

  unexpected_types <- setdiff(
    unique(pull(dat, type)),
    get_balance_transaction_types()
  )

  if (length(unexpected_types)) {
    abort(
      sprintf(
        "The following types are missing from column `type`: %s.",
        unexpected_types
      ),
      class = "missing_types"
    )
  }

  dat |>
    mutate(
      across(
        all_of(cols[c(3, 9, 11)]),
        ~ convert_stripe_amount_to_decimal(.x)
      )
    ) |>
    mutate(
      across(all_of(cols[c(4, 5)]), ~ lubridate::as_datetime(.x))
    )
}

# do not change order
get_balance_transactions_cols <- function() {
  c(
    "id",
    "object",
    "amount",
    "available_on",
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
  )
}

# do not change order
get_balance_transaction_types <- function() {
  c(
    "adjustment",
    "advance",
    "advance_funding",
    "anticipation_repayment",
    "application_fee",
    "application_fee_refund",
    "charge",
    "climate_order_purchase",
    "climate_order_refund",
    "connect_collection_transfer",
    "contribution",
    "issuing_authorization_hold",
    "issuing_authorization_release",
    "issuing_dispute",
    "issuing_transaction",
    "obligation_outbound",
    "obligation_reversal_inbound",
    "payment",
    "payment_failure_refund",
    "payment_network_reserve_hold",
    "payment_network_reserve_release",
    "payment_refund",
    "payment_reversal",
    "payment_unreconciled",
    "payout",
    "payout_cancel",
    "payout_failure",
    "payout_minimum_balance_hold",
    "payout_minimum_balance_release",
    "refund",
    "refund_failure",
    "reserve_transaction",
    "reserved_funds",
    "stripe_fee",
    "stripe_fx_fee",
    "stripe_balance_payment_debit",
    "stripe_balance_payment_debit_reversal",
    "tax_fee",
    "topup",
    "topup_reversal",
    "transfer",
    "transfer_cancel",
    "transfer_failure",
    "transfer_refund"
  )
}
