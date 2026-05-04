#' Fetch Balance Transactions from Stripe API
#'
#' Retrieves balance transaction data from the Stripe API.
#'
#' @param client A `Striper` object created by [rstripe()].
#' @param limit Either a scalar between 1 and 100 or `Inf` to traverse all the
#'   available data. 10 by default.
#'
#' @family endpoints
#'
#' @return A data frame (tibble if available) containing balance transaction
#'   data.
#'
#' @section API Documentation: For more information about Stripe balance
#'   transactions, see: \url{https://docs.stripe.com/api/balance_transactions/}
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' test_transactions <- list_balance_transactions(client)
#' live_transactions <- list_balance_transactions(rstripe("live"))
#' }
#'
#' @export
list_balance_transactions <- function(client, limit = 10L) {
  check_limit(limit)

  dat <- fetch(client, "balance_transactions", limit)

  cols <- get_cols("balance_transactions")
  check_missing_cols(colnames(dat), cols)

  unexpected_types <- setdiff(
    unique(dat[["type"]]),
    get_balance_transaction_types()
  )

  if (length(unexpected_types)) {
    stripe_abort_missing_types(unexpected_types, "type")
  }

  dat[["amount"]] <- convert_amt_to_decimal(dat[["amount"]])
  dat[["fee"]] <- convert_amt_to_decimal(dat[["fee"]])
  dat[["net"]] <- convert_amt_to_decimal(dat[["net"]])

  dat[["exchange_rate"]] <- as.double(dat[["exchange_rate"]])

  dat[["available_on"]] <- date(as_datetime(dat[["available_on"]]))
  dat[["created"]] <- date(as_datetime(dat[["created"]]))

  dat
}

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
