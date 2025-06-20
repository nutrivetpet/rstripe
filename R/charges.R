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
  check_mode(mode)
  check_limit(limit)

  dat <- exec_api_call("charges", mode, limit)

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

  dat[["amount"]] <- convert_amt_to_decimal(dat[["amount"]])
  dat[["amount_captured"]] <- convert_amt_to_decimal(dat[[
    "amount_captured"
  ]])
  dat[["amount_refunded"]] <- convert_amt_to_decimal(dat[[
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
