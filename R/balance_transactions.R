#' Fetch Balance Transactions from Stripe API
#'
#' Retrieves balance transaction data from the Stripe API.
#'
#' @param mode Character string specifying the API mode. Must be either "test"
#'   for test mode or "live" for live mode. The API key will be automatically
#'   retrieved based on this mode.
#' @param limit Either a scalar between 1 and 100 or `Inf` to traverse all the
#'   available data. 10 by default.
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
fetch_balance_transactions <- function(mode = c("test", "live"), limit = 10L) {
  mode <- arg_match(mode, mode)
  stopifnot((limit >= 1L && limit <= 100L) || is.infinite(limit))

  if (is.infinite(limit)) {
    limit <- 100L
  }

  api_key <- get_api_key(mode)
  req <- build_req(
    api_key,
    endpoint = "balance_transactions",
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
    unique(dat[["type"]]),
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

  dat[["amount"]] <- convert_stripe_amount_to_decimal(dat[["amount"]])
  dat[["fee"]] <- convert_stripe_amount_to_decimal(dat[["fee"]])
  dat[["net"]] <- convert_stripe_amount_to_decimal(dat[["net"]])

  dat[["available_on"]] <- lubridate::date(lubridate::as_datetime(dat[[
    "available_on"
  ]]))
  dat[["created"]] <- lubridate::date(lubridate::as_datetime(dat[["created"]]))

  dat
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
