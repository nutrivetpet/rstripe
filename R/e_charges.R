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

  cols <- get_cols("charges")
  check_missing_cols(colnames(dat), cols)

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
