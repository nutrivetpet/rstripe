#' Fetch Prices from Stripe API
#'
#' Retrieves prices data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing prices data.
#'
#' @section API Documentation: For more information about Stripe balance
#'   transactions, see: \url{https://docs.stripe.com/api/prices/}
#'
#' @examples
#' \dontrun{
#' # Fetch test mode balance transactions
#' test_prices <- list_prices("test")
#'
#' # Fetch live mode balance transactions
#' live_prices <- list_prices("live")
#' }
#'
#' @export
list_prices <- function(mode = c("test", "live"), limit = 10L) {
  check_mode(mode)
  check_limit(limit)

  dat <- exec_api_call("prices", mode, limit)

  cols <- get_cols("prices")
  check_missing_cols(colnames(dat), cols)

  dat[["unit_amount"]] <- convert_amt_to_decimal(dat[["unit_amount"]])
  dat[["unit_amount_decimal"]] <- convert_amt_to_decimal(as.integer(
    dat[[
      "unit_amount_decimal"
    ]]
  ))

  dat[["created"]] <- lubridate::date(lubridate::as_datetime(dat[["created"]]))
  dat
}
