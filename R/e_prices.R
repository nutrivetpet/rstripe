#' Fetch Prices from Stripe API
#'
#' Retrieves prices data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing prices data.
#'
#' @section API Documentation: For more information about Stripe prices, see:
#' \url{https://docs.stripe.com/api/prices/}
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' test_prices <- list_prices(client)
#' live_prices <- list_prices(rstripe("live"))
#' }
#'
#' @export
list_prices <- function(client, limit = 10L) {
  check_limit(limit)

  dat <- fetch(client, "prices", limit)

  cols <- get_cols("prices")
  check_missing_cols(colnames(dat), cols)

  dat[["unit_amount"]] <- convert_amt_to_decimal(dat[["unit_amount"]])
  dat[["unit_amount_decimal"]] <- convert_amt_to_decimal(as.integer(
    dat[["unit_amount_decimal"]]
  ))

  dat[["created"]] <- date(as_datetime(dat[["created"]]))
  dat
}
