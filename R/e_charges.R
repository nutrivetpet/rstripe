#' Fetch Charges from Stripe API
#'
#' Retrieves charges data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing charges data.
#'
#' @section API Documentation: For more information about Stripe charges see:
#' \url{https://docs.stripe.com/api/charges/}
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' test_charges <- list_charges(client)
#' live_charges <- list_charges(rstripe("live"))
#' }
#'
#' @export
list_charges <- function(client, limit = 10L) {
  check_limit(limit)

  dat <- fetch(client, "charges", limit)

  cols <- get_cols("charges")
  check_missing_cols(colnames(dat), cols)

  dat[["amount"]] <- convert_amt_to_decimal(dat[["amount"]])
  dat[["amount_captured"]] <- convert_amt_to_decimal(dat[["amount_captured"]])
  dat[["amount_refunded"]] <- convert_amt_to_decimal(dat[["amount_refunded"]])

  dat[["created"]] <- date(as_datetime(dat[["created"]]))

  dat
}
