#' Fetch Invoices Items from Stripe API
#'
#' Retrieves invoices items data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing invoices items data.
#'
#' @section API Documentation: For more information about Stripe invoice items,
#'   see: \url{https://docs.stripe.com/api/invoiceitems/}
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' test_items <- list_invoice_items(client)
#' live_items <- list_invoice_items(rstripe("live"))
#' }
#'
#' @export
list_invoice_items <- function(client, limit = 10L) {
  check_limit(limit)

  dat <- fetch(client, "invoiceitems", limit)

  cols <- get_cols("invoiceitems")
  check_missing_cols(colnames(dat), cols)

  for (col in c(
    "amount",
    "unit_amount",
    "unit_amount_decimal"
  )) {
    if (is_character(dat[[col]])) {
      dat[[col]] <- as.integer(dat[[col]])
    }
    dat[[col]] <- convert_amt_to_decimal(dat[[col]])
  }

  for (col in c("date")) {
    dat[[col]] <- date(as_datetime(dat[[col]]))
  }
  dat
}
