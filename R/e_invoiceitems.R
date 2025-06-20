#' Fetch Invoices Items from Stripe API
#'
#' Retrieves invoices items data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing invoices items data.
#'
#' @section API Documentation: For more information about Stripe balance
#'   transactions, see: \url{https://docs.stripe.com/api/invoices/}
#'
#' @examples
#' \dontrun{
#' # Fetch test mode balance transactions
#' test_invoices <- list_invoice_items("test")
#'
#' # Fetch live mode balance transactions
#' live_invoices <- list_invoice_items("live")
#' }
#'
#' @export
list_invoice_items <- function(mode = c("test", "live"), limit = 10L) {
  check_mode(mode)
  check_limit(limit)

  dat <- exec_api_call("invoiceitems", mode, limit)

  cols <- get_cols("invoiceitems")
  check_missing_cols(colnames(dat), cols)

  unexpected_types <- setdiff(
    unique(dat[["status"]]),
    get_invoiceitems_status()
  )

  if (length(unexpected_types)) {
    abort(
      sprintf(
        "The following status are missing from column `status`: %s.",
        unexpected_types
      ),
      class = "missing_types"
    )
  }

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
