#' Fetch Customers from Stripe API
#'
#' Retrieves customers data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing balance transaction
#'   data.
#'
#' @section API Documentation: For more information about Stripe balance
#'   transactions, see: \url{https://docs.stripe.com/api/customers/}
#'
#' @examples
#' \dontrun{
#' # Fetch test mode balance transactions
#' test_customers <- list_customers("test")
#'
#' # Fetch live mode balance transactions
#' live_customers <- list_customers("live")
#' }
#'
#' @export
list_customers <- function(mode, limit = 10L) {
  check_mode(mode)
  check_limit(limit)

  dat <- exec_api_call("customers", mode, limit)

  cols <- get_balance_transactions_cols()
  check_missing_cols(colnames(dat), cols)

  dat[["created"]] <- lubridate::date(lubridate::as_datetime(dat[["created"]]))
  dat
}

get_customers_cols <- function() {
  c(
    "id",
    "object",
    "address",
    "balance",
    "created",
    "currency",
    "default_source",
    "delinquent",
    "description",
    "email",
    "invoice_prefix",
    "invoice_settings",
    "livemode",
    "metadata",
    "name",
    "next_invoice_sequence",
    "phone",
    "preferred_locales",
    "shipping",
    "tax_exempt",
    "test_clock"
  )
}
