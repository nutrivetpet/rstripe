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
list_customers <- function(mode = c("test", "live"), limit = 10L) {
  check_mode(mode)
  check_limit(limit)

  dat <- exec_api_call("customers", mode, limit)

  cols <- get_cols("customers")
  check_missing_cols(colnames(dat), cols)

  dat[["created"]] <- lubridate::date(lubridate::as_datetime(dat[["created"]]))
  dat
}
