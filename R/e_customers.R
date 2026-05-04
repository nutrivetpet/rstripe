#' Fetch Customers from Stripe API
#'
#' Retrieves customers data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing customers data.
#'
#' @section API Documentation: For more information about Stripe customers, see:
#' \url{https://docs.stripe.com/api/customers/}
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' test_customers <- list_customers(client)
#' live_customers <- list_customers(rstripe("live"))
#' }
#'
#' @export
list_customers <- function(client, limit = 10L) {
  check_limit(limit)

  dat <- fetch(client, "customers", limit)

  cols <- get_cols("customers")
  check_missing_cols(colnames(dat), cols)

  dat[["created"]] <- date(as_datetime(dat[["created"]]))
  dat
}
