#' Fetch Products from Stripe API
#'
#' Retrieves products data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing products data.
#'
#' @section API Documentation: For more information about Stripe balance
#'   transactions, see: \url{https://docs.stripe.com/api/products/}
#'
#' @examples
#' \dontrun{
#' # Fetch test mode balance transactions
#' test_products <- list_products("test")
#'
#' # Fetch live mode balance transactions
#' live_products <- list_products("live")
#' }
#'
#' @export
list_products <- function(mode = c("test", "live"), limit = 10L) {
  check_mode(mode)
  check_limit(limit)

  dat <- exec_api_call("products", mode, limit)

  cols <- get_cols("products")
  check_missing_cols(colnames(dat), cols)

  dat[["created"]] <- date(as_datetime(dat[["created"]]))
  dat[["updated"]] <- date(as_datetime(dat[["updated"]]))
  dat
}
