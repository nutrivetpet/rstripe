#' Fetch Products from Stripe API
#'
#' Retrieves products data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing products data.
#'
#' @section API Documentation: For more information about Stripe products, see:
#' \url{https://docs.stripe.com/api/products/}
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' test_products <- list_products(client)
#' live_products <- list_products(rstripe("live"))
#' }
#'
#' @export
list_products <- function(client, limit = 10L) {
  check_limit(limit)

  dat <- fetch(client, "products", limit)

  cols <- get_cols("products")
  check_missing_cols(colnames(dat), cols)

  dat[["created"]] <- date(as_datetime(dat[["created"]]))
  dat[["updated"]] <- date(as_datetime(dat[["updated"]]))
  dat
}
