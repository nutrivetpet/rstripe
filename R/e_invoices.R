#' Fetch Invoices from Stripe API
#'
#' Retrieves invoices data from the Stripe API.
#'
#' @inheritParams list_balance_transactions
#'
#' @return A data frame (tibble if available) containing invoices data.
#'
#' @section API Documentation: For more information about Stripe invoices, see:
#' \url{https://docs.stripe.com/api/invoices/}
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' test_invoices <- list_invoices(client)
#' live_invoices <- list_invoices(rstripe("live"))
#' }
#'
#' @export
list_invoices <- function(client, limit = 10L) {
  check_limit(limit)

  dat <- fetch(client, "invoices", limit)

  cols <- get_cols("invoices")
  check_missing_cols(colnames(dat), cols)

  unexpected_types <- setdiff(
    unique(dat[["status"]]),
    get_invoices_status()
  )

  if (length(unexpected_types)) {
    stripe_abort_missing_types(unexpected_types, "status")
  }

  for (col in c(
    "amount_due",
    "amount_overpaid",
    "amount_paid",
    "amount_remaining",
    "amount_shipping",
    "subtotal",
    "subtotal_excluding_tax",
    "tax",
    "total",
    "total_excluding_tax"
  )) {
    dat[[col]] <- convert_amt_to_decimal(dat[[col]])
  }

  for (col in c(
    "created",
    "due_date",
    "effective_at",
    "period_end",
    "period_start",
    "webhooks_delivered_at"
  )) {
    dat[[col]] <- date(as_datetime(dat[[col]]))
  }
  dat
}

get_invoices_status <- function() {
  c("draft", "open", "paid", "uncollectable", "void")
}
