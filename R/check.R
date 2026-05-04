check_limit <- function(limit) {
  stopifnot(
    (limit >= STRIPE_MIN_LIMIT && limit <= STRIPE_MAX_LIMIT) ||
      is.infinite(limit)
  )
}

check_missing_cols <- function(x, truth) {
  missing <- setdiff(truth, x)
  if (length(missing)) {
    stripe_abort_missing_columns(missing)
  }
}
