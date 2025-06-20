check_limit <- function(limit) {
  stopifnot((limit >= 1L && limit <= 100L) || is.infinite(limit))
}

check_mode <- function(mode) {
  arg_match(mode, c("test", "live"))
}

check_missing_cols <- function(x, truth) {
  missing <- setdiff(truth, x)
  if (length(missing)) {
    abort(
      sprintf("The following columns are missing: %s.", missing),
      class = "missing_columns"
    )
  }
}
