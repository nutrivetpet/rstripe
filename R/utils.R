is_null <- function(x) {
  is.null(x)
}

convert_amt_to_decimal <- function(x) {
  stopifnot(is_integer(x))
  x / 100L
}

as_tibble_if_inst <- function(dat) {
  stopifnot(is.data.frame(dat))
  if (is_installed("tibble")) {
    dat <- tibble::as_tibble(dat)
  }
  dat
}

as_datetime <- function(x, tz = "UTC") {
  stopifnot(is_integer(x))
  as.POSIXlt(x, tz)
}

date <- function(x) {
  as.Date(format(x, "%Y-%m-%d"))
}
