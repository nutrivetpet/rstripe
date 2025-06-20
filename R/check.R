check_limit <- function(limit) {
  stopifnot((limit >= 1L && limit <= 100L) || is.infinite(limit))
}

check_mode <- function(mode) {
  arg_match(mode, c("test", "live"))
}
