parse_date_bound <- function(x, arg, end_of_day = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }

  if (inherits(x, "Date")) {
    d <- x
  } else if (is_scalar_character(x) && grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    d <- tryCatch(as.Date(x), error = function(e) NA_real_)
    if (is.na(d)) {
      stripe_abort(
        sprintf("`%s` must be a Date or a 'YYYY-MM-DD' string.", arg),
        class = "invalid_date"
      )
    }
  } else {
    stripe_abort(
      sprintf("`%s` must be a Date or a 'YYYY-MM-DD' string.", arg),
      class = "invalid_date"
    )
  }

  ts <- as.integer(as.POSIXct(d, tz = "UTC"))
  if (end_of_day) ts + 86399L else ts
}

build_created_filter <- function(
  gte = NULL,
  lte = NULL,
  gte_arg = "created_gte",
  lte_arg = "created_lte"
) {
  g <- parse_date_bound(gte, gte_arg, end_of_day = FALSE)
  l <- parse_date_bound(lte, lte_arg, end_of_day = TRUE)

  if (!is.null(g) && !is.null(l) && l < g) {
    stripe_abort(
      sprintf("`%s` must be on or after `%s`.", lte_arg, gte_arg),
      class = "invalid_date_range"
    )
  }

  q <- list()
  if (!is.null(g)) {
    q[["created[gte]"]] <- g
  }
  if (!is.null(l)) {
    q[["created[lte]"]] <- l
  }
  q
}
