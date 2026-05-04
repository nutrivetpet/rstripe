stripe_abort <- function(
  message,
  ...,
  class = character(),
  call = caller_env()
) {
  abort(
    message = message,
    ...,
    class = c(class, "stripe_error"),
    call = call
  )
}

stripe_abort_missing_api_key <- function(mode, call = caller_env()) {
  stripe_abort(
    sprintf("Cannot find env. var. `STRIPE_API_KEY_%s`.", toupper(mode)),
    class = "missing_api_key",
    call = call
  )
}

stripe_abort_incorrect_api_key <- function(
  mode,
  env_var = NULL,
  call = caller_env()
) {
  prefix <- switch(mode, test = "sk_test_", live = "sk_live_")
  msg <- if (!is.null(env_var)) {
    sprintf("`%s` does not start with `%s`.", env_var, prefix)
  } else {
    sprintf("`api_key` must start with `%s`.", prefix)
  }
  stripe_abort(msg, class = "incorrect_api_key", call = call)
}

stripe_abort_api_error <- function(status, call = caller_env()) {
  msg <- get_error_msg(status)
  stripe_abort(
    msg,
    class = c("stripe_api_error", paste0("stripe_", status, "_error")),
    call = call
  )
}

stripe_abort_api_errors <- function(statuses, call = caller_env()) {
  msgs <- chr_ply(statuses, get_error_msg)
  stripe_abort(
    c("API Error(s)!", set_names(msgs, "x")),
    class = "stripe_api_error",
    call = call
  )
}

stripe_abort_empty_response <- function(call = caller_env()) {
  stripe_abort(
    "Response returned empty data.",
    class = "empty_response",
    call = call
  )
}

stripe_abort_missing_columns <- function(cols, call = caller_env()) {
  stripe_abort(
    sprintf(
      "The following columns are missing: %s.",
      paste0(cols, collapse = ", ")
    ),
    class = "missing_columns",
    call = call
  )
}

stripe_abort_missing_types <- function(types, column, call = caller_env()) {
  stripe_abort(
    sprintf(
      "The following %s are missing from column `%s`: %s.",
      column,
      column,
      paste0(types, collapse = ", ")
    ),
    class = "missing_types",
    call = call
  )
}

stripe_abort_incorrect_has_more <- function(call = caller_env()) {
  stripe_abort(
    "Response does not contain `has_more` or it has an unexpected type.",
    class = "incorrect_has_more",
    call = call
  )
}
