get_api_key <- function(mode = c("test", "live")) {
  mode <- arg_match(mode)
  key <- Sys.getenv(paste0("STRIPE_API_KEY_", toupper(mode)))
  if (!nzchar(key)) {
    abort(
      sprintf(
        "Cannot find env. var. `STRIPE_API_KEY_%s`.",
        toupper(mode)
      ),
      class = "missing_api_key"
    )
  }
  if (!grepl("^sk_(test|live)_", key)) {
    abort(
      sprintf(
        "`STRIPE_API_KEY_%s` does not start with `%s`.",
        toupper(mode),
        switch(mode, test = "sk_test_", live = "sk_live_")
      ),
      class = "incorrect_api_key"
    )
  }

  key
}

build_req <- function(api_key, endpoint, limit) {
  stopifnot(
    is_scalar_character(api_key),
    is_scalar_character(endpoint),
    is_scalar_integer(limit)
  )

  request("https://api.stripe.com/v1") |>
    req_url_path_append(endpoint) |>
    req_url_query("limit" = limit) |>
    req_auth_basic(username = api_key, password = "")
}

is_null <- function(x) {
  is.null(x)
}

convert_stripe_amount_to_decimal <- function(x) {
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

xtr_data <- function(resps) {
  stopifnot(is.list(resps)) # TODO: more checks
  if (!is_installed("vctrs")) {
    abort("`resps_data()` requires the {vctrs} package to be installed.")
  }
  successes <- resps_successes(resps)
  resps_data(
    successes,
    function(resp) {
      out <- resp_body_json(resp, simplifyVector = TRUE)
      out[["data"]]
    }
  )
}

throw_errors <- function(resps_failures) {
  if (length(resps_failures)) {
    errors <- sapply(
      # can return NULL
      resps_failures,
      `[[`,
      "status"
    )
    statuses <- unlist(Filter(
      function(x) is_scalar_integerish(x, finite = TRUE),
      errors
    ))

    others <- Filter(
      function(x) !is_scalar_integerish(x, finite = TRUE),
      errors
    )

    if (length(statuses)) {
      msgs <- vapply(
        statuses,
        \(x) get_error_msg(x),
        FUN.VALUE = character(length(statuses))
      )
      abort(
        c("API Error(s)!", set_names(msgs, "x")),
        class = "stripe_api_error"
      )
    }

    if (length(others)) {
      abort("%s of non API related errors encountered.", length(others))
    }
  }
}
