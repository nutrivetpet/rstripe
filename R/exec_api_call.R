exec_api_call <- function(epoint, mode, limit) {
  api_key <- get_api_key(mode)
  if (is.infinite(limit)) {
    limit <- 100L
  }

  req <- build_req(
    api_key,
    endpoint = epoint,
    limit
  )
  # TODO: review, can we do it better?
  if (is.infinite(limit)) {
    resps <- req_perform_iterative(
      req,
      next_req = next_req,
      max_reqs = Inf,
      on_error = "return" # error objects are stored at the end
    )

    resps_successes_dat <- xtr_data(resps)
    resps_failures <- resps_failures(resps)
    throw_errors(resps_failures)

    dat <- as_tibble_if_inst(resps_successes_dat)
  } else {
    resp <- req_perform(req)
    if (resp_is_error(resp)) {
      status <- resp_status(resp)
      msg <- get_error_msg(status)
      abort(
        msg,
        class = c("stripe_api_error", paste0("stripe_", status, "_error"))
      )
    }
    resp_body <- resp_body_json(resp, simplifyVector = TRUE)
    dat <- resp_body[["data"]]
    if (is_null(dat) || (!is.data.frame(dat) && !nrow(dat))) {
      abort("Response returned empty data.", class = "empty_response")
    }

    dat <- as_tibble_if_inst(dat)
  }

  dat
}

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
