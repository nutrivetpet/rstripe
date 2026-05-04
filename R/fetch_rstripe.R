#' @noRd
fetch <- S7::new_generic("fetch", "client")

fetch_rstripe <- function(client, endpoint, limit, query = NULL) {
  if (is.infinite(limit)) {
    exec_paginated_call(client, endpoint, query)
  } else {
    exec_single_call(client, endpoint, limit, query)
  }
}

exec_paginated_call <- function(client, endpoint, query = NULL) {
  req <- build_req(client, endpoint, limit = STRIPE_MAX_LIMIT, query = query)

  resps <- req_perform_iterative(
    req,
    next_req = next_req,
    max_reqs = Inf,
    on_error = "return"
  )

  resps_successes_dat <- xtr_data(resps)
  resps_failures <- resps_failures(resps)
  throw_errors(resps_failures)

  as_tibble_if_inst(resps_successes_dat)
}

exec_single_call <- function(client, endpoint, limit, query = NULL) {
  req <- build_req(client, endpoint, limit, query = query)
  resp <- req_perform(req)
  handle_single_response(resp)
}

handle_single_response <- function(resp) {
  if (resp_is_error(resp)) {
    stripe_abort_api_error(resp_status(resp))
  }

  resp_body <- resp_body_json(resp, simplifyVector = TRUE)
  dat <- resp_body[["data"]]

  validate_response_data(dat)
  as_tibble_if_inst(dat)
}

validate_response_data <- function(dat) {
  if (is_null(dat) || (!is.data.frame(dat) && !nrow(dat))) {
    stripe_abort_empty_response()
  }
}

xtr_data <- function(resps) {
  stopifnot(is.list(resps))
  if (!is_installed("vctrs")) {
    stripe_abort("`resps_data()` requires the {vctrs} package to be installed.")
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
  if (!length(resps_failures)) {
    return(invisible())
  }

  errors <- lapply(resps_failures, `[[`, "status")
  statuses <- unlist(Filter(
    function(x) is_scalar_integerish(x, finite = TRUE),
    errors
  ))
  others <- Filter(
    function(x) !is_scalar_integerish(x, finite = TRUE),
    errors
  )

  if (length(statuses)) {
    stripe_abort_api_errors(statuses)
  }

  if (length(others)) {
    stripe_abort(sprintf("%d non-API error(s) encountered.", length(others)))
  }
}
