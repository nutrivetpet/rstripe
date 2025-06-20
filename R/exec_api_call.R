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
    dat <- as_tibble_if_inst(resp_body[["data"]])
  }

  dat
}
