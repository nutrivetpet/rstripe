exec_api_call <- function(epoint, mode, limit) {
  if (is.infinite(limit)) {
    limit <- 100L
  }

  api_key <- get_api_key(mode)
  req <- build_req(
    api_key,
    endpoint = epoint,
    limit
  )

  resps <- req_perform_iterative(
    req,
    next_req = next_req,
    max_reqs = Inf,
    on_error = "return" # error objects are stored at the end
  )

  resps_successes_dat <- xtr_data(resps)

  dat <- as_tibble_if_inst(resps_successes_dat)

  resps_failures <- resps_failures(resps)

  throw_errors(resps_failures)

  dat
}
