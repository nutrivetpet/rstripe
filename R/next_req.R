next_req <- function(resp, req) {
  resp <- resp_body_json(resp, simplifyVector = TRUE)

  has_more <- resp[["has_more"]]
  if (is_null(has_more) || !is_scalar_logical(has_more)) {
    stripe_abort_incorrect_has_more()
  }

  dat <- resp[["data"]]
  if (is_null(dat) || (!is.data.frame(dat) && !nrow(dat))) {
    stripe_abort_empty_response()
  }

  if (has_more) {
    last_id <- utils::tail(dat, 1L)[["id"]]
    req_url_query(req, starting_after = last_id)
  } else {
    NULL
  }
}
