next_req <- function(resp, req) {
  resp <- resp_body_json(resp, simplifyVector = TRUE)

  has_more <- resp[["has_more"]]
  if (is_null(has_more) || !is_scalar_logical(has_more)) {
    abort(
      "Response does not contain `has_more` or it has an unexpected type.",
      class = "incorrect_has_more"
    )
  }

  # has_more being FALSE does not mean that the current iteration does not
  # contain data, I guess
  dat <- resp[["data"]]
  if (is_null(dat) || (!is.data.frame(dat) && !nrow(dat))) {
    abort("Response returned empty data.", class = "empty_response")
  }

  if (has_more) {
    last_id <- tail(dat, 1L)[["id"]]
    # modify with new url
    req_url_query(req, starting_after = last_id)
  } else {
    NULL
  }
}
