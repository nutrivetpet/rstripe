build_req <- function(client, endpoint, limit, query = NULL) {
  stopifnot(S7::S7_inherits(client, Rstripe))
  req <- request(client@base_url) |>
    req_url_path_append(endpoint) |>
    req_url_query("limit" = limit) |>
    req_auth_basic(username = client@api_key, password = "") |>
    req_timeout(client@timeout)

  if (length(query)) {
    req <- exec(req_url_query, req, !!!query)
  }
  req
}
