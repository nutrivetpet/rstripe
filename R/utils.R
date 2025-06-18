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
  key
}

build_req <- function(api_key, endpoint) {
  stopifnot(
    is_scalar_character(api_key),
    is_scalar_character(endpoint)
  )

  request("https://api.stripe.com/v1") |>
    req_url_path_append(endpoint) |>
    req_auth_basic(username = api_key, password = "")
}

is_null <- function(x) {
  is.null(x)
}

convert_stripe_amount_to_decimal <- function(x) {
  stopifnot(rlang::is_integer(x))
  x / 100L
}
