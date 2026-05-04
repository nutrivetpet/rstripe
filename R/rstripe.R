#' @noRd
Rstripe <- S7::new_class(
  name = "Rstripe",
  properties = list(
    api_key = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1L || !nzchar(value)) {
          return("`api_key` must be a single non-empty string")
        }
        if (!grepl("^sk_(test|live)_", value)) {
          return("`api_key` must start with `sk_test_` or `sk_live_`")
        }
      }
    ),
    mode = S7::new_property(
      class = S7::class_character,
      validator = function(value) {
        if (length(value) != 1L || !value %in% c("test", "live")) {
          return("`mode` must be either 'test' or 'live'")
        }
      }
    ),
    base_url = S7::new_property(
      class = S7::class_character,
      default = STRIPE_BASE_URL
    ),
    timeout = S7::new_property(
      class = S7::class_numeric,
      default = STRIPE_DEFAULT_TIMEOUT
    )
  ),
  validator = function(self) {
    prefix <- switch(self@mode, test = "sk_test_", live = "sk_live_")
    if (!startsWith(self@api_key, prefix)) {
      return(sprintf(
        "`api_key` starts with a prefix that does not match `mode = \"%s\"`",
        self@mode
      ))
    }
  }
)

#' Create a Stripe API client
#'
#' Constructs an `Rstripe` S7 object holding configuration for interacting
#' with the Stripe API: the API key, the mode (test or live), the base
#' URL, and the request timeout. When `api_key` is not provided, the key
#' is read from `STRIPE_API_KEY_TEST` or `STRIPE_API_KEY_LIVE` depending
#' on `mode`.
#'
#' @param mode One of `"test"` or `"live"`.
#' @param api_key Optional API key. If omitted, the appropriate environment
#'   variable is read. Must start with `sk_test_` or `sk_live_` and match
#'   `mode`.
#' @param base_url Base URL for the Stripe API.
#' @param timeout Request timeout in seconds.
#'
#' @return An `Rstripe` S7 object.
#' @aliases Rstripe
#' @export
#'
#' @examples
#' \dontrun{
#' client <- rstripe("test")
#' }
rstripe <- function(
  mode = c("test", "live"),
  api_key = NULL,
  base_url = STRIPE_BASE_URL,
  timeout = STRIPE_DEFAULT_TIMEOUT
) {
  mode <- arg_match(mode)
  from_env <- is.null(api_key)
  if (from_env) {
    api_key <- get_api_key(mode)
  } else {
    warn(
      paste0(
        "Passing `api_key` directly is discouraged. ",
        "Set `STRIPE_API_KEY_TEST` or `STRIPE_API_KEY_LIVE` instead."
      ),
      class = "rstripe_explicit_key"
    )
  }
  if (!grepl("^sk_(test|live)_", api_key)) {
    env_var <- if (from_env) paste0("STRIPE_API_KEY_", toupper(mode)) else NULL
    stripe_abort_incorrect_api_key(mode, env_var = env_var)
  }
  Rstripe(
    api_key = api_key,
    mode = mode,
    base_url = base_url,
    timeout = timeout
  )
}

get_api_key <- function(mode = c("test", "live")) {
  mode <- arg_match(mode)
  key <- Sys.getenv(paste0("STRIPE_API_KEY_", toupper(mode)))
  if (!nzchar(key)) {
    stripe_abort_missing_api_key(mode)
  }
  key
}

redact_api_key <- function(key) {
  if (!nzchar(key) || nchar(key) < 12L) {
    return("****")
  }
  paste0(substr(key, 1L, 8L), strrep("*", nchar(key) - 8L))
}
