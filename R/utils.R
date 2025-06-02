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
