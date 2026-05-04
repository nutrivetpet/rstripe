print_rstripe <- function(x, ...) {
  cat("<rstripe::Rstripe>\n")
  cat("  @ api_key  : chr ", redact_api_key(x@api_key), "\n", sep = "")
  cat("  @ mode     : chr ", x@mode, "\n", sep = "")
  cat("  @ base_url : chr ", x@base_url, "\n", sep = "")
  cat("  @ timeout  : num ", x@timeout, " seconds\n", sep = "")
  invisible(x)
}
