get_error_msg <- function(status) {
  stopifnot(is_integerish(status))
  switch(
    as.character(status),
    "400" = "Bad Request: The request was unacceptable, often due to missing a required parameter.",
    "401" = "Unauthorized: No valid API key provided.",
    "402" = "Request Failed: The parameters were valid but the request failed.",
    "403" = "Forbidden: The API key doesn't have permissions to perform the request.",
    "404" = "Not Found: The requested resource doesn't exist.",
    "409" = "Conflict: The request conflicts with another request (perhaps due to using the same idempotent key).",
    "424" = "External Dependency Failed: The request couldn't be completed due to a failure in a dependency external to Stripe.",
    "429" = "Too Many Requests: Too many requests hit the API too quickly. We recommend an exponential backoff of your requests.",
    "500" = "Server Error: Something went wrong on Stripe's end.",
    "502" = "Server Error: Something went wrong on Stripe's end.",
    "503" = "Server Error: Something went wrong on Stripe's end.",
    "504" = "Server Error: Something went wrong on Stripe's end.",
    paste("HTTP", status, "error occurred")
  )
}
