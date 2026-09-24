AI_KEY_VALIDATION_TIMEOUT <- 10L

AI_KEY_VALIDATION_ENDPOINTS <- list(
  openai = list(
    url = "https://api.openai.com/v1/models",
    headers = function(key) add_headers(Authorization = paste("Bearer", key))
  ),
  anthropic = list(
    url = "https://api.anthropic.com/v1/models",
    headers = function(key) add_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01")
  ),
  google = list(
    url = "https://generativelanguage.googleapis.com/v1beta/models",
    headers = function(key) add_headers(`x-goog-api-key` = key)
  )
)

# only these say something about the key itself; rate limits, outages etc. do not
isAiKeyRejected <- function(provider, statusCode) {
  return(statusCode %in% c(401L, 403L) || (identical(provider, "google") && statusCode == 400L))
}

#' Returns "valid", "invalid" (the provider rejected the key) or "unreachable"
#' (the key could not be verified, which says nothing about the key itself).
validateAiApiKey <- function(provider, apiKey) {
  endpoint <- AI_KEY_VALIDATION_ENDPOINTS[[provider]]
  apiKey <- trimws(apiKey)
  if (is.null(endpoint) || nchar(apiKey) < 10L) {
    return("invalid")
  }
  resp <- tryCatch(
    GET(endpoint$url, endpoint$headers(apiKey), timeout(AI_KEY_VALIDATION_TIMEOUT)),
    error = function(err) err
  )
  if (inherits(resp, "error")) {
    flog.warn("Could not reach AI provider: %s to validate API key. Error message: %s", provider, conditionMessage(resp))
    return("unreachable")
  }
  statusCode <- status_code(resp)
  if (statusCode == 200L) {
    return("valid")
  }
  if (isAiKeyRejected(provider, statusCode)) {
    flog.info("AI provider: %s rejected API key (HTTP %d).", provider, statusCode)
    return("invalid")
  }
  flog.warn("Could not validate API key for AI provider: %s (HTTP %d).", provider, statusCode)
  return("unreachable")
}
