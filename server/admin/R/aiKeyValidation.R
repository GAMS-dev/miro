AI_KEY_VALIDATION_TIMEOUT <- 10L
# a thinking model may take a while even to answer "ping"
BEDROCK_PROBE_TIMEOUT <- 60L

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

#' Amazon Bedrock has no route that lists the models a key may invoke, so the
#' key is checked with a minimal request to a model. Returns the status like
#' validateAiApiKey and, if Bedrock rejected the request, its error message:
#' an invalid key and e.g. a model without access are both HTTP 403.
validateBedrockConfig <- function(region, apiKey, modelId) {
  apiKey <- trimws(apiKey)
  # the region becomes part of the host the key is sent to
  if (!grepl("^[a-z]{2}(-gov)?-[a-z]+-[0-9]$", region) || nchar(apiKey) < 10L) {
    return(list(status = "invalid"))
  }
  resp <- tryCatch(
    POST(
      sprintf(
        "https://bedrock-runtime.%s.amazonaws.com/model/%s/converse",
        region, URLencode(modelId, reserved = TRUE)
      ),
      add_headers(Authorization = paste("Bearer", apiKey)),
      # reasoning models reject a token cap below their thinking budget
      body = list(
        messages = list(list(role = "user", content = list(list(text = "ping")))),
        inferenceConfig = list(maxTokens = 2048L)
      ),
      encode = "json", timeout(BEDROCK_PROBE_TIMEOUT)
    ),
    error = function(err) err
  )
  if (inherits(resp, "error")) {
    flog.warn("Could not reach Amazon Bedrock to validate API key. Error message: %s", conditionMessage(resp))
    return(list(status = "unreachable"))
  }
  statusCode <- status_code(resp)
  if (statusCode == 200L) {
    return(list(status = "valid"))
  }
  if (statusCode %in% c(408L, 429L) || statusCode >= 500L) {
    flog.warn("Could not validate Amazon Bedrock API key (HTTP %d).", statusCode)
    return(list(status = "unreachable"))
  }
  body <- tryCatch(content(resp, "parsed", type = "application/json"), error = function(e) NULL)
  if (!is.list(body)) {
    body <- NULL
  }
  message <- body$Message %||% body$message %||% sprintf("HTTP %d", statusCode)
  flog.info("Amazon Bedrock rejected request to model: %s (HTTP %d): %s", modelId, statusCode, message)
  return(list(status = "invalid", message = message))
}
