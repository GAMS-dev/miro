library(httr)
library(jsonlite)


check_cran_vulnerabilities <- function(packages_df) {
  osv_api_url <- "https://api.osv.dev/v1/querybatch"

  # Validate input
  if (!is.data.frame(packages_df) || ncol(packages_df) < 2) {
    stop("Input must be a data frame with at least two columns: package_name, package_version")
  }

  # Pre-process the dataframe to fill in any missing package versions
  for (i in seq_len(nrow(packages_df))) {
    package_name <- packages_df[i, 1]
    package_version <- packages_df[i, 2]

    # Coalesce NULL/NA to "" to simplify the check
    if (is.null(package_version) || is.na(package_version)) {
      package_version <- ""
    }

    if (package_version == "") {
      message(paste0("Version for '", package_name, "' is missing. Checking local DESCRIPTION file..."))

      description_path <- file.path("r-src", package_name, "DESCRIPTION")

      if (!file.exists(description_path)) {
        stop(paste0("Cannot determine version. File not found at '", description_path, "' and no version was provided."))
      }

      # read.dcf is designed to read DESCRIPTION files
      description_content <- read.dcf(description_path)

      # Check if 'Version' field exists in the file
      if ("Version" %in% colnames(description_content)) {
        local_version <- description_content[1, "Version"]
        message(paste0("Using local source version for '", package_name, "': ", local_version))
        packages_df[i, 2] <- local_version
      } else {
        stop(paste0("Cannot determine version. 'Version' field not found in '", description_path, "'."))
      }
    }
  }

  # Create a list to hold each individual query by iterating over rows
  queries <- lapply(seq_len(nrow(packages_df)), function(i) {
    list(
      version = packages_df[i, 2],
      package = list(
        name = packages_df[i, 1],
        ecosystem = "CRAN"
      )
    )
  })

  # Construct the final request body for the batch query
  request_body <- list(queries = queries)
  json_body <- toJSON(request_body, auto_unbox = TRUE, pretty = TRUE)

  message("Checking all packages in a single batch request...")

  # Make the POST request to the OSV API
  response <- POST(
    url = osv_api_url,
    body = json_body,
    encode = "json",
    add_headers("Content-Type" = "application/json")
  )

  # Check for HTTP errors
  stop_for_status(response)

  # Parse the JSON response content
  response_content <- content(response, "parsed")

  # Process the results and collect any vulnerabilities found
  vulnerabilities_found <- c()
  vuln_data <- list()
  results <- response_content$results

  for (i in seq_along(results)) {
    result <- results[[i]]
    # The API returns an empty object {} if no vulns are found for a query.
    if (!is.null(result$vulns) && length(result$vulns) > 0) {
      package_name <- packages_df[i, 1]
      package_version <- packages_df[i, 2]

      vuln_summary <- sapply(result$vulns, function(v) {
        paste0("    - ID: ", v$id, ", Summary: ", v$summary)
      })

      package_vuln_report <- paste(
        "  - Package:", package_name, "version:", package_version, "\n",
        paste(vuln_summary, collapse = "\n")
      )
      vulnerabilities_found <- c(vulnerabilities_found, package_vuln_report)
      for (v in result$vulns) {
        vuln_data <- c(vuln_data, list(list(package = package_name, version = package_version, id = v$id)))
      }
    }
  }

  # If any vulnerabilities were collected, construct the error message and stop
  if (length(vulnerabilities_found) > 0) {
    error_message <- paste(
      "Vulnerabilities found in the following packages:\n",
      paste(vulnerabilities_found, collapse = "\n\n")
    )
    message(error_message)
  } else {
    message("Success: No vulnerabilities found for the specified packages and versions.")
  }

  return(vuln_data)
}


packageVersionMap <- read.csv("./src/miro-pkg-lock.csv", header = FALSE, stringsAsFactors = FALSE)
vulnerabilities <- check_cran_vulnerabilities(packageVersionMap)
write_json(list(vuln_data = vulnerabilities),
  "r-audit-report.json",
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)
