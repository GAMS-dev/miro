library(futile.logger)
library(R6)
library(DBI)
library(jsonlite)
library(yaml)
library(httr)

ENGINE_URL <- Sys.getenv("MIRO_ENGINE_HOST")
ENGINE_NAMESPACE <- Sys.getenv("MIRO_ENGINE_NAMESPACE")
ENGINE_TOKEN <- Sys.getenv("SHINYPROXY_WEBSERVICE_ACCESS_TOKEN")

LOGIN_REQUIRED <- Sys.getenv("SHINYPROXY_NOAUTH") == "true"
IN_KUBERNETES <- Sys.getenv("KUBERNETES_SERVICE_HOST", "") != ""

REQUIRED_API_VERSION <- 1
MAX_LOGO_SIZE <- 1e6
MIRO_VERSION <- "2.11.2"
MIRO_CONTAINER_DATA_DIR <- if (IN_KUBERNETES) "/home/miro/mnt/data" else "/home/miro/app/data"
MIRO_MODEL_DIR <- file.path(getwd(), if (IN_KUBERNETES) "mnt" else "", "models")
MIRO_DATA_DIR <- file.path(getwd(), if (IN_KUBERNETES) "mnt" else "", "data")

MIRO_APP_PATH <- "/home/miro/app"

ENFORCE_SIGNED_APPS <- identical(Sys.getenv("MIRO_ENFORCE_SIGNED_APPS"), "true")

DEFAULT_LOGO_B64 <- character(0L)

flog.appender("appender.console")
flog.threshold("TRACE")

lang <- fromJSON(file.path(MIRO_APP_PATH, "conf", "en.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)
