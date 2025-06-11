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
MAX_FAVICON_SIZE <- 100e3
MAX_ROLE_PREFIX_CREATE_ATTEMPTS <- 5L
MIRO_VERSION <- "2.12.0"

if (IN_KUBERNETES) {
  SHARED_FS_MNT_DIR <- file.path(getwd(), "mnt")
  SPECS_YAML_PATH <- file.path(SHARED_FS_MNT_DIR, "specs.yaml")
  LOGO_DIR <- file.path(SHARED_FS_MNT_DIR, "logos")
  LOGO_DIR_SP_CONTAINER <- "/home/miroproxy/mnt/logos"
} else {
  MIRO_MODEL_DIR <- file.path(getwd(), "models")
  MIRO_DATA_DIR <- file.path(getwd(), "data")
  SPECS_YAML_PATH <- file.path(MIRO_DATA_DIR, "specs.yaml")
  LOGO_DIR <- file.path(MIRO_DATA_DIR, "logos")
  LOGO_DIR_SP_CONTAINER <- "/home/miroproxy/data/logos"
}

MIRO_APP_PATH <- "/home/miro/app"

ENFORCE_SIGNED_APPS <- identical(Sys.getenv("MIRO_ENFORCE_SIGNED_APPS"), "true")

DEFAULT_LOGO_B64 <- character(0L)

MAX_ENV_VARS <- 50L

RESTRICTED_ENV_KEYS <- c(
  "MIRO_MODEL_PATH", "MIRO_DATA_DIR", "MIRO_MODE",
  "MIRO_VERSION_STRING", "MIRO_DB_USERNAME",
  "MIRO_DB_PASSWORD", "MIRO_DB_SCHEMA",
  "MIRO_ENGINE_MODELNAME", "MIRO_CACHE_DIR"
)

flog.appender("appender.console")
flog.threshold("TRACE")

lang <- fromJSON(file.path(MIRO_APP_PATH, "conf", "en.json"),
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
)
