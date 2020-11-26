library(futile.logger)
library(R6)
library(jsonlite)
library(yaml)
library(httr)

ENGINE_URL        <- Sys.getenv("MIRO_ENGINE_HOST")
ENGINE_NAMESPACE  <- Sys.getenv("MIRO_ENGINE_NAMESPACE")
ENGINE_ADMIN_USER <- Sys.getenv("MIRO_ENGINE_ADMIN_USER")
ENGINE_ADMIN_PWD  <- Sys.getenv("MIRO_ENGINE_ADMIN_PASS")

USERNAME          <- Sys.getenv("MIRO_ADMIN_USER")
PASSWORD          <- Sys.getenv("SHINYPROXY_PASSWORD")
LOGIN_REQUIRED    <- Sys.getenv("SHINYPROXY_NOAUTH") == "true" && PASSWORD != ""

REQUIRED_API_VERSION <- 1
MAX_LOGO_SIZE     <- 1e6
MIRO_VERSION      <- "1.1.99"
MIRO_CONTAINER_DATA_DIR <- "/home/miro/app/data"
MIRO_MODEL_DIR    <- "models/"
MIRO_DATA_DIR     <- "data/"

MIRO_APP_PATH     <- "/home/miro/app"

DEFAULT_LOGO_B64  <- character(0L)

flog.appender("appender.console")
flog.threshold("TRACE")
