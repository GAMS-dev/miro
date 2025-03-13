ModelConfig <- R6::R6Class("ModelConfig",
  public = list(
    initialize = function(configPath) {
      private$configPath <- configPath

      accessGroupsTmp <- toupper(unique(csv2Vector(Sys.getenv("SHINYPROXY_USERGROUPS"))))

      private$accessGroups <- accessGroupsTmp[!accessGroupsTmp %in% c("USERS", "ADMINS")]

      if (file.exists(configPath)) {
        configTmp <- private$readConfig()
        modelConfigsHasAccess <- vapply(configTmp, function(appConfig) {
          return(private$appIsVisible(appConfig))
        }, logical(1L), USE.NAMES = FALSE)
        private$currentModelConfigs <- configTmp[modelConfigsHasAccess]
        private$modelConfigsNoAccess <- configTmp[!modelConfigsHasAccess]
      } else {
        stop(sprintf("Could not find configuration file: %s", configPath), call. = FALSE)
      }
      return(invisible(self))
    },
    getAccessGroupUnion = function() {
      return(I(private$accessGroups))
    },
    getModelIds = function(modelIndex) {
      if (length(private$currentModelConfigs) < modelIndex) {
        stop(sprintf("Model with index: %s does not exist", modelIndex), call. = FALSE)
      }
      return(list(
        private$currentModelConfigs[[modelIndex]][["id"]],
        private$currentModelConfigs[[model_index]][["containerEnv"]][["MIRO_MODEL_PATH"]][["value"]]
      ))
    },
    setAppsNotOnEngine = function(appIds) {
      private$appsNotOnEngine <- appIds
      return(invisible(self))
    },
    getConfigList = function() {
      return(lapply(seq_along(private$currentModelConfigs), self$getAppConfig))
    },
    getAllAppIds = function(includeNoAccess = FALSE) {
      if (includeNoAccess) {
        private$refreshModelConfigsNoAccess()
        return(vapply(c(private$modelConfigsNoAccess, private$currentModelConfigs), "[[", character(1L), "id", USE.NAMES = FALSE))
      }
      return(vapply(private$currentModelConfigs, "[[", character(1L), "id", USE.NAMES = FALSE))
    },
    getAppId = function(appIndex) {
      return(private$currentModelConfigs[[appIndex]]$id)
    },
    getAppIndex = function(appId) {
      for (appIndex in seq_along(private$currentModelConfigs)) {
        if (identical(private$currentModelConfigs[[appIndex]][["id"]], appId)) {
          return(appIndex)
        }
      }
      stop("App id not found (error 1789236).", call. = FALSE)
    },
    getModelName = function(appIndex) {
      return(
        tools::file_path_sans_ext(
          basename(
            private$currentModelConfigs[[appIndex]][["containerEnv"]][["MIRO_MODEL_PATH"]][["value"]]
          )
        )
      )
    },
    getAppLogo = function(appIndex) {
      return(private$currentModelConfigs[[appIndex]]$logoURL)
    },
    getEnvValue = function(envConfig) {
      if (length(envConfig) == 1L && is.character(envConfig)) {
        return(envConfig)
      }
      return(envConfig[["value"]])
    },
    add = function(newConfig) {
      if ("accessGroups" %in% names(newConfig)) {
        newConfig[["accessGroups"]] <- as.list(toupper(newConfig[["accessGroups"]]))
      }
      newConfig[["containerEnv"]] <- private$fixContainerEnv(newConfig[["containerEnv"]])
      private$currentModelConfigs <- c(private$currentModelConfigs, list(newConfig))

      private$writeConfig()
      return(invisible(self))
    },
    remove = function(appIndex) {
      if (appIndex > length(private$currentModelConfigs)) {
        stop(sprintf("An app with index: %s does not exists.", as.character(appIndex)),
          call. = FALSE
        )
      }
      visibleAppIds <- self$getAllAppIds()
      private$currentModelConfigs[[appIndex]] <- NULL
      private$writeConfig(visibleAppIds)

      return(invisible(self))
    },
    update = function(appIndex, newConfig, allowUpdateRestrictedEnv = FALSE) {
      if (length(appIndex) != 1 || is.na(appIndex) ||
        appIndex > length(private$currentModelConfigs)) {
        stop("Invalid app index.", call. = FALSE)
      }
      if ("containerEnv" %in% names(newConfig)) {
        newConfig[["containerEnv"]] <- private$fixContainerEnv(newConfig[["containerEnv"]])
        for (envKey in c(
          names(newConfig[["containerEnv"]]),
          names(private$currentModelConfigs[[appIndex]][["containerEnv"]])
        )) {
          if (!allowUpdateRestrictedEnv && envKey %in% RESTRICTED_ENV_KEYS) {
            if (envKey %in% names(newConfig[["containerEnv"]])) {
              flog.warn("Invalid environment variable name: %s in custom environment file. It was ignored.", envKey)
            }
            next
          }
          if (envKey %in% names(newConfig[["containerEnv"]])) {
            private$currentModelConfigs[[appIndex]][["containerEnv"]][[envKey]] <- newConfig[["containerEnv"]][[envKey]]
            next
          }
          private$currentModelConfigs[[appIndex]][["containerEnv"]][[envKey]] <- NULL
        }
      }
      for (configId in c("displayName", "description", "logoURL", "accessGroups", "extraData", "faviconPath")) {
        if (configId %in% names(newConfig)) {
          if (identical(configId, "displayName")) {
            if (nchar(trimws(newConfig[["displayName"]])) < 1) {
              stop("App title must contain at least 1 non-whitespace character.",
                call. = FALSE
              )
            }
          } else if (identical(configId, "accessGroups")) {
            currentAccessGroups <- private$currentModelConfigs[[appIndex]][["accessGroups"]]
            accessGroupsNoAccess <- currentAccessGroups[!toupper(currentAccessGroups) %in% private$accessGroups]
            if (length(newConfig[["accessGroups"]]) > 0) {
              newConfig[["accessGroups"]] <- as.list(unique(c(toupper(newConfig[["accessGroups"]]), accessGroupsNoAccess)))
            } else {
              newConfig[["accessGroups"]] <- as.list(accessGroupsNoAccess)
            }
          }
          private$currentModelConfigs[[appIndex]][[configId]] <- newConfig[[configId]]
        }
      }

      private$writeConfig()

      return(invisible(self))
    },
    swapApps = function(appIdFrom, appIdTo) {
      appIds <- vapply(private$currentModelConfigs, "[[", character(1), "id",
        USE.NAMES = FALSE
      )
      appIndices <- match(c(appIdFrom, appIdTo), appIds)
      if (any(is.na(appIndices))) {
        stop(sprintf(
          "One of the app IDs provided (%s, %s) was not found.",
          appIdFrom, appIdTo
        ), call. = FALSE)
      }
      appConfigTo <- private$currentModelConfigs[[appIndices[2]]]
      private$currentModelConfigs[[appIndices[2]]] <- private$currentModelConfigs[[appIndices[1]]]
      private$currentModelConfigs[[appIndices[1]]] <- appConfigTo
      private$writeConfig()

      return(invisible(self))
    },
    getAppConfigFull = function(id) {
      for (modelConfig in private$currentModelConfigs) {
        if (identical(modelConfig$id, id)) {
          return(modelConfig)
        }
      }
      stop(sprintf("A MIRO app with the id: %s does not exist.", id), call. = FALSE)
    },
    getAppDbConf = function(id) {
      configFull <- self$getAppConfigFull(id)
      return(list(
        user = configFull[["containerEnv"]][["MIRO_DB_USERNAME"]][["value"]],
        password = configFull[["containerEnv"]][["MIRO_DB_PASSWORD"]][["value"]]
      ))
    },
    getAppConfig = function(index) {
      appConfig <- private$currentModelConfigs[[index]]
      if ("logoURL" %in% names(appConfig)) {
        logoB64 <- tryCatch(getLogoB64(file.path(
          LOGO_DIR, appConfig[["logoURL"]]
        )), error = function(e) {
          flog.info("Problems reading app logo. Default logo will be used. Error: %s", conditionMessage(e))
          return(DEFAULT_LOGO_B64)
        })
      } else {
        logoB64 <- DEFAULT_LOGO_B64
      }

      accessGroups <- character()

      if ("accessGroups" %in% names(appConfig) && length(appConfig[["accessGroups"]])) {
        accessGroups <- appConfig[["accessGroups"]]
        accessGroups <- accessGroups[toupper(accessGroups) %in% private$accessGroups]
      }

      appEnv <- list()
      for (envKey in names(appConfig[["containerEnv"]])) {
        if (!envKey %in% RESTRICTED_ENV_KEYS) {
          appEnv[[envKey]] <- appConfig[["containerEnv"]][[envKey]]
        }
      }
      if (is.null(appConfig[["extraData"]][["appAuthors"]])) {
        appAuthors <- character()
      } else {
        appAuthors <- appConfig[["extraData"]][["appAuthors"]]
      }

      return(list(
        id = appConfig[["id"]], alias = appConfig[["displayName"]],
        desc = appConfig[["description"]], logob64 = logoB64,
        appEnv = appEnv,
        groups = I(accessGroups),
        version = appConfig[["extraData"]][["appVersion"]],
        authors = I(appAuthors),
        isDirty = appConfig[["id"]] %in% private$appsNotOnEngine
      ))
    }
  ),
  private = list(
    configPath = NULL,
    accessGroups = NULL,
    currentModelConfigs = NULL,
    modelConfigsNoAccess = NULL,
    appsNotOnEngine = NULL,
    fixContainerEnv = function(containerEnvRaw) {
      if (is.null(containerEnvRaw)) {
        return(list())
      }
      return(lapply(containerEnvRaw, function(envConfig) {
        if (length(envConfig) == 1L && is.character(envConfig)) {
          return(list(value = envConfig, description = ""))
        }
        return(envConfig)
      }))
    },
    appIsVisible = function(appConfig) {
      return(!identical(appConfig[["id"]], "admin") &&
        (!length(appConfig[["accessGroups"]]) || any(appConfig[["accessGroups"]] %in% private$accessGroups)))
    },
    refreshModelConfigsNoAccess = function(visibleAppIds = NULL) {
      configTmp <- private$readConfig()
      if (is.null(visibleAppIds)) {
        visibleAppIds <- self$getAllAppIds()
      }
      modelConfigsHasAccess <- vapply(configTmp, function(appConfig) {
        return(appConfig[["id"]] %in% visibleAppIds)
      }, logical(1L), USE.NAMES = FALSE)
      private$modelConfigsNoAccess <- configTmp[!modelConfigsHasAccess]
      return(invisible(self))
    },
    readConfig = function() {
      configTmp <- tryCatch(yaml::read_yaml(private$configPath),
        error = function(e) {
          stop(sprintf(
            "Faulty yaml syntax in configuration file: %s. Error message: %s",
            private$configPath, conditionMessage(e)
          ), call. = FALSE)
        }
      )
      return(lapply(configTmp[["specs"]], function(appConfig) {
        containerEnvNames <- names(appConfig[["containerEnv"]])
        appConfig[["containerEnv"]] <- lapply(containerEnvNames, function(envName) {
          return(list(
            value = appConfig[["containerEnv"]][[envName]],
            description = appConfig[["extraData"]][["containerEnvSettings"]][[envName]][["description"]]
          ))
        })
        names(appConfig[["containerEnv"]]) <- containerEnvNames
        appConfig[["accessGroups"]] <- as.list(appConfig[["accessGroups"]])
        return(appConfig)
      }))
    },
    writeConfig = function(visibleAppIds = NULL) {
      private$refreshModelConfigsNoAccess(visibleAppIds)
      appSpecsTmp <- list(specs = c(private$modelConfigsNoAccess, private$currentModelConfigs))
      allAppIds <- vapply(appSpecsTmp[["specs"]], "[[", character(1L), "id", USE.NAMES = FALSE)
      duplicatedAppIds <- duplicated(allAppIds)
      if (any(duplicatedAppIds)) {
        stop_custom("error_bad_config",
          sprintf(
            "Duplicated app id(s): %s found when trying to save specs.yaml file.",
            allAppIds[duplicatedAppIds]
          ),
          call. = FALSE
        )
      }
      appSpecsTmp$specs <- lapply(appSpecsTmp$specs, function(appConfig) {
        if (is.null(appConfig[["extraData"]])) {
          appConfig[["extraData"]] <- list()
        }
        appConfig[["extraData"]][["containerEnvSettings"]] <- lapply(appConfig[["containerEnv"]], function(envConfig) {
          return(list(description = envConfig[["description"]]))
        })
        appConfig[["containerEnv"]] <- lapply(appConfig[["containerEnv"]], "[[", "value")
        return(appConfig)
      })
      yaml::write_yaml(
        appSpecsTmp,
        private$configPath
      )
    }
  )
)
