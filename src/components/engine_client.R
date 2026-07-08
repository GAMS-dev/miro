EngineClient <- R6Class("EngineClient", public = list(
  initialize = function(url, username, password, namespace, useBearer = TRUE, appAccessGroups = NULL) {
    stopifnot(length(url) == 1L, length(username) == 1L, length(password) == 1, length(namespace) == 1L)
    private$url <- trimws(url, which = "right", whitespace = "/")
    private$username <- username
    private$namespace <- namespace
    private$authHeader <- private$buildAuthHeader(password, useBearer)
    private$appAccessGroups <- appAccessGroups
    return(self)
  },
  getConfig = function() {
    return(list(url = private$url, authHeader = private$authHeader, username = private$username, namespace = private$namespace))
  },
  populateVolumeInfoCard = function(session, id) {
    if (!is.null(private$observers$quota)) {
      private$observers$quota$destroy()
      private$observers$quota <- NULL
    }
    private$fetchQuotaInfo()
    private$observers$quota <- observe({
      tryCatch(
        {
          if (!unresolved(private$mQuotaResp)) {
            quotaInfo <- private$mQuotaResp$data
            if (quotaInfo[["error"]]) {
              flog.error("Problem fetching quota info. Error message: %s", quotaInfo[["message"]])
              showEl(session, "#settingsDialogUnknownError")
            } else {
              remainingQuota <- calcRemainingQuota(quotaInfo[["quotaInfo"]])
              if (is.finite(remainingQuota$volume)) {
                volumeQuotaStr <- remainingQuota$volume
                if (any(startsWith(private$url, c("https://engine.gams.com/api", "https://engine-eu.gams.com/api")))) {
                  # Engine SaaS, use $ as quota unit
                  volumeQuotaStr <- paste0(formatC(volumeQuotaStr * 36, format = "f", digits = 2, big.mark = ","), " $")
                } else if (volumeQuotaStr < 1) {
                  volumeQuotaStr <- paste0(formatC(volumeQuotaStr * 3600, format = "f", digits = 0, big.mark = ","), " s")
                } else {
                  volumeQuotaStr <- paste0(formatC(volumeQuotaStr, format = "f", digits = 2, big.mark = ","), " h")
                }
              } else {
                volumeQuotaStr <- lang$nav$dialogSettings$unlimitedQuotaLabel
              }
              if (is.finite(remainingQuota$disk)) {
                diskQuotaStr <- remainingQuota$disk
                if (diskQuotaStr > 1e3) {
                  diskQuotaStr <- paste0(formatC(diskQuotaStr / 1e3, format = "f", digits = 2, big.mark = ","), " GB")
                } else {
                  diskQuotaStr <- paste0(formatC(diskQuotaStr, format = "f", digits = 2, big.mark = ","), " MB")
                }
              } else {
                diskQuotaStr <- lang$nav$dialogSettings$unlimitedQuotaLabel
              }
              setTextContent(session, paste0("#", id, "Wrapper .quota-info-volume"), volumeQuotaStr)
              setTextContent(session, paste0("#", id, "Wrapper .quota-info-disk"), diskQuotaStr)
              showEl(session, paste0("#", id, "Wrapper"))
            }
            hideEl(session, paste0("#", id, "Spinner"))
            private$observers$quota$destroy()
          }
          invalidateLater(500L, session)
        },
        error = function(e) {
          flog.error("Unexpected error fetching quota info. Error message: %s", conditionMessage(e))
          showEl(session, "#settingsDialogUnknownError")
          hideEl(session, paste0("#", id, "Spinner"))
          private$observers$quota$destroy()
        }
      )
    })
    return(invisible(self))
  },
  populateInstanceSelector = function(session, selectizeId, dropdownCategories, forceRefresh = FALSE) {
    if (!is.null(private$observers$instances)) {
      private$observers$instances$destroy()
      private$observers$instances <- NULL
    }
    if (is.null(private$instanceInfo) || isTRUE(forceRefresh)) {
      private$fetchInstanceInfo()
    }
    getInstances <- function(instanceInfo) {
      if (!identical(instanceInfo[["error"]], FALSE)) {
        flog.error("Error fetching instances. Error message: %s", instanceInfo[["message"]])
        return(list(valid = FALSE))
      }
      if (!length(private$apiInfo)) {
        private$apiInfo <- instanceInfo[["apiInfo"]]
      }
      instanceToStr <- function(instance) {
        if ("instance" %in% names(instance)) {
          # pool instance
          instanceDetails <- instance[["instance"]]
        } else {
          instanceDetails <- instance
        }
        return(paste0(
          instance[["label"]], " (", instanceDetails[["cpu_request"]], " vCPU, ",
          instanceDetails[["memory_request"]], " MiB RAM, ", round(instanceDetails[["multiplier"]], 1), "x)"
        ))
      }
      if (identical(instanceInfo[["instancesSupported"]], FALSE)) {
        flog.info("Engine backend does not support instances.")
        return(list(valid = TRUE, instancesSupported = FALSE))
      }
      if (identical(length(instanceInfo[["instances"]]), 0L)) {
        flog.info("No instances found for user: %s.", private$username)
        return(list(valid = TRUE, instancesSupported = FALSE))
      }
      availableInstancePools <- instanceInfo[["pools"]][vapply(instanceInfo[["pools"]], function(poolInfo) {
        !identical(poolInfo[["cancelling"]], TRUE)
      }, logical(1L), USE.NAMES = FALSE)]
      availableInstancePools <- lapply(availableInstancePools, function(poolInfo) {
        poolInfo$owner <- poolInfo$owner$username
        for (instanceKey in names(poolInfo$instance)) {
          if (instanceKey %in% names(poolInfo)) {
            instanceKey <- paste("instance_", instanceKey)
          }
          poolInfo[[instanceKey]] <- poolInfo$instance[[instanceKey]]
        }
        poolInfo$instance_type <- "pool"
        return(poolInfo)
      })
      availableInstances <- c(instanceInfo[["instances"]], availableInstancePools)
      availableInstances <- lapply(availableInstances, function(instanceInfo) {
        instanceInfo$tolerations <- NULL
        instanceInfo$node_selectors <- NULL
        if (is.null(instanceInfo$instance_type)) {
          instanceInfo$instance_type <- "instance"
        }
        return(instanceInfo)
      })
      defaultInstance <- instanceInfo[["default"]][["label"]]
      return(list(valid = TRUE, instancesSupported = TRUE, choices = availableInstances, selected = defaultInstance))
    }
    private$observers$instances <- observe({
      tryCatch(
        {
          if (is.null(private$mInstanceResp)) {
            instanceInfo <- private$instanceInfo
          } else if (unresolved(private$mInstanceResp)) {
            invalidateLater(500L, session)
            return()
          } else {
            instanceInfo <- getInstances(private$mInstanceResp$data)
            private$mInstanceResp <- NULL
          }
          if (!instanceInfo[["valid"]]) {
            showEl(session, "#settingsDialogUnknownError")
            hideEl(session, paste0("#", selectizeId, "Spinner"))
            private$observers$instances$destroy()
            return()
          }
          private$instanceInfo <- instanceInfo
          if (!identical(instanceInfo[["instancesSupported"]], TRUE)) {
            hideEl(session, paste0("#", selectizeId, "Spinner"))
            private$observers$instances$destroy()
            return()
          }
          updateSelectizeInput(session, selectizeId,
            choices = arrange(
              bind_rows(instanceInfo[["choices"]]),
              factor(instance_type, levels = c("pool", "instance")),
              stringr::str_to_lower(label),
              label
            ),
            selected = instanceInfo[["selected"]],
            server = TRUE,
            options = list(
              valueField = "label",
              searchField = "label",
              labelField = "label",
              optgroupField = "instance_type",
              render = I(paste0("{optgroup_header: function(item,escape){
                                        let label = ", toJSString(dropdownCategories[[1L]]), ";
                                        if (item.value==='instance') {
                                            label = ", toJSString(dropdownCategories[[2L]]), ";
                                        }
                                        return `<div class=\"optgroup-header\">${escape(label)}</div>`
                                        }}")),
              onChange = I(paste0("function(val){
                                        const esc = (s) => ($('<div>').text(s ?? '—').html());
                                    const data = this.options[val];
                                    const $info = $(this.$input[0].closest('.form-group').nextElementSibling);
                                    const GiB = 1024*1024*1024;
                                    const scalingUpLabel = esc(", toJSString(lang$nav$dialogSettings$scalingUpLabel), ");
                                    const scalingDownLabel = esc(", toJSString(lang$nav$dialogSettings$scalingDownLabel), ");
                                    const instanceLabel = esc(", toJSString(lang$nav$dialogSettings$instanceLabel), ");
                                    const instancePoolLabel = esc(", toJSString(lang$nav$dialogSettings$instancePoolLabel), ");
                                    const desiredLabel = esc(", toJSString(lang$nav$dialogSettings$desiredWorkersLabel), ");
                                    const activeLabel = esc(", toJSString(lang$nav$dialogSettings$activeWorkersLabel), ");
                                    const busyLabel = esc(", toJSString(lang$nav$dialogSettings$busyWorkersLabel), ");
                                    const memoryLabel = esc(", toJSString(lang$nav$dialogSettings$memoryLabel), ");
                                    const cpuLabel = esc(", toJSString(lang$nav$dialogSettings$cpuLabel), ");
                                    const multiplierLabel = esc(", toJSString(lang$nav$dialogSettings$multiplierLabel), ");
                                    const poolOwnerLabel = esc(", toJSString(lang$nav$dialogSettings$poolOwnerLabel), ");
                                    const statsHTML = `
                                        <span class=\"label label-default\">${cpuLabel}: <strong>${+parseFloat(data.cpu_request).toFixed(1)}</strong></span>
                                        <span class=\"label label-default\">${memoryLabel}: <strong>${+parseFloat(data.memory_request>1024?data.memory_request/1024:data.memory_request).toFixed(1)} ${data.memory_request>1024?'GiB':'MiB'}</strong></span>
                                        <span class=\"label label-default\">${multiplierLabel}: <strong>${+parseFloat(data.multiplier).toFixed(2)}</strong></span>`;
                                    if (data.instance_type === 'pool') {
                                        const desired = +data.size || 0, active = +data.size_active || 0, busy = +data.size_busy || 0;
                                        const scaling = desired !== active ? (desired > active ? scalingUpLabel : scalingDownLabel) + ` (${desiredLabel} ${desired}, ${activeLabel} ${active})`: '';
                                        $info.html(`<div>
                                          <div><strong>${esc(data.label)}</strong> <span class=\"label label-primary\" style=\"margin-left:6px;\">${instancePoolLabel}</span></div>
                                          <div class=\"text-muted\" style=\"margin:4px 0;\">${desiredLabel} <strong>${desired}</strong> · ${activeLabel} <strong>${active}</strong> · ${busyLabel} <strong>${busy}</strong></div>
                                          <div class=\"text-muted\" style=\"margin-bottom:6px\">${scaling}</div>
                                          <div><span class=\"text-muted\">${poolOwnerLabel}:</span> <strong>${esc(data.owner)}</strong></div>
                                          <div style=\"margin-bottom:6px;\"><span class=\"text-muted\">${instanceLabel}:</span> <strong>${esc(data.instance)}</strong></div>
                                          <div>${statsHTML}</div>
                                        </div>`);
                                    } else {
                                        $info.html(`<div><strong>${esc(data.label)}</strong> <span class=\"label label-primary\">${instanceLabel}</span></div>
                                        <div style=\"margin-top:6px;\">${statsHTML}</div>`)
                                    }}"))
            )
          )
          hideEl(session, paste0("#", selectizeId, "Spinner"))
          showEl(session, paste0("#", selectizeId, "Wrapper"))
          private$observers$instances$destroy()
        },
        error = function(e) {
          flog.error("Unexpected error fetching instances. Error message: %s", conditionMessage(e))
          showEl(session, "#settingsDialogUnknownError")
          hideEl(session, paste0("#", selectizeId, "Spinner"))
          private$observers$instances$destroy()
        }
      )
    })
    return(invisible(self))
  },
  getInstanceInfo = function() {
    return(private$instanceInfo)
  },
  updateDefaultInstance = function(newDefault) {
    stopifnot(is.character(newDefault), newDefault %in% vapply(private$instanceInfo[["choices"]], "[[", character(1L),
      "label",
      USE.NAMES = FALSE
    ))
    updateDefaultInstanceReq <- httr::PUT(
      url = paste0(
        private$url, "/usage/instances/", URLencode(private$username, reserved = TRUE), "/default"
      ),
      body = list(default_label = newDefault),
      httr::add_headers(
        Authorization = private$authHeader,
        Timestamp = as.character(Sys.time(), usetz = TRUE)
      ),
      httr::timeout(10L)
    )
    if (!identical(httr::status_code(updateDefaultInstanceReq), 200L)) {
      errMsg <- httr::content(updateDefaultInstanceReq,
        type = "application/json",
        encoding = "utf-8"
      )
      stop(sprintf(
        "Invalid status code: %s. Error message: %s",
        httr::status_code(updateDefaultInstanceReq), errMsg[["message"]]
      ), call. = FALSE)
    }
    private$instanceInfo$selected <- newDefault
  },
  getRemoteAccessGroups = function(userAccessGroups) {
    getGroupsReq <- GET(
      url = paste0(
        private$url,
        "/namespaces/",
        private$namespace, "/user-groups"
      ),
      add_headers(
        Authorization = private$authHeader,
        Timestamp = as.character(Sys.time(), usetz = TRUE)
      ),
      timeout(10L)
    )
    if (!identical(status_code(getGroupsReq), 200L)) {
      errMsg <- content(getGroupsReq,
        type = "application/json",
        encoding = "utf-8"
      )
      stop(sprintf(
        "Invalid status code: %s. Error message: %s",
        status_code(getGroupsReq), errMsg[["message"]]
      ), call. = FALSE)
    }
    groupsTmp <- content(getGroupsReq,
      type = "application/json",
      encoding = "utf-8"
    )

    groupsTmp <- unlist(lapply(groupsTmp, function(accessGroup) {
      if (!identical(accessGroup$label, tolower(accessGroup$label))) {
        flog.warn("Remote access group: %s ignored as it contains uppercase letters. Currently, MIRO does not support group labels that include uppercase letters.", accessGroup$label)
        return(NULL)
      }
      if (!accessGroup$label %in% private$appAccessGroups) {
        flog.debug(
          "Remote access group: %s ignored as it is not part of the app's access groups.",
          accessGroup$label
        )
        return(NULL)
      }
      return(c(
        paste0("#", accessGroup$label),
        vapply(accessGroup$members, "[[", character(1L), "username", USE.NAMES = FALSE)
      ))
    }), use.names = FALSE)
    groupsTmp <- groupsTmp[!groupsTmp %in% c("#admins", "#users")]
    groupsTmp <- groupsTmp[!duplicated(groupsTmp)]
    return(c("#users", if ("#admins" %in% userAccessGroups) "#admins", groupsTmp))
  }
), private = list(
  url = NULL,
  username = NULL,
  namespace = NULL,
  authHeader = NULL,
  apiInfo = NULL,
  instanceInfo = NULL,
  mInstanceResp = NULL,
  mQuotaResp = NULL,
  appAccessGroups = NULL,
  observers = list(quota = NULL, instances = NULL),
  fetchQuotaInfo = function() {
    if (is_mirai(private$mQuotaResp) && unresolved(private$mQuotaResp)) {
      stop_mirai(private$mQuotaResp)
    }
    private$mQuotaResp <- mirai(
      {
        tryCatch(
          {
            quotaInfo <- httr::GET(
              url = paste0(url, "/usage/quota?username=", URLencode(username, reserved = TRUE)),
              httr::add_headers(
                Authorization = authHeader,
                Timestamp = as.character(Sys.time(), usetz = TRUE)
              ),
              httr::timeout(10L)
            )
            if (!identical(httr::status_code(quotaInfo), 200L)) {
              errMsg <- httr::content(quotaInfo,
                type = "application/json",
                encoding = "utf-8"
              )
              stop(sprintf(
                "Invalid status code when fetching quota info: %s. Error message: %s",
                httr::status_code(quotaInfo), errMsg[["message"]]
              ), call. = FALSE)
            }
            quotaInfo <- httr::content(quotaInfo,
              type = "application/json",
              encoding = "utf-8"
            )
            return(list(
              error = FALSE, quotaInfo = quotaInfo
            ))
          },
          error = function(e) {
            return(list(error = TRUE, message = conditionMessage(e)))
          }
        )
      },
      .args = list(url = private$url, username = private$username, authHeader = private$authHeader)
    )
  },
  fetchInstanceInfo = function() {
    if (is_mirai(private$mInstanceResp) && unresolved(private$mInstanceResp)) {
      stop_mirai(private$mInstanceResp)
    }
    private$mInstanceResp <- mirai(
      {
        tryCatch(
          {
            if (length(apiInfoGlobal)) {
              apiInfo <- apiInfoGlobal
            } else {
              apiInfo <- httr::content(httr::GET(url = paste0(url, "/version"), httr::timeout(10L)),
                type = "application/json",
                encoding = "utf-8"
              )
              apiInfo$apiVersionInt <- suppressWarnings(
                as.integer(gsub(".", "", apiInfo[["version"]], fixed = TRUE))
              )[1]
            }
            if (!identical(apiInfo[["in_kubernetes"]], TRUE)) {
              return(list(error = FALSE, instancesSupported = FALSE, apiInfo = apiInfo))
            }
            urls <- c(
              instances = paste0(url, "/usage/instances/", URLencode(username, reserved = TRUE)),
              pools     = paste0(url, "/usage/pools/", URLencode(username, reserved = TRUE)),
              default   = paste0(url, "/usage/instances/", URLencode(username, reserved = TRUE), "/default")
            )

            mkHandle <- function() {
              h <- curl::new_handle()
              curl::handle_setheaders(
                h,
                Authorization = authHeader,
                Timestamp = as.character(Sys.time(), usetz = TRUE)
              )
              curl::handle_setopt(h, timeout = 10L)
              h
            }

            pool <- curl::new_pool()
            out <- vector("list", length(urls))
            names(out) <- names(urls)

            lapply(names(urls), function(nm) {
              curl::curl_fetch_multi(
                urls[[nm]],
                handle = mkHandle(),
                done = function(res) {
                  if (res$status_code != 200L) {
                    msg <- tryCatch(jsonlite::fromJSON(rawToChar(res$content))[["message"]], error = function(e) NA_character_)
                    stop(sprintf(
                      "[%s] Invalid status code: %s. Error message: %s",
                      nm, res$status_code, ifelse(is.na(msg), "<no message>", msg)
                    ), call. = FALSE)
                  }
                  out[[nm]] <<- jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE, simplifyDataFrame = FALSE)
                },
                fail = function(err) {
                  stop(sprintf("[%s] Request failed: %s", nm, err$message), call. = FALSE)
                },
                pool = pool
              )
            })

            curl::multi_run(pool = pool)

            return(list(
              error = FALSE, instancesSupported = TRUE, apiInfo = apiInfo,
              instances = out$instances[["instances_available"]],
              pools = out$pools[["instance_pools_available"]],
              default = out$default[["default_instance"]]
            ))
          },
          error = function(e) {
            return(list(error = TRUE, message = conditionMessage(e)))
          }
        )
      },
      .args = list(
        url = private$url, username = private$username, authHeader = private$authHeader,
        apiInfoGlobal = private$apiInfo
      )
    )
  },
  buildAuthHeader = function(password, useTokenAuth = FALSE) {
    if (useTokenAuth) {
      return(paste0("Bearer ", password))
    }
    return(paste0(
      "Basic ",
      base64_encode(charToRaw(
        paste0(
          private$username,
          ":", password
        )
      ))
    ))
  }
))
