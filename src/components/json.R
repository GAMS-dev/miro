JSONValidator <- R6Class(
  "Worker",
  public = list(
    initialize = function(miroRootDir = ".") {
      private$ct <- V8::v8(global = "window")
      private$ct$source(file.path(miroRootDir, "JS", "ajv.min.js"))
      private$ct$source(file.path(miroRootDir, "JS", "better-ajv-errors.js"))
      private$ct$eval("const ajv=new ajv7.Ajv({useDefaults:true,validateSchema:false,strict:false});")
    },
    validate = function(jsonFileLocation, jsonSchemaLocation, returnRawData = FALSE) {
      tryCatch(
        {
          private$ct$assign("schema", readr::read_file(jsonSchemaLocation))
          private$ct$eval("var validate=ajv.compile(JSON.parse(schema));")
        },
        error = function(e) {
          stop(
            paste0(
              "Error reading'", jsonSchemaLocation,
              "'. Check for valid JSON syntax and make sure file is accessible.\nError message: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
      tryCatch(
        {
          dataTmp <- readr::read_file(jsonFileLocation)
          private$ct$assign("dataRaw", dataTmp)
          private$ct$eval("data=JSON.parse(dataRaw);")
        },
        error = function(e) {
          stop(
            paste0(
              "Error reading'", jsonFileLocation,
              "'. Check for valid JSON syntax and make sure file is accessible.\nError message: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
      tryCatch(
        {
          private$ct$eval("var valid=validate(data);")
        },
        error = function(e) {
          stop(
            paste0(
              "Problems validating JSON file: '", jsonFileLocation,
              "'.\nError message: '", conditionMessage(e), "'."
            ),
            call. = FALSE
          )
        }
      )
      valid <- private$ct$get("valid")
      if (identical(valid, TRUE)) {
        if (returnRawData) {
          data <- jsonlite::fromJSON(dataTmp,
            simplifyDataFrame = FALSE,
            simplifyMatrix = FALSE
          )
        } else {
          data <- private$ct$get("data",
            simplifyDataFrame = FALSE,
            simplifyMatrix = FALSE
          )
        }
      } else {
        stop(private$ct$get("window.betterAjvErrors(schema,data,validate.errors,{json:dataRaw})"), call. = FALSE)
      }
      return(data)
    }
  ),
  private = list(
    ct = NULL
  )
)
