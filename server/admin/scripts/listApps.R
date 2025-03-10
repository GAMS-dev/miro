library(jsonlite)

dontcare <- lapply(c("global.R", list.files("./R", full.names = TRUE)), source)

tryCatch(
  {
    modelConfig <- ModelConfig$new(SPECS_YAML_PATH)
  },
  error = function(e) {
    write(sprintf(
      "merr:::500:::Problems initializing required components. Error message: %s",
      conditionMessage(e)
    ), stderr())
    quit("no", 1L, FALSE)
  }
)

write("merr:::200:::", stderr())
write(jsonlite::toJSON(modelConfig$getConfigList(), dataframe = "rows"), stderr())
