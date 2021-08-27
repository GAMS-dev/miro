context("Unit tests - language files")

langsToTest <- c("en", "de", "cn")
miroRootDir <- file.path(getwd(), "..", "..")
source(file.path(miroRootDir, "components", "json.R"))

jsonValidator <- JSONValidator$new(miroRootDir)

for (langToTest in langsToTest) {
  test_that(sprintf("Language file: %s is valid", langToTest), {
    expect_true(file.exists(file.path(miroRootDir, "conf", paste0(langToTest, ".json"))))
    expect_error(
      {
        valid <- jsonValidator$validate(
          file.path(miroRootDir, "conf", paste0(langToTest, ".json")),
          file.path(miroRootDir, "conf", "language_schema.json")
        )
        if (!is.null(valid$errors)) {
          stop(valid$errors, call. = FALSE)
        }
      },
      NA
    )
  })
}
