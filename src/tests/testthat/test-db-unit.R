context("Unit tests - database class")

source("../../components/db_schema.R")
source("../../components/db.R")

createTestDb()

ioConfig <<- list()

dbSchema <<- DbSchema$new()
if (identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) {
  dbConfig <- list(
    type = "postgres",
    username = Sys.getenv("MIRO_DB_USERNAME", "postgres"),
    password = Sys.getenv("MIRO_DB_PASSWORD", ""),
    name = Sys.getenv("MIRO_DB_NAME", "postgres"),
    host = Sys.getenv("MIRO_DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("MIRO_DB_PORT", "5432")),
    schema = "mirotests"
  )
} else {
  dbConfig <- list(
    type = "sqlite",
    name = file.path(
      testDir, "testdb",
      "miro.sqlite3"
    )
  )
}
db <- Db$new(
  uid = "te_de\\%d",
  dbConf = dbConfig,
  slocktimeLimit = 400, modelName = "transport",
  ugroups = c("bla_blubb", "test123")
)
conn <- db$getConn()
dbSchema$setConn(conn)

test_that("deleteRows method works", {
  DBI::dbWriteTable(
    conn, "_sys_attach_",
    tibble(
      `_sid` = c(1L, 1L, 1L, 2L),
      fileName = paste0(c("a", "b", "c", "a"), ".txt"),
      b = c("a", "b", "c", "a")
    )
  )
  db$deleteRows("_scenAttach", "fileName",
    c("b.txt", "a.txt"),
    conditionSep = "OR", subsetSids = 2L
  )
  expect_identical(
    DBI::dbGetQuery(conn, paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(conn, "_sys_attach_"))),
    data.frame(
      `_sid` = c(1L, 1L, 1L),
      fileName = paste0(c("a", "b", "c"), ".txt"),
      b = c("a", "b", "c"),
      check.names = FALSE
    )
  )
})

db$finalize()
