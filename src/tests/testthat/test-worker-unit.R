context("Unit tests - worker class")

library(R6)
library(mirai)

source("../../components/worker.R")
source("../../components/worker_adapter.R")


test_that("WorkerAdapter enforces abstract methods", {
  # Create a minimal subclass just to bypass the validateMetadata requirement
  DummyAdapter <- R6Class("DummyAdapter",
    inherit = WorkerAdapter,
    private = list(
      validateMetadata = function(metadata) {
        return(metadata)
      }
    )
  )

  adapter <- DummyAdapter$new(metadata = list(), workDir = "/tmp")

  # Ensure abstract methods throw the expected errors
  expect_error(adapter$run(), "must be implemented by subclass")
  expect_error(adapter$interrupt(), "must be implemented by subclass")
  expect_error(adapter$pingProcess(), "must be implemented by subclass")
})

test_that("WorkerAdapter active log binding works correctly", {
  DummyAdapter <- R6Class("DummyAdapter",
    inherit = WorkerAdapter,
    private = list(validateMetadata = function(m) m)
  )
  adapter <- DummyAdapter$new(list(), "/tmp")

  # Add logs
  adapter$log <- "Starting process...\n"
  adapter$log <- "Process running...\n"

  # Retrieve logs (which should clear the buffer)
  current_logs <- adapter$log
  expect_equal(current_logs, "Starting process...\nProcess running...\n")

  # Buffer should now be empty
  expect_equal(adapter$log, "")
})

test_that("LocalWorkerAdapter validates metadata correctly", {
  # Missing required fields should throw an error
  bad_metadata <- list(
    isGamsPy = TRUE
    # Missing modelName, modelGmsName, etc.
  )

  expect_error(
    LocalWorkerAdapter$new(metadata = bad_metadata, workDir = "/tmp"),
    "is.logical\\(metadata\\$saveTraceFile\\) is not TRUE"
  )

  # Valid metadata should initialize successfully
  good_metadata <- list(
    isGamsPy = FALSE,
    saveTraceFile = TRUE,
    modelName = "trnsport",
    modelGmsName = "trnsport.gms",
    executablePath = "/opt/gams/gams",
    clArgs = c("a=1", "b=2")
  )

  adapter <- LocalWorkerAdapter$new(metadata = good_metadata, workDir = "/tmp")
  expect_s3_class(adapter, "LocalWorkerAdapter")
})

test_that("AsyncJobManager initialize blocks synchronous adapters", {
  # Mock a local adapter (supportsAsync = FALSE)
  mock_sync_adapter <- list(supportsAsync = FALSE)

  expect_error(
    AsyncJobManager$new(dbJobSchema = list(), db = list(), adapter = mock_sync_adapter),
    "adapter\\$supportsAsync is not TRUE"
  )
})

test_that("AsyncJobManager getInfoFromJobList handles empty/missing states safely", {
  # 1. Setup Mocks
  MockAdapter <- R6Class("MockAdapter", public = list(supportsAsync = TRUE))
  MockDB <- R6Class("MockDB", public = list(
    getConn = function() NULL,
    getUid = function() "user_123"
  ))

  manager <- AsyncJobManager$new(
    dbJobSchema = list(),
    db = MockDB$new(),
    adapter = MockAdapter$new()
  )

  # 2. Test fetching from an empty job list
  # Because we haven't loaded the jobList, it should gracefully return NULL
  expect_null(manager$getInfoFromJobList(jID = 999))
  expect_null(manager$getPid(jID = 999))
})

test_that("Worker$runAsync extracts job data and maps correctly", {
  # 1. Mock the Adapter to return a specific "mirai" style response
  MockRemoteAdapter <- R6Class("MockRemoteAdapter", public = list(
    supportsAsync = TRUE,
    inputData = list(getSid = function() 42L),
    runAsync = function(solveOptions, name) {
      # Simulate a successful httr/mirai response
      return(list(
        status_code = 201L,
        response = list(
          token = "remote_process_id_99",
          quota_warning = list() # Empty quota warning
        )
      ))
    }
  ))

  # 2. Mock the AsyncJobManager to track if addJob was called
  MockManager <- R6Class("MockManager", public = list(
    added_pid = NULL,
    addJob = function(pid, sid, tags, name, isHcJob) {
      self$added_pid <- pid
      return(101L) # Return fake Database Job ID
    }
  ))

  # 3. Initialize Worker with mocks
  adapter <- MockRemoteAdapter$new()
  worker <- Worker$new(adapter = adapter, db = list(getConn = function() list(), getUid = function() "freddy"), dbJobSchema = list())

  # Inject the mock manager to bypass DB logic
  mock_manager <- MockManager$new()
  worker$asyncJobManager <- mock_manager

  # 4. Execute target method
  result <- worker$runAsync(name = "Test Job")

  # 5. Assertions
  expect_equal(result$pid, "remote_process_id_99")
  expect_equal(result$jid, 101L)
  expect_null(result$quotaWarning)

  # Verify the manager received the correct PID from the adapter
  expect_equal(mock_manager$added_pid, "remote_process_id_99")
})

test_that("AsyncJobManager$addJob returns -1L on database error", {
  # 1. Create a Mock DB that intentionally fails
  MockFailingDB <- R6Class("MockFailingDB", public = list(
    getConn = function() "fake_connection",
    getUid = function() "user_123",
    createJobMeta = function() {
      stop("Database connection lost!")
    }
  ))

  # 2. Mock Adapter
  MockAdapter <- R6Class("MockAdapter", public = list(supportsAsync = TRUE))

  manager <- AsyncJobManager$new(
    dbJobSchema = list(colNames = letters[1:10], tabName = "test_tab"),
    db = MockFailingDB$new(),
    adapter = MockAdapter$new()
  )

  # 3. Suppress the logger output so it doesn't clutter the test console,
  # and verify that the manager catches the error and returns -1L.
  suppressMessages({
    result <- manager$addJob(pid = "proc_123", sid = 1L)
  })

  expect_equal(result, -1L)
})

test_that("RemoteWorkerAdapter$pingProcess handles API submission errors", {
  # 1. Create the adapter
  adapter <- RemoteWorkerAdapter$new(
    metadata = list(
      isGamsPy = FALSE, saveTraceFile = FALSE, modelName = "test",
      modelId = "id1", modelGmsName = "test.gms", modelNameRaw = "test",
      clArgs = c("a=1"), modelDataFiles = c("data.gdx"), textEntries = c("entry1"),
      useRegistered = TRUE
    ),
    workDir = "/tmp",
    engineConfig = list(url = "http://fake-engine.com", namespace = "default", authHeader = "Bearer token")
  )

  # 2. Simulate a resolved `mirai` promise that returned an API error (e.g., 401 Unauthorized)
  mock_failed_promise <- list(
    data = list(
      status_code = 401L,
      response = list(message = "Unauthorized access", exceeded_quotas = list())
    )
  )
  class(mock_failed_promise) <- "mirai"

  # Inject the fake resolved promise (bypassing R6 encapsulation just for testing)
  adapter$.__enclos_env__$private$mSubRes <- mock_failed_promise

  # 3. Call pingProcess and check that it mapped the 401 status code to a negative integer
  status <- adapter$pingProcess()

  expect_equal(status, -401L)
  expect_equal(adapter$processStatus, -401L)
})

test_that("RemoteWorkerAdapter$pingProcess handles successful queuing", {
  adapter <- RemoteWorkerAdapter$new(
    metadata = list(
      isGamsPy = FALSE, saveTraceFile = FALSE, modelName = "test",
      modelId = "id1", modelGmsName = "test.gms", modelNameRaw = "test",
      clArgs = c("a=1"), modelDataFiles = c("data.gdx"), textEntries = c("entry1"),
      useRegistered = TRUE
    ),
    workDir = "/tmp",
    engineConfig = list()
  )

  # Simulate a successful 201 Created response from the engine
  mock_success_promise <- list(
    data = list(
      status_code = 201L,
      response = list(token = "tok_abc123", queue_position = 5L, quota_warning = list())
    )
  )
  class(mock_success_promise) <- "mirai"

  adapter$.__enclos_env__$private$mSubRes <- mock_success_promise

  status <- adapter$pingProcess()

  # It should extract the queue position and format it as "q5"
  expect_equal(status, "q5")
  expect_equal(adapter$processId, "tok_abc123")
})

test_that("Worker$run operates synchronously if adapter does not support async", {
  # Mock a local adapter
  MockSyncAdapter <- R6Class("MockSyncAdapter", public = list(
    supportsAsync = FALSE,
    inputData = NULL,
    run_called = FALSE,
    run = function(solveOptions = NULL, name = NULL) {
      self$run_called <- TRUE
      return("local_pid_456")
    }
  ))

  adapter <- MockSyncAdapter$new()
  worker <- Worker$new(adapter = adapter, db = list(), dbJobSchema = list())

  # Because supportsAsync is FALSE, asyncJobManager should not be initialized
  expect_null(worker$asyncJobManager)

  # Run the job synchronously
  worker$run(name = "Sync Job")

  # Verify the adapter's run method was triggered
  expect_true(adapter$run_called)

  # Verify async methods explicitly block execution
  expect_error(
    worker$runAsync(name = "Async Job"),
    "private\\$adapter\\$supportsAsync is not TRUE"
  )
})
