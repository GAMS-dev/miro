# gams model name
modelName <- "pickstock"
# miro workspace
miroWorkspaceDir <- ".miro"
# miro data folder prefix
miroDataDirPrefix <- "data_"
# keywords in JSON input data that define that the data is NOT to be imported by an external source
keywordsNoImport <- c("noImport")
# keywords that define data does not HAVE TO BE imported in order to run the model
keywordsNoMustImport <- c("slider", "dropdown", "date", "daterange", "checkbox", "textinput", "numericinput",
                          "noImport")
# list of return value/keyword pairs that define input data type (example: "hot" = "columns")
keywordsType <- list("custom" = "rendererName", "dt" = "dtHeaders", "hot" = "headers", "slider" = "slider", 
                     "dropdown" = "dropdown", "dropdowne" = "dropdowne", "daterange" = "daterange", 
                     "date" = "date", "checkbox" = "checkbox", "textinput" = "textinput", "numericinput" = "numericinput")
listOfOperators <- list("count" = "card", "max" = "max", "min" = "min", 
                        "mean" = "mean", "median" = "median", "var" = "var", "sd" = "sd")
# define identifier names for user id and scenario id 
# (_ sign as first character as it is not a valid identifier name in GAMS but in PostgreSQL)
uidIdentifier <- "_uid"
sidIdentifier <- "_sid"
snameIdentifier <- "_sname"
stimeIdentifier <- "_stime"
slocktimeIdentifier <- "_slocktime"
stagIdentifier <- "_stag"
scodeIdentifier <- "_scode"

# scenario code mapping
SCODEMAP <- c(
  'hcube_jobconfig' = -1L,
  'scen' = 0L
)

# maximum upload size [MB] for data files
maxUploadSize <- 100L

# maximum number of scalars to represent as value box
maxScalarsValBox <- 20L

# define maximum duration a lock is allowed to persist (without being refreshed),
# before it will be deleted (in seconds)
slocktimeLimit <- 3600
# log file directory and name
logFileDir <- paste0("logs", .Platform$file.sep)
# specify the logging level (["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"])
loggingLevel <- Sys.getenv("MIRO_LOG_LEVEL", "TRACE")
# name of table with scenario/hcube metadata as well as scenario locks
scenMetadataTablePrefix <- "_sys_metadata_"
tableNameMetaHcubePrefix <- "_sys_hcubemeta_"
tableNameJobPrefix <- "_sys_jobs_"
scenLockTablePrefix      <- "_sys_scenlocks_"
# maximum number ofscenarios that can be displayed at the same time 
# (used in loop for observeEvent remove/save buttons)
maxNumberScenarios <- 50
# maximum number of scenarios that can be displayed in dropdown menu
maxNoScenToShow <- 2e2
# local user ID (single user)
uid <- Sys.info()[["user"]]
# define the default type for output format
defOutType <- "datatable"
# define the default format for input sheets
defInType <- "pivot"
# default height of pivot table
pivotDefaultHeight <- 800
# name and headers of the csv file that scalars will be saved in
scalarsFileName    <- "_scalars"
scalarsOutName     <- "_scalars_out"
scalarEquationsName <- "_scalarsve"
scalarEquationsOutName <- "_scalarsve_out"
scalarsFileHeaders <- c("scalar", "description", "value")
# name of the directory where GAMS models are saved
modelDir <- "model"
# prefix used for identifying compile time variables
prefixDDPar  <- "_gmspar_"
prefixGMSOpt <- "_gmsopt_"
# language schema name
languageSchemaName <- "language_schema.json"
# get maximum number of lines to read for files
maxSizeToRead <- 5e5
# Limits on external programs that are allowed to be executed 
# (0 -> all allowed, 4 -> no shell calls, echo, put etc.) [0,4]
gamsExecMode <- 0L

MIROGdxInName <- "_miro_gdxin_.gdx"
MIROGdxOutName <- "_miro_gdxout_.gdx"

# ACCESS CONTROL
# db table name where group hierarchies are stored
amTableNameHierarchy <- "_sys_am_hier"
# db table name where user groups are stored
amTableNameGroups    <- "_sys_am_groups"
# db table name where restricted elements as well as the permissions are stored
amTableNameElements  <- "_sys_am_elements"
# column name for access level column
accessIdentifier    <- "_access"
accessElIdentifier <- "_ael"
# prefix for database tables with shared data
sharedTablePrefix <- "_shared"
# default access group every user is in
defaultGroup <- "users"
# list all supported standard renderers
standardRenderers <- c("datatable", "dtgraph", "pivot", "graph", "valuebox")
# name of the folder where hcube jobs will be executed
hcubeDirName <- "hcube_jobs"
# filename of hcube submission file (will be called when hypercube jobs are to be launched)
hcubeSubmissionFile <- "hcube_submission"
# maximum number of scenarios that can be solved per hcube run
maxNoHcube <- 10000L
# maximum number of scenarios to fetch when querying the database
hcubeLoadMaxScen <- 3e7
# maximum number of scenarios that can be loaded to scenario
# comparison mode at the same time
maxConcurentLoad <- 10L
maxSolversPaver  <- 15L
traceColNames <- c("InputFileName","ModelType","SolverName","NLP","MIP","JulianDate","Direction"
                   ,"NumberOfEquations","NumberOfVariables","NumberOfDiscreteVariables",
                   "NumberOfNonZeros","NumberOfNonlinearNonZeros","OptionFile",
                   "ModelStatus","SolverStatus","ObjectiveValue","ObjectiveValueEstimate",
                   "SolverTime","NumberOfIterations","NumberOfDomainViolations","NumberOfNodes","#User1")
exclTraceCols <- c("NLP", "MIP", "NumberOfEquations", "NumberOfVariables", "NumberOfDiscreteVariables",
                   "NumberOfNonZeros", "NumberOfNonlinearNonZeros")
tableNameTracePrefix <- "_sys_trace_"

tableNameAttachPrefix <- "_sys_attach_"
tableNameScriptsPrefix <- "_sys_scripts_"
attachAllowExec       <- TRUE
attachMaxFileSize     <- 1e7
attachMaxNo           <- 5L

TIMEVIS_MAX_EVENTS <- 50L

# delay (in seconds) the server should wait with shutdown after user disconnectedv (equal to ShinyProxy timeout)
SERVER_SHUTDOWN_DELAY <- 600L

JOBSTATUSMAP <- c(
  'running' = -1L,
  'completed' = 0L,
  'downloaded' = 1L,
  'corrupted' = 20L,
  'corrupted(noDir)' = 21L,
  'corrupted(noProcess)' = 22L,
  'corrupted(man)' = 23L,
  'discarded' = 30L,
  'discarded(corrupted)' = 31L,
  'discarded(running)' = 32L,
  'discarded(scheduled)' = 33L,
  'discarded(completed)' = 34L,
  'imported' = 40L,
  'imported(man)' = 41L
)

