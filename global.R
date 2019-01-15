# gams model name
modelName <- "pickstock"
# controls whether webUI parameters are compiled on startup or loaded from save file
developMode <- TRUE
# name of the environment variable used in shiny proxy to identify model name to use
spModelPathEnvVar <- "GMSMODELNAME"
spModelModeEnvVar <- "GMSMODE"
# keywords in JSON input data that define that the data is NOT to be imported by an external source
keywordsNoImport <- c("noImport")
# keywords that define data MUST not be imported in order to run the model
keywordsNoMustImport <- c("slider", "dropdown", "date", "daterange", "checkbox", "noImport")
# list of return value/keyword pairs that define input data type (example: "hot" = "columns")
keywordsType <- list("dt" = "dtHeaders", "hot" = "headers", "slider" = "slider", 
                     "dropdown" = "dropdown", "dropdowne" = "dropdowne", "daterange" = "daterange", 
                     "date" = "date", "checkbox" = "checkbox")
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

# maximum number of scalars to represent as value box
maxScalarsValBox <- 20L

# define maximum duration a lock is allowed to persist (without being refreshed),
# before it will be deleted (in seconds)
slocktimeLimit <- 3600
# log file directory and name
logFileDir <- paste0("logs", .Platform$file.sep)
# whether to print log output in console (in additional to file) or not
logToConsole <- TRUE
# specify the logging level (["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"])
loggingLevel <- "TRACE"
# name of table with scenario/hcube metadata as well as scenario locks
scenMetadataTablePrefix <- "_sys_metadata_"
tableNameMetaHcubePrefix <- "_sys_hcubemeta_"
scenLockTablePrefix      <- "_sys_scenlocks_"
# maximum number ofscenarios that can be displayed at the same time 
# (used in loop for observeEvent remove/save buttons)
maxNumberScenarios <- 50
# local user ID (single user)
uid <- "user"
# define the default type for output format
defOutType <- "datatable"
# define the default format for input sheets
defInType <- "pivot"
# default height of pivot table
pivotDefaultHeight <- 800
# folder with custom renderer functions
customRendererDirName <- "customRenderer"
# name and headers of the csv file that scalars will be saved in
scalarsFileName    <- "scalars"
scalarsOutName     <- "scalars_out"
scalarsFileHeaders <- c("Scalar", "Description", "Value")
# name of the directory where GAMS models are saved
modelDir <- "model/"
# prefix used for identifying compile time variables
prefixDDPar  <- "GMSPAR_"
prefixGMSOpt <- "GMSOPT_"
# language schema name
languageSchemaName <- "language_schema.json"
# get maximum number of lines to read for files
maxNoLinesToRead <- 5e3
# Limits on external programs that are allowed to be executed 
# (0 -> all allowed, 4 -> no shell calls, echo, put etc.) [0,4]
gamsExecMode <- 0L

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
defaultGroup <- "user"
# list all supported standard renderers
standardRenderers <- c("datatable", "dtgraph", "pivot", "graph", "valuebox")
# name of the folder where hcube jobs will be executed
hcubeDirName <- "hcubeJobs"
# filename of hcube submission file (will be called when hypercube jobs are to be launched)
hcubeSubmissionFile <- "hcube_submission"
# maximum number of scenarios that can be solved per hcube run
maxNoHcube <- 10000L
# maximum number of scenarios to fetch when querying the database
hcubeLoadMaxScen <- 1000L
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
attachAllowExec       <- TRUE
attachMaxFileSize     <- 1e7
attachMaxNo           <- 5L


