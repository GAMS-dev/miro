# gams model name
modelName <- "transport"
# miro workspace
miroWorkspaceDir <- ".miro"
# miro data folder prefix
miroDataDirPrefix <- "data_"
# keywords in JSON input data that define that the data is NOT to be imported by an external source
keywordsNoImport <- c("noImport")
# keywords that define data does not HAVE TO BE imported in order to run the model
keywordsNoMustImport <- c(
  "slider", "dropdown", "date", "daterange", "checkbox", "textinput", "numericinput",
  "noImport"
)
# list of return value/keyword pairs that define input data type (example: "hot" = "columns")
keywordsType <- list(
  "custom" = "rendererName", "dt" = "dtHeaders", "hot" = "headers", "slider" = "slider",
  "dropdown" = "dropdown", "dropdowne" = "dropdowne", "daterange" = "daterange",
  "date" = "date", "checkbox" = "checkbox", "textinput" = "textinput", "numericinput" = "numericinput"
)
listOfOperators <- list(
  "count" = "card", "max" = "max", "min" = "min",
  "mean" = "mean", "median" = "median", "var" = "var", "sd" = "sd"
)

# scenario code mapping
SCODEMAP <- c(
  "hcube_inputs" = -2L,
  "hcube_jobconfig" = -1L,
  "scen" = 0L
)

# maximum upload size [MB] for data files
maxUploadSize <- 500L

# maximum number of scalars to represent as value box
maxScalarsValBox <- 20L

# define maximum duration a lock is allowed to persist (without being refreshed),
# before it will be deleted (in seconds)
slocktimeLimit <- 3600
# log file directory and name
logFileDir <- paste0("logs", .Platform$file.sep)
# specify the logging level (["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"])
loggingLevel <- Sys.getenv("MIRO_LOG_LEVEL", "TRACE")
# maximum number ofscenarios that can be displayed at the same time
# (used in loop for observeEvent remove/save buttons)
maxNumberScenarios <- 50
# maximum number of scenarios that can be displayed in dropdown menu
maxNoScenToShow <- 2e2
# local user ID (single user)
uid <- Sys.info()[["user"]]
# define the default format for input sheets
defInType <- "miroPivot"
# default height of pivot table
pivotDefaultHeight <- 800
# name and headers of the csv file that scalars will be saved in
scalarsFileName <- "_scalars"
scalarsOutName <- "_scalars_out"
scalarEquationsName <- "_scalarsve"
scalarEquationsOutName <- "_scalarsve_out"
scalarsFileHeaders <- c("scalar", "description", "value")
# name of the directory where GAMS models are saved
modelDir <- "model"
# prefix used for identifying compile time variables
prefixDDPar <- "_gmspar_"
prefixGMSOpt <- "_gmsopt_"
reservedGMSOpt <- c("idir1", "trace", "traceopt", "curdir")

# strings that indicate the value of a command line parameter should not
# be communicated with GAMS (will be unset)
CLARG_MISSING_VALUES <- c("_", "system.empty", "")

miroLanguage <- Sys.getenv("MIRO_LANG", "en")
miroColorTheme <- Sys.getenv("MIRO_THEME", "default")
# get maximum number of lines to read for files
maxSizeToRead <- 5e5
# Limits on external programs that are allowed to be executed
# (0 -> all allowed, 4 -> no shell calls, echo, put etc.) [0,4]
gamsExecMode <- 0L

MIROGdxInName <- "_miro_gdxin_.gdx"
MIROGdxOutName <- "_miro_gdxout_.gdx"

# default access group
defaultGroup <- "users"
# list all supported standard renderers
standardRenderers <- c("datatable", "dtgraph", "pivot", "graph", "valuebox", "miropivot")
# name of the folder where hcube jobs will be executed
hcubeDirName <- "hcube_jobs"
# maximum number of scenarios that can be solved per hcube run
MAX_NO_HCUBE <- 10000L
# maximum number of scenarios to fetch when querying the database
hcubeLoadMaxScen <- 5e5
# maximum number of scenarios that can be loaded to scenario
# comparison mode at the same time
maxConcurentLoad <- 10L
TRACE_COL_NAMES <- c(
  "InputFileName", "ModelType", "SolverName", "NLP", "MIP", "JulianDate", "Direction",
  "NumberOfEquations", "NumberOfVariables", "NumberOfDiscreteVariables",
  "NumberOfNonZeros", "NumberOfNonlinearNonZeros", "OptionFile",
  "ModelStatus", "SolverStatus", "ObjectiveValue", "ObjectiveValueEstimate",
  "SolverTime", "NumberOfIterations", "NumberOfDomainViolations", "NumberOfNodes", "#User1"
)
exclTraceCols <- c(
  "NLP", "MIP", "NumberOfEquations", "NumberOfVariables", "NumberOfDiscreteVariables",
  "NumberOfNonZeros", "NumberOfNonlinearNonZeros"
)

attachAllowExec <- TRUE
attachMaxFileSize <- 5e7
attachMaxNo <- 7L

JOBSTATUSMAP <- c(
  "queued" = -2L,
  "running" = -1L,
  "completed" = 0L,
  "downloaded" = 1L,
  "corrupted" = 20L,
  "corrupted(noDir)" = 21L,
  "corrupted(noProcess)" = 22L,
  "corrupted(man)" = 23L,
  "discarded" = 30L,
  "discarded(corrupted)" = 31L,
  "discarded(running)" = 32L,
  "discarded(scheduled)" = 33L,
  "discarded(completed)" = 34L,
  "imported" = 40L,
  "imported(man)" = 41L
)

GAMSRCMAP <- c(
  "-500" = "Internal error",
  "-404" = "Host could not be reached",
  "-403" = "Forbidden",
  "-402" = "Quota exceeded",
  "-401" = "Access denied",
  "-400" = "License expired",
  "-100" = "Model execution timed out",
  "-15" = "Model execution was interrupted",
  "-9" = "Model execution was interrupted",
  "1" = "Solver is to be called, the system should never return this number",
  "2" = "There was a compilation error",
  "3" = "There was an execution error",
  "4" = "System limits were reached",
  "5" = "There was a file error",
  "6" = "There was a parameter error",
  "7" = "There was a licensing error",
  "8" = "There was a GAMS system error",
  "9" = "GAMS could not be started",
  "10" = "Out of memory",
  "11" = "Out of disk",
  "15" = "Model execution was interrupted",
  "109" = "Could not create process/scratch directory",
  "110" = "Too many process/scratch directories",
  "112" = "Could not delete the process/scratch directory",
  "113" = "Could not write the script gamsnext",
  "114" = "Could not write the parameter file",
  "115" = "Could not read environment variable",
  "144" = "Could not spawn the GAMS language compiler (gamscmex)",
  "400" = "Could not spawn the GAMS language compiler (gamscmex)",
  "145" = "Current directory (curdir) does not exist",
  "401" = "Current directory (curdir) does not exist",
  "146" = "Cannot set current directory (curdir)",
  "402" = "Cannot set current directory (curdir)",
  "148" = "Blank in system directory (UNIX only)",
  "404" = "Blank in system directory (UNIX only)",
  "149" = "Blank in current directory (UNIX only)",
  "405" = "Blank in current directory (UNIX only)",
  "150" = "Blank in scratch extension (scrext)",
  "406" = "Blank in scratch extension (scrext)",
  "151" = "Unexpected cmexRC",
  "407" = "Unexpected cmexRC",
  "152" = "Could not find the process directory (procdir)",
  "408" = "Could not find the process directory (procdir)",
  "153" = "CMEX library could not be found (experimental)",
  "409" = "CMEX library could not be found (experimental)",
  "154" = "Entry point in CMEX library could not be found (experimental)",
  "410" = "Entry point in CMEX library could not be found (experimental)",
  "155" = "Blank in process directory (UNIX only)",
  "411" = "Blank in process directory (UNIX only)",
  "156" = "Blank in scratch directory (UNIX only)",
  "412" = "Blank in scratch directory (UNIX only)",
  "141" = "Cannot add path / unknown UNIX environment / cannot set environment variable",
  "232" = "Driver error: incorrect command line parameters for gams",
  "1000" = "Driver error: incorrect command line parameters for gams",
  "208" = "Cannot add path / unknown UNIX environment / cannot set environment variable",
  "2000" = "Cannot add path / unknown UNIX environment / cannot set environment variable",
  "184" = "Driver error: problems getting current directory",
  "3000" = "Driver error: problems getting current directory",
  "160" = "Driver error: internal error: GAMS compile and execute module not found",
  "4000" = "Driver error: internal error: GAMS compile and execute module not found",
  "126" = "Driver error: internal error: cannot load option handling library",
  "5000" = "Driver error: internal error: cannot load option handling library"
)
