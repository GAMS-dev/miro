# gams model name
modelName <- "pickstock"
# show extensive error messages/ obscoure error messages
debug.mode <- TRUE
# name of the environment variable used in shiny proxy to identify model name to use
sp.modelName.env.var <- "SHINYPROXY_MODELNAME"
# keywords in JSON input data that define that the data is NOT to be imported by an external source
keywords.no.import <- c("noimport")
# keywords that define data MUST not be imported in order to run the model
keywords.no.must.import <- c("slider", "dropdown", "date", "daterange", "checkbox", "noimport")
# list of return value/keyword pairs that define input data type (example: "hot" = "columns")
keywords.type <- list("hot" = "headers", "slider" = "slider", "dropdown" = "dropdown", "dropdowne" = "dropdowne", "daterange" = "daterange", "date" = "date", "checkbox" = "checkbox")
list.of.operators <- list("count" = "card", "max" = "max", "min" = "min", "mean" = "mean", "median" = "median", "var" = "var", "sd" = "sd")
# define identifier names for user id and scenario id 
# (_ sign as first character as it is not a valid identifier name in GAMS but in PostgreSQL)
uid.identifier <- "_uid"
sid.identifier <- "_sid"
sname.identifier <- "_sname"
stime.identifier <- "_stime"
slocktime.identifier <- "_slocktime"
stagIdentifier <- "_stag"
# define maximum duration a lock is allowed to persist (without being refreshed), before it will be deleted (in seconds)
slocktimeLimit <- 3600
# log file directory and name
logFileDir <- paste0("logs", .Platform$file.sep)
tmpFileDir <- paste0("tmp", .Platform$file.sep)
# specify the logging level (["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"])
loggingLevel <- "INFO"
# name of table with scenario metadata as well as scenario locks
scen.metadata.table.prefix <- "_sys_metadata_"
scenLock.table.prefix      <- "_sys_scenlocks_"
# maximum number ofscenarios that can be displayed at the same time (used in loop for observeEvent remove/save buttons)
maxNumberScenarios <- 50
# local user ID (single user)
uid <- "Freddy"
# define the default type for output format
def.out.type <- "pivot"
# define the default format for input sheets
def.in.type <- "pivot"
# default height of pivot table
pivot.default.height <- 800
# folder with custom renderer functions
customRendererDir <- "./customRenderer/"
# name and headers of the csv file that scalars will be saved in
scalars.file.name    <- "scalars"
scalars.out.name     <- "scalars_out"
scalars.file.headers <- c("Scalar", "Description", "Value")
# supported operating systems for "Stop"-Button for gams.exe
osSupportInterrupt <- c("windows", "linux", "osx")
# name of the directory where GAMS models are saved
modelDir <- "./model/"
# name of the directory that input data used for asynchronous solve will be saved
asyncDir.name <- "./async/"
# file name of file used to communicate asynchronous solves with GAMS
async.exec.file <- "run_async.txt"
# prefix used for identifying compile time variables
prefixDDPar  <- "GMSPAR_"
prefixGMSOpt <- "GMSOPT_"
# language schema name
language.schema.name <- "language_schema.json"
# ACCESS CONTROL
# db table name where group hierarchies are stored
am.tableName.hierarchy <- "access_test2"
# db table name where user groups are stored
am.tableName.groups    <- "_sys_am_groups"
# db table name where restricted elements as well as the permissions are stored
am.tableName.elements  <- "_sys_am_elements"
# column name for access level column
access.identifier    <- "_access"
access.el.identifier <- "_ael"
# prefix for database tables with shared data
shared.table.prefix <- "_shared"
# default access group every user is in
defaultGroup <- "user"
# list all supported standard renderers
standardRenderers <- c("datatable", "dtgraph", "pivot", "graph")


