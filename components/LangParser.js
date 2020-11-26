'use strict'

const Ajv = require('ajv');
const ajv = new Ajv();
const schema = {  
   "$schema":"http://json-schema.org/draft-07/schema#",
   "title":"GAMS MIRO Launcher language file schema",
   "type":"object",
   "additionalProperties":false,
   "properties":{
     "update":{
       "type":"object",
       "additionalProperties":false,
       "properties":{
           "title": {
               "type":"string",
               "minLength":1
           },
           "updateAvailable": {
               "type":"string",
               "minLength":1
           },
           "downloadUpdate": {
               "type":"string",
               "minLength":1
           },
           "here": {
               "type":"string",
               "minLength":1
           },
           "upToDate": {
               "type":"string",
               "minLength":1
           },
           "error": {
               "type":"string",
               "minLength":1
           },
           "btClose": {
               "type":"string",
               "minLength":1
           }
         },
         "required": ["title", "updateAvailable", "downloadUpdate", "here", "upToDate", "error",
         "btClose"]
     },
     "main":{
       "type":"object",
       "additionalProperties":false,
       "properties":{
           "ErrorUnexpectedHdr": {
               "type":"string",
               "minLength":1
           }, 
           "ErrorUnexpectedMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorUnexpectedMsg2": {
               "type":"string",
               "minLength":1
           },
           "BtnCancel": {
               "type":"string",
               "minLength":1
           },
           "BtnOk": {
               "type":"string",
               "minLength":1
           },
           "BtnRemove": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidTwoMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidThreeMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorAPIHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorAPIMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorVersionMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorReadMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorLogoHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorLogoMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorLogoMultiMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorLogoLargeHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorLogoLargeMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorExampleExistsHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorModelExistsHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorModelExistsMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorModelExistsMsg2": {
               "type":"string",
               "minLength":1
           },
           "ErrorWriteHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorWriteMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorWriteMsg2": {
               "type":"string",
               "minLength":1
           },
           "ErrorAppRunningHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorAppRunningMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorAppIncompMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorMsgLaunch": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallStartMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallPermHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallPerm1Msg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallPerm2Msg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallPerm3Msg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallPerm4Msg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallPermBtnYes": {
               "type":"string",
               "minLength":1
           },
           "ErrorInstallPermBtnNo": {
               "type":"string",
               "minLength":1
           },
           "ErrorWritePerm2Msg": {
               "type":"string",
               "minLength":1
           },
           "ErrorNoWritePermMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidPathHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidPathMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidPathMsg2": {
               "type":"string",
               "minLength":1
           },
           "ErrorInvalidPathMsgMac": {
               "type":"string",
               "minLength":1
           },
           "ErrorMessage": {
               "type":"string",
               "minLength":1
           },
           "SuccessUpdateHdr": {
               "type":"string",
               "minLength":1
           },
           "SuccessUpdateMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorUnexpectedWriteMsg": {
               "type":"string",
               "minLength":1
           },
           "DeleteMsg": {
               "type":"string",
               "minLength":1
           },
           "DeleteDataMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInit": {
               "type":"string",
               "minLength":1
           },
           "ErrorRInstallHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorRInstallMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorModelPathHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorModelPathMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorRNotFoundHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorRNotFoundMsg": {
               "type":"string",
               "minLength":1
           },
           "ErrorInconsistentDbTablesHdr": {
               "type":"string",
               "minLength":1
           },
           "ErrorInconsistentDbTablesMsg": {
               "type":"string",
               "minLength":1
           },
         },
         "required": ["ErrorUnexpectedHdr", "ErrorUnexpectedMsg", "ErrorUnexpectedMsg2", "BtnCancel", "BtnOk", "BtnRemove", "ErrorInvalidHdr",
         "ErrorInvalidMsg", "ErrorInvalidTwoMsg", "ErrorInvalidThreeMsg", "ErrorAPIHdr", "ErrorAPIMsg", "ErrorVersionMsg", "ErrorReadMsg",
         "ErrorLogoHdr", "ErrorLogoMsg", "ErrorLogoMultiMsg", "ErrorLogoLargeHdr", "ErrorLogoLargeMsg", "ErrorExampleExistsHdr", "ErrorModelExistsHdr",
         "ErrorModelExistsMsg", "ErrorModelExistsMsg2", "ErrorWriteHdr", "ErrorWriteMsg", "ErrorWriteMsg2", "ErrorAppRunningHdr", "ErrorAppRunningMsg",
         "ErrorAppIncompMsg", "ErrorMsgLaunch", "ErrorInstallStartMsg", "ErrorInstallPermHdr", "ErrorInstallPerm1Msg", "ErrorInstallPerm2Msg",
         "ErrorInstallPerm3Msg", "ErrorInstallPerm4Msg", "ErrorInstallPermBtnYes", "ErrorInstallPermBtnNo", "ErrorWritePerm2Msg", "ErrorNoWritePermMsg",
         "ErrorInvalidPathHdr", "ErrorInvalidPathMsg", "ErrorInvalidPathMsgMac", "ErrorInvalidPathMsg2", "ErrorMessage", "SuccessUpdateHdr",
         "SuccessUpdateMsg", "ErrorUnexpectedWriteMsg", "DeleteMsg", "DeleteDataMsg", "ErrorInit", "ErrorRInstallHdr", "ErrorRInstallMsg",
         "ErrorModelPathHdr", "ErrorModelPathMsg", "ErrorRNotFoundHdr", "ErrorRNotFoundMsg", "ErrorInconsistentDbTablesHdr", "ErrorInconsistentDbTablesMsg"]
     },
     "general":{
       "type":"object",
       "additionalProperties":false,
       "properties":{
           "title": {
               "type":"string",
               "minLength":1
           },
           "noApps": {
               "type":"string",
               "minLength":1
           },
           "btEdit": {
               "type":"string",
               "minLength":1
           },
           "btEditDone": {
               "type":"string",
               "minLength":1
           },
           "btAddExamples": {
               "type":"string",
               "minLength":1
           },
           "appFilesPlaceholder": {
               "type":"string",
               "minLength":1
           },
           "appNamePlaceholder": {
               "type":"string",
               "minLength":1
           },
           "appDescPlaceholder": {
               "type":"string",
               "minLength":1
           },
           "appDbPathPlaceholder": {
               "type":"string",
               "minLength":1
           },
           "appLogoPlaceholder": {
               "type":"string",
               "minLength":1
           },
           "appDbPathReset": {
               "type":"string",
               "minLength":1
           },
           "editAppInfoText": {
               "type":"string",
               "minLength":1
           },
           "btLaunch": {
               "type":"string",
               "minLength":1
           },
           "btLaunchBase": {
               "type":"string",
               "minLength":1
           },
           "btLaunchHcube": {
               "type":"string",
               "minLength":1
           },
           "btCancel": {
               "type":"string",
               "minLength":1
           },
           "btSave": {
               "type":"string",
               "minLength":1
           },
           "btAddApp": {
               "type":"string",
               "minLength":1
           },
           "errNoAppTitleHdr": {
               "type":"string",
               "minLength":1
           },
           "errNoAppTitleMsg": {
               "type":"string",
               "minLength":1
           },
           "errInvalidDbPathHdr": {
               "type":"string",
               "minLength":1
           },
           "errInvalidDbPathMsg": {
               "type":"string",
               "minLength":1
           },
           "dialogSelectAppLogoHdr": {
               "type":"string",
               "minLength":1
           },
           "dialogSelectAppLogoMsg": {
               "type":"string",
               "minLength":1
           },
           "dialogSelectAppLogoBtn": {
               "type":"string",
               "minLength":1
           },
           "dialogSelectAppLogoFilter": {
               "type":"string",
               "minLength":1
           },
           "dialogSelectDbPathHdr": {
               "type":"string",
               "minLength":1
           },
           "dialogSelectDbPathMsg": {
               "type":"string",
               "minLength":1
           },
           "dialogSelectDbPathBtn": {
               "type":"string",
               "minLength":1
           },
           "dialogErrHdr": {
               "type":"string",
               "minLength":1
           },
           "dialogErrMsg": {
               "type":"string",
               "minLength":1
           },
           "dialogNewAppFilesHdr": {
               "type":"string",
               "minLength":1
           },
           "dialogNewAppFilesMsg": {
               "type":"string",
               "minLength":1
           },
           "dialogNewAppFilesBtn": {
               "type":"string",
               "minLength":1
           },
           "dialogNewAppFilesFilter": {
               "type":"string",
               "minLength":1
           },
       },
       "required": ["title", "noApps", "btEdit", "btEditDone", "btAddExamples", "appFilesPlaceholder", "appNamePlaceholder", 
       "appDescPlaceholder", "appDbPathPlaceholder", "appLogoPlaceholder", "appDbPathReset", "editAppInfoText", "btLaunch", "btLaunchBase", 
       "btLaunchHcube", "btCancel", "btSave", "btAddApp", "errNoAppTitleHdr", "errNoAppTitleMsg" ,"errInvalidDbPathHdr",
       "errInvalidDbPathMsg", "dialogSelectAppLogoHdr", "dialogSelectAppLogoMsg", "dialogSelectAppLogoBtn",
       "dialogSelectAppLogoFilter", "dialogSelectDbPathHdr", "dialogSelectDbPathMsg", "dialogSelectDbPathBtn",
       "dialogErrHdr", "dialogErrMsg", "dialogNewAppFilesHdr", "dialogNewAppFilesMsg", "dialogNewAppFilesBtn",
       "dialogNewAppFilesFilter"]
     },
     "menu":{
       "type":"object",
       "additionalProperties":false,
       "properties":{
           "pref": {
               "type":"string",
               "minLength":1
           },
           "about": {
               "type":"string",
               "minLength":1
           },
           "services": {
               "type":"string",
               "minLength":1
           },
           "hide": {
               "type":"string",
               "minLength":1
           },
           "unhide": {
               "type":"string",
               "minLength":1
           },
           "hideothers": {
               "type":"string",
               "minLength":1
           },
           "file": {
               "type":"string",
               "minLength":1
           },
           "edit": {
               "type":"string",
               "minLength":1
           },
           "addApp": {
               "type":"string",
               "minLength":1
           },
           "editApp": {
               "type":"string",
               "minLength":1
           },
           "addExampleApps": {
               "type":"string",
               "minLength":1
           },
           "undo": {
               "type":"string",
               "minLength":1
           },
           "redo": {
               "type":"string",
               "minLength":1
           },
           "cut": {
               "type":"string",
               "minLength":1
           },
           "copy": {
               "type":"string",
               "minLength":1
           },
           "paste": {
               "type":"string",
               "minLength":1
           },
           "selectAll": {
               "type":"string",
               "minLength":1
           },
           "view": {
               "type":"string",
               "minLength":1
           },
           "window": {
               "type":"string",
               "minLength":1
           },
           "minimize": {
               "type":"string",
               "minLength":1
           },
           "zoom": {
               "type":"string",
               "minLength":1
           },
           "front": {
               "type":"string",
               "minLength":1
           },
           "close": {
               "type":"string",
               "minLength":1
           },
           "quit": {
               "type":"string",
               "minLength":1
           },
           "fullscreen": {
               "type":"string",
               "minLength":1
           },
           "resetZoom": {
               "type":"string",
               "minLength":1
           },
           "zoomIn": {
               "type":"string",
               "minLength":1
           },
           "zoomOut": {
               "type":"string",
               "minLength":1
           },
           "help": {
               "type":"string",
               "minLength":1
           },
           "doc": {
               "type":"string",
               "minLength":1
           },
           "update": {
               "type":"string",
               "minLength":1
           }
         },
         "required": ["pref", "file", "edit", "addApp", "editApp", "addExampleApps", "undo",
         "redo", "cut", "copy", "paste", "selectAll", "view", "window", "minimize", "zoom", "front", "close", "quit", "fullscreen", "resetZoom", "zoomIn", "zoomOut", "help", "doc", "update"]
      },
      "settings":{
        "type":"object",
        "additionalProperties":false,
        "properties":{
           "title": {
               "type":"string",
               "minLength":1
           },
           "general-tab": {
               "type":"string",
               "minLength":1
           }, 
           "paths-tab": {
               "type":"string",
               "minLength":1
           }, 
           "launchBrowser": {
               "type":"string",
               "minLength":1
           }, 
           "browserReset": {
               "type":"string",
               "minLength":1
           }, 
           "generalLanguage": {
               "type":"string",
               "minLength":1
           }, 
           "languageReset": {
               "type":"string",
               "minLength":1
           },
           "generalRemoteExec": {
               "type":"string",
               "minLength":1
           },
           "remoteExecReset": {
               "type":"string",
               "minLength":1
           },
           "generalLogging": {
               "type":"string",
               "minLength":1
           }, 
           "loggingReset": {
               "type":"string",
               "minLength":1
           }, 
           "generalLoglife": {
               "type":"string",
               "minLength":1
           }, 
           "loglifeReset": {
               "type":"string",
               "minLength":1
           }, 
           "pathMiroapp": {
               "type":"string",
               "minLength":1
           }, 
           "pathMiroappSelect": {
               "type":"string",
               "minLength":1
           }, 
           "resetPathMiroapp": {
               "type":"string",
               "minLength":1
           }, 
           "pathGams": {
               "type":"string",
               "minLength":1
           }, 
           "pathGamsSelect": {
               "type":"string",
               "minLength":1
           }, 
           "pathGamsReset": {
               "type":"string",
               "minLength":1
           }, 
           "pathLog": {
               "type":"string",
               "minLength":1
           }, 
           "pathLogSelect": {
               "type":"string",
               "minLength":1
           }, 
           "pathLogReset": {
               "type":"string",
               "minLength":1
           }, 
           "pathR": {
               "type":"string",
               "minLength":1
           }, 
           "pathRSelect": {
               "type":"string",
               "minLength":1
           }, 
           "pathRReset": {
               "type":"string",
               "minLength":1
           }, 
           "needHelp": {
               "type":"string",
               "minLength":1
           }, 
           "btSave": {
               "type":"string",
               "minLength":1
           }, 
           "btCancel": {
               "type":"string",
               "minLength":1
           }, 
           "dialogConfigPathHdr": {
               "type":"string",
               "minLength":1
           }, 
           "dialogConfigPathMsg": {
               "type":"string",
               "minLength":1
           }, 
           "dialogConfigPathBtn": {
               "type":"string",
               "minLength":1
           }, 
           "dialogConfigPathLabel": {
               "type":"string",
               "minLength":1
           }, 
           "dialogGamsPathHdr": {
               "type":"string",
               "minLength":1
           }, 
           "dialogGamsPathMsg": {
               "type":"string",
               "minLength":1
           }, 
           "dialogGamsPathLabel": {
               "type":"string",
               "minLength":1
           }, 
           "dialogGamsPathBtn": {
               "type":"string",
               "minLength":1
           }, 
           "dialogRPathHdr": {
               "type":"string",
               "minLength":1
           }, 
           "dialogRPathMsg": {
               "type":"string",
               "minLength":1
           }, 
           "dialogRPathLabel": {
               "type":"string",
               "minLength":1
           }, 
           "dialogRPathBtn": {
               "type":"string",
               "minLength":1
           }, 
           "dialogLogPathHdr": {
               "type":"string",
               "minLength":1
           }, 
           "dialogLogPathMsg": {
               "type":"string",
               "minLength":1
           }, 
           "dialogLogPathLabel": {
               "type":"string",
               "minLength":1
           }, 
           "dialogLogPathBtn": {
               "type":"string",
               "minLength":1
           }, 
           "dialogLogLifeErrHdr": {
               "type":"string",
               "minLength":1
           }, 
           "dialogLogLifeErrMsg": {
               "type":"string",
               "minLength":1
           }, 
           "dialogLogLifeErrBtn": {
               "type":"string",
               "minLength":1
           }, 
           "browseFiles": {
               "type":"string",
               "minLength":1
           }
         },
         "required": ["title", "general-tab", "paths-tab", "launchBrowser", "browserReset", "generalLanguage", "languageReset", "generalRemoteExec", "remoteExecReset", "generalLogging", "loggingReset", "generalLoglife", "loglifeReset", "pathMiroapp", "pathMiroappSelect", "resetPathMiroapp", "pathGams", "pathGamsSelect", "pathGamsReset", "pathLog", "pathLogSelect", "pathLogReset", "pathR", "pathRSelect", "pathRReset", "needHelp", "btSave", "btCancel", "dialogConfigPathHdr", "dialogConfigPathMsg", "dialogConfigPathBtn", "dialogConfigPathLabel", "dialogGamsPathHdr", "dialogGamsPathMsg", "dialogGamsPathLabel", "dialogGamsPathBtn", "dialogRPathHdr", "dialogRPathMsg", "dialogRPathLabel", "dialogRPathBtn", "dialogLogPathHdr", "dialogLogPathMsg", "dialogLogPathLabel", "dialogLogPathBtn", "dialogLogLifeErrHdr", "dialogLogLifeErrMsg", "dialogLogLifeErrBtn", "browseFiles"]
      }
   },
   "required": ["update", "main", "general", "menu", "settings"]
}

const en = {
    "update": {
        "title": "Check for Update",
        "updateAvailable": "A new version of GAMS MIRO is available:",
        "downloadUpdate": "You can download this version",
        "here": "here",
        "upToDate": "You are using the latest version of GAMS MIRO.",
        "error": "An unexpected error has occurred. Please try again later.",
        "btClose": "Ok"
    },
    "main": {
        "ErrorUnexpectedHdr": "Unexpected error",
        "ErrorUnexpectedMsg": "The MIRO app could not be started. Please report to GAMS when this problem persists!",
        "ErrorUnexpectedMsg2": "An unexpected error occurred. Error message:",
        "ErrorMsgLaunch": "Problems launching MIRO app. Error message:",
        "ErrorInvalidHdr": "Invalid MIRO app file",
        "ErrorInvalidMsg": "The file you selected is not a valid MIRO app!",
        "ErrorInvalidTwoMsg": "Please drop only a single MIRO app file!",
        "ErrorInvalidThreeMsg": "Invalid app",
        "ErrorAPIHdr": "MIRO app incompatible",
        "ErrorAPIMsg": "The MIRO app you want to add is not compatible with the MIRO version you installed. Please ask the developer of the app to update it and try again!",
        "ErrorVersionMsg": "The MIRO app you want to add was created with a newer MIRO version than the one you use. Please update your MIRO version and try again!",
        "ErrorAppIncompMsg": "The MIRO app you want to launch is not compatible with the MIRO version you installed. Please ask the developer of the app to update it and try again!",
        "ErrorReadMsg": "There was a problem reading the MIRO app file. Error message:",
        "ErrorLogoHdr": "Invalid MIRO app logo",
        "ErrorLogoMsg": "The file you selected is not a valid MIRO logo. Only jpg/jpeg and png supported!",
        "ErrorLogoMultiMsg": "Please drop only a single MIRO app logo!",
        "ErrorLogoLargeHdr": "Logo too large",
        "ErrorLogoLargeMsg": "Logos must not be larger than 10MB!",
        "ErrorExampleExistsHdr": "Example models already exist",
        "ErrorModelExistsHdr": "Model already exists",
        "ErrorModelExistsMsg": "The following example models could not be added because they are already part of your library: ",
        "ErrorModelExistsMsg2": "A model with the same name already exists. Please first delete this model before trying again.",
        "ErrorWriteHdr": "No write permissions",
        "ErrorWriteMsg": "Model could not be added as you don't have permissions to write to this location:",
        "ErrorWriteMsg2": "Model could not be removed as you don't have permissions to write to this location:",
        "ErrorWritePerm2Msg": "Model could not be saved as you don't have permissions to write to this location:",
        "ErrorNoWritePermMsg": "Model could not be updated as you don't have permissions to write to this location:",
        "ErrorAppRunningHdr": "App running",
        "ErrorAppRunningMsg": "A MIRO process is already running for your app. Currently, only one instance per app can be launched at a time.",
        "ErrorInstallStartMsg": "The MIRO installation could not be started. Please check the log files and report to GAMS when this problem persists!",
        "ErrorInstallPermHdr": "Installation",
        "ErrorInstallPerm1Msg": "You don't have permissions to install libraries inside here:",
        "ErrorInstallPerm2Msg": ". \nWould you like to install MIRO locally instead",
        "ErrorInstallPerm3Msg": "?\n In case you want to install MIRO globally, consider starting AppImage with sudo and --no-sandbox flag: sudo ./GAMS-MIRO-",
        "ErrorInstallPerm4Msg": ".AppImage --no-sandbox",
        "ErrorInstallPermBtnYes": "Yes, local installation",
        "ErrorInstallPermBtnNo": "No, quit",
        "ErrorInvalidPathHdr": "path invalid",
        "ErrorInvalidPathMsg": ": The path you selected is not a valid path. Note that in order to run MIRO at least the following version is required: ",
        "ErrorInvalidPathMsgMac": ": The path you selected is not a valid path. Note that in order to run MIRO the following version is required: ",
        "ErrorInvalidPathMsg2": "An unexpected error occurred while validating the path you selected:",
        "ErrorMessage": "Error message:",
        "SuccessUpdateHdr": "Configuration updated",
        "SuccessUpdateMsg": "Your configuration was successfully updated. MIRO must be restarted for your changes to take effect. Do you want to restart MIRO now?",
        "ErrorUnexpectedWriteMsg": "Configuration data could not be saved. Do you miss write permissions in this location:",
        "DeleteMsg": "Are you sure you want to permanently remove the app?",
        "DeleteDataMsg": "Do you want to permanently remove all data belonging to this app? This cannot be undone.",
        "ErrorInit": "Error initialising MIRO",
        "ErrorRInstallHdr": "Failed to install R packages",
        "ErrorRInstallMsg": "The R packages required to run MIRO could not be installed. Check log file for more information.",
        "ErrorModelPathHdr": "No model path",
        "ErrorModelPathMsg": "You need to specify the path to the main gms file via the environment variable: MIRO_MODEL_PATH",
        "ErrorRNotFoundHdr": "R not found",
        "ErrorRNotFoundMsg": "In order to use MIRO, you have to have R installed. You can add the path to your R installations in the Preferences menu.",
        "BtnCancel": "Cancel",
        "BtnOk": "OK",
        "BtnRemove": "Remove",
        "ErrorInconsistentDbTablesHdr": "Inconsistent data",
        "ErrorInconsistentDbTablesMsg": "Your database contains records that are inconsistent with the new version of the MIRO application you want to add. Do you want to remove all inconsistent data? The records to be removed are: '{0}'."
    },
    "general": {
        "title": "MIRO Library",
        "noApps": "No apps",
        "btEdit": "Edit", 
        "btEditDone": "Done",
        "btAddExamples": "Add example apps",
        "appFilesPlaceholder": "Drop your MIRO app here or click to browse.",
        "appNamePlaceholder": "App title",
        "appDescPlaceholder": "Short model description (optional)",
        "appDbPathPlaceholder": "MIRO database path (optional)",
        "appLogoPlaceholder": "Different app logo? Drop your MIRO app logo here or click to browse.",
        "appDbPathReset": "Reset to default",
        "editAppInfoText": "Click on app to edit",
        "btLaunch": "Launch",
        "btLaunchBase": "Base mode",
        "btLaunchHcube": "Hypercube mode",
        "btCancel": "Cancel",
        "btSave": "Save",
        "btAddApp": "Add app",
        "errNoAppTitleHdr": "No title",
        "errNoAppTitleMsg": "Please enter a title for your MIRO app!",
        "errInvalidDbPathHdr": "Invalid database path",
        "errInvalidDbPathMsg": "The database path you selected does not exist.",
        "dialogSelectAppLogoHdr": "Select MIRO app logo",
        "dialogSelectAppLogoMsg": "Please select a logo for your MIRO app (jpg/jpeg/png supported)",
        "dialogSelectAppLogoBtn": "Choose",
        "dialogSelectAppLogoFilter": "Images",
        "dialogSelectDbPathHdr": "Select database path",
        "dialogSelectDbPathMsg": "Please select a directory in which the database should be located.",
        "dialogSelectDbPathBtn": "Select",
        "dialogErrHdr": "Unexpected error",
        "dialogErrMsg": "No MIRO app configuration was found. If this problem persists, please contact GAMS!",
        "dialogNewAppFilesHdr": "Select MIRO app",
        "dialogNewAppFilesMsg": "Please select the MIRO app you want to add.",
        "dialogNewAppFilesBtn": "Add app",
        "dialogNewAppFilesFilter": "MIRO apps"
   },
   "menu": {
        "pref": "Preferences",
        "about": "About GAMS MIRO",
        "services": "Services",
        "hide": "Hide GAMS MIRO",
        "unhide": "Show All",
        "hideothers": "Hide Others",
        "file": "File",
        "edit": "Edit",
        "addApp": "➕ Add MIRO app",
        "editApp": "⚙️ Edit apps",
        "addExampleApps": "Add example apps",
        "undo": "Undo",
        "redo": "Redo",
        "cut": "Cut",
        "copy": "Copy",
        "paste": "Paste",
        "selectAll": "Select all",
        "view": "View",
        "window": "Window",
        "minimize": "Minimize",
        "zoom": "Zoom",
        "front": "Bring all to front",
        "close": "Close Window",
        "quit": "Quit",
        "fullscreen": "Toggle Full Screen",
        "resetZoom": "Reset Zoom",
        "zoomIn": "Zoom In",
        "zoomOut": "Zoom Out",
        "help": "Help",
        "doc": "Documentation",
        "update": "Check for Update"
   },
   "settings": {
        "title": "Preferences",
        "general-tab": "General", 
        "paths-tab": "Paths", 
        "launchBrowser": "Launch MIRO apps in your browser?", 
        "browserReset": "Reset to default", 
        "generalLanguage": "Language", 
        "languageReset": "Reset to default", 
        "generalRemoteExec": "Execution of models on GAMS Engine?",
        "remoteExecReset": "Reset to default",
        "generalLogging": "Logging Level", 
        "loggingReset": "Reset to default", 
        "generalLoglife": "Number of days log files are stored", 
        "loglifeReset": "Reset to default", 
        "pathMiroapp": "MIRO app path", 
        "pathMiroappSelect": "Select MIRO app path", 
        "resetPathMiroapp": "Reset to default", 
        "pathGams": "GAMS path", 
        "pathGamsSelect": "Select GAMS path", 
        "pathGamsReset": "Reset to default", 
        "pathLog": "Log path", 
        "pathLogSelect": "Select log file path", 
        "pathLogReset": "Reset to default", 
        "pathR": "R path", 
        "pathRSelect": "Select R path", 
        "pathRReset": "Reset to default", 
        "needHelp": "Need help?", 
        "btSave": "Apply",
        "btCancel": "Close",
        "dialogConfigPathHdr": "Select MIRO app path",
        "dialogConfigPathMsg": "Please select your MIRO app directory.",
        "dialogConfigPathBtn": "Select",
        "dialogConfigPathLabel": "Select MIRO app path",
        "dialogGamsPathHdr": "Please select the path where you installed GAMS.",
        "dialogGamsPathMsg": "Please select the path where you installed GAMS.",
        "dialogGamsPathLabel": "Select GAMS path",
        "dialogGamsPathBtn": "Select",
        "dialogRPathHdr": "Please select the path where you installed R",
        "dialogRPathMsg": "Please select the path where you installed R.",
        "dialogRPathLabel": "Select R path",
        "dialogRPathBtn": "Select",
        "dialogLogPathHdr": "Please select the path where logs shall be stored",
        "dialogLogPathMsg": "Please select the path where logs shall be stored.",
        "dialogLogPathLabel": "Select log file path",
        "dialogLogPathBtn": "Select",
        "dialogLogLifeErrHdr": "Invalid log lifetime",
        "dialogLogLifeErrMsg": "The value you entered for the number of days \
log file should be stored is invalid! Please enter only whole numbers!",
        "dialogLogLifeErrBtn": "OK",
        "browseFiles": "Browse"
   }
}
const de = {
    "update": {
        "title": "Auf Update prüfen",
        "updateAvailable": "Eine neue Version von GAMS MIRO ist verfügbar:",
        "downloadUpdate": "Die neuste Version von GAMS MIRO finden Sie",
        "here": "hier",
        "upToDate": "Sie benutzen die neueste Version von GAMS MIRO.",
        "error": "Ein unerwarteter Fehler ist aufgetreten. Bitte versuchen Sie es später noch einmal.",
        "btClose": "Ok"
    },
    "main": {
        "ErrorUnexpectedHdr": "Unerwarteter Fehler",
        "ErrorUnexpectedMsg": "Die MIRO-App konnte nicht gestartet werden. Bitte kontaktieren Sie GAMS, wenn dieses Problem weiterhin besteht!",
        "ErrorUnexpectedMsg2": "Ein unerwarteter Fehler ist aufgetreten. Fehlermeldung:",
        "ErrorMsgLaunch": "Probleme beim Start der MIRO-App. Fehlermeldung:",
        "ErrorInvalidHdr": "Ungültige MIRO-App-Datei",
        "ErrorInvalidMsg": "Die von Ihnen ausgewählte Datei ist keine gültige MIRO-App!",
        "ErrorInvalidTwoMsg": "Bitte legen Sie nur eine einzige MIRO-App-Datei ab!",
        "ErrorInvalidThreeMsg": "Ungültige App",
        "ErrorAPIHdr": "MIRO-App nicht kompatibel",
        "ErrorAPIMsg": "Die MIRO-App, die Sie hinzufügen möchten, ist nicht mit der installierten MIRO-Version kompatibel. Bitte fragen Sie den Entwickler der App, diese zu aktualisieren und versuchen Sie es erneut!",
        "ErrorVersionMsg": "Die MIRO-App, die Sie hinzufügen möchten, wurde mit einer neueren MIRO-Version erstellt als die, die Sie verwenden. Bitte aktualisieren Sie Ihre MIRO-Version und versuchen Sie es erneut!",
        "ErrorAppIncompMsg": "Die zu startende MIRO-App ist nicht mit der installierten MIRO-Version kompatibel. Bitte fragen Sie den Entwickler der App ein Update durchzuführen und versuchen Sie es erneut!",
        "ErrorReadMsg": "Es gab ein Problem beim Lesen der MIRO-App-Datei. Fehlermeldung:",
        "ErrorLogoHdr": "Ungültiges MIRO-App-Logo",
        "ErrorLogoMsg": "Die von Ihnen ausgewählte Datei ist kein gültiges MIRO-Logo. Es werden nur jpg/jpeg und png unterstützt!",
        "ErrorLogoMultiMsg": "Bitte legen Sie nur ein einzelnes MIRO-App-Logo ab!",
        "ErrorLogoLargeHdr": "Logo zu groß",
        "ErrorLogoLargeMsg": "Logos dürfen nicht größer als 10MB sein!",
        "ErrorExampleExistsHdr": "Beispielmodelle existieren bereits",
        "ErrorModelExistsHdr": "Modell existiert bereits",
        "ErrorModelExistsMsg": "Die folgenden Beispielmodelle konnten nicht hinzugefügt werden, da sie bereits Teil Ihrer Bibliothek sind: ",
        "ErrorModelExistsMsg2": "Ein Modell mit dem gleichen Namen existiert bereits. Bitte löschen Sie zuerst dieses Modell, bevor Sie es erneut versuchen.",
        "ErrorWriteHdr": "Keine Schreibrechte",
        "ErrorWriteMsg": "Das Modell konnte nicht hinzugefügt werden, da Sie keine Berechtigungen zum Schreiben in diesem Verzeichnis haben:",
        "ErrorWriteMsg2": "Das Modell konnte nicht entfernt werden, da Sie keine Berechtigungen zum Schreiben in diesem Verzeichnis haben:",
        "ErrorWritePerm2Msg": "Das Modell konnte nicht gespeichert werden, da Sie keine Berechtigungen zum Schreiben in diesem Verzeichnis haben:",
        "ErrorNoWritePermMsg": "Das Modell konnte nicht aktualisiert werden, da Sie keine Berechtigungen zum Schreiben in diesem Verzeichnis haben:",
        "ErrorAppRunningHdr": "Anwendung läuft",
        "ErrorAppRunningMsg": "Für Ihre App läuft bereits ein MIRO-Prozess. Derzeit kann nur eine Instanz pro App gleichzeitig gestartet werden.",
        "ErrorInstallStartMsg": "Die MIRO-Installation konnte nicht gestartet werden. Bitte überprüfen Sie die Log-Dateien und kontaktieren Sie GAMS, wenn das Problem weiterhin besteht!",
        "ErrorInstallPermHdr": "Installation",
        "ErrorInstallPerm1Msg": "Sie haben keine Berechtigungen zur Installation von Bibliotheken in diesem Verzeichnis:",
        "ErrorInstallPerm2Msg": ". \nMöchten Sie MIRO stattdessen lokal installieren",
        "ErrorInstallPerm3Msg": "?\n Falls Sie MIRO global installieren wollen, dann sollten Sie das AppImage starten mit sudo und --no-sandbox Flag: sudo ./GAMS-MIRO-",
        "ErrorInstallPerm4Msg": ".AppImage --no-sandbox",
        "ErrorInstallPermBtnYes": "Ja, lokale Installation",
        "ErrorInstallPermBtnNo": "Nein, beenden",
        "ErrorInvalidPathHdr": "Pfad ungültig",
        "ErrorInvalidPathMsg": ": Der von Ihnen gewählte Pfad ist kein gültiger Pfad. Beachten Sie, dass für den Betrieb von MIRO mindestens die folgende Version benötigt wird: ",
        "ErrorInvalidPathMsgMac": ": Der von Ihnen gewählte Pfad ist kein gültiger Pfad. Beachten Sie, dass für den Betrieb von MIRO die folgende Version benötigt wird: ",
        "ErrorInvalidPathMsg2": "Bei der Validierung des ausgewählten Pfades ist ein unerwarteter Fehler aufgetreten. Pfad:",
        "ErrorMessage": "Fehlermeldung:",
        "SuccessUpdateHdr": "Konfiguration aktualisiert",
        "SuccessUpdateMsg": "Ihre Konfiguration wurde erfolgreich aktualisiert. MIRO muss neu gestartet werden, damit Ihre Änderungen wirksam werden. Wollen Sie MIRO jetzt neu starten?",
        "ErrorUnexpectedWriteMsg": "Konfigurationsdaten konnten nicht gespeichert werden. Fehlen Ihnen an dieser Stelle Schreibrechte:",
        "DeleteMsg": "Sind Sie sicher, dass Sie die App dauerhaft entfernen möchten?",
        "DeleteDataMsg": "Möchten Sie alle Daten, die zu dieser App gehören, dauerhaft entfernen? Dies kann nicht rückgängig gemacht werden.",
        "ErrorInit": "Fehler bei der Initialisierung von MIRO",
        "ErrorRInstallHdr": "Installation von R Paketen fehlgeschlagen",
        "ErrorRInstallMsg": "Die zum Betrieb von MIRO erforderlichen R Pakete konnten nicht installiert werden. Prüfen Sie die Logdatei für weitere Informationen.",
        "ErrorModelPathHdr": "Kein Modellpfad",
        "ErrorModelPathMsg": "Sie müssen den Pfad zur Haupt .gms Datei über die Umgebungsvariable angeben: MIRO_MODEL_PATH",
        "ErrorRNotFoundHdr": "R nicht gefunden",
        "ErrorRNotFoundMsg": "Um MIRO verwenden zu können, müssen Sie R installiert haben. Sie können den Pfad zu Ihren R-Installationen im Einstellungs-Menü hinzufügen.",
        "BtnCancel": "Abbrechen",
        "BtnOk": "OK",
        "BtnRemove": "Entfernen",
        "ErrorInconsistentDbTablesHdr": "Inkonsistente Daten",
        "ErrorInconsistentDbTablesMsg": "Ihre Datenbank enthält Datensätze, die mit der neuen Version der MIRO-Anwendung, die Sie hinzufügen möchten, nicht konsistent sind. Möchten Sie alle inkonsistenten Daten entfernen? Die zu entfernenden Datensätze sind: '{0}'."
    },
    "general": {
        "title": "MIRO Bibliothek",
        "noApps": "Keine Apps",
        "btEdit": "Bearbeiten", 
        "btEditDone": "Fertig",
        "btAddExamples": "Beispiel-Apps hinzufügen",
        "appFilesPlaceholder": "Legen Sie Ihre MIRO-App hier ab oder klicken Sie zum Durchsuchen.",
        "appNamePlaceholder": "App-Titel",
        "appDescPlaceholder": "Kurze Modellbeschreibung (optional)",
        "appDbPathPlaceholder": "MIRO-Datenbankpfad (optional)",
        "appLogoPlaceholder": "Anderes App-Logo? Legen Sie Ihr MIRO App-Logo hier ab oder klicken Sie zum Durchsuchen.",
        "appDbPathReset": "Zurücksetzen",
        "editAppInfoText": "Zum Bearbeiten auf App klicken",
        "btLaunch": "Starten",
        "btLaunchBase": "Basismodus",
        "btLaunchHcube": "Hypercube-Modus",
        "btCancel": "Abbrechen",
        "btSave": "Speichern",
        "btAddApp": "App hinzufügen",
        "errNoAppTitleHdr": "Kein Titel",
        "errNoAppTitleMsg": "Bitte geben Sie einen Titel für Ihre MIRO-App ein!",
        "errInvalidDbPathHdr": "Ungültiger Datenbankpfad",
        "errInvalidDbPathMsg": "Der von Ihnen gewählte Datenbankpfad existiert nicht.",
        "dialogSelectAppLogoHdr": "MIRO-App-Logo auswählen",
        "dialogSelectAppLogoMsg": "Bitte wählen Sie ein Logo für Ihre MIRO-App (jpg/jpeg/png unterstützt)",
        "dialogSelectAppLogoBtn": "Auswählen",
        "dialogSelectAppLogoFilter": "Bilder",
        "dialogSelectDbPathHdr": "Datenbankpfad auswählen",
        "dialogSelectDbPathMsg": "Bitte wählen Sie ein Verzeichnis aus, in dem sich die Datenbank befinden soll.",
        "dialogSelectDbPathBtn": "Auswählen",
        "dialogErrHdr": "Unerwarteter Fehler",
        "dialogErrMsg": "Es wurde keine MIRO-App-Konfiguration gefunden. Sollte dieses Problem weiterhin bestehen, kontaktieren Sie bitte die GAMS!",
        "dialogNewAppFilesHdr": "MIRO-App auswählen",
        "dialogNewAppFilesMsg": "Bitte wählen Sie die MIRO-App, die Sie hinzufügen möchten.",
        "dialogNewAppFilesBtn": "App hinzufügen",
        "dialogNewAppFilesFilter": "MIRO-Apps"
   },
   "menu": {
        "about": "Über GAMS MIRO",
        "services": "Dienste",
        "hide": "GAMS MIRO ausblenden",
        "unhide": "Alle anzeigen",
        "hideothers": "Andere ausblenden",
        "pref": "Einstellungen",
        "file": "Datei",
        "edit": "Bearbeiten",
        "addApp": "➕ MIRO-App hinzufügen",
        "editApp": "⚙️ Apps bearbeiten",
        "addExampleApps": "Beispiel-Apps hinzufügen",
        "undo": "Rückgängig",
        "redo": "Wiederholen",
        "cut": "Ausschneiden",
        "copy": "Kopieren",
        "paste": "Einfügen",
        "selectAll": "Alles auswählen",
        "view": "Ansicht",
        "window": "Fenster",
        "minimize": "Minimieren",
        "zoom": "Maximieren",
        "front": "Alle nach vorne bringen",
        "fullscreen": "Vollbild umschalten",
        "resetZoom": "Zoomfaktor zurücksetzen",
        "zoomIn": "Vergrößern",
        "zoomOut": "Verkleinern",
        "close": "Fenster schließen",
        "quit": "Beenden",
        "help": "Hilfe",
        "doc": "Dokumentation",
        "update": "Auf Update prüfen"
   },
   "settings": {
        "title": "Einstellungen",
        "general-tab": "Allgemein", 
        "paths-tab": "Verzeichnisse", 
        "launchBrowser": "MIRO-Apps in Ihrem Browser starten?", 
        "browserReset": "Auf Standard zurücksetzen", 
        "generalLanguage": "Sprache", 
        "languageReset": "Auf Standard zurücksetzen", 
        "generalRemoteExec": "Ausführung der Modelle auf GAMS Engine?",
        "remoteExecReset": "Auf Standard zurücksetzen",
        "generalLogging": "Log Level", 
        "loggingReset": "Auf Standard zurücksetzen", 
        "generalLoglife": "Speicherdauer von Logdateien (in Tagen)", 
        "loglifeReset": "Auf Standard zurücksetzen", 
        "pathMiroapp": "MIRO Apps", 
        "pathMiroappSelect": "MIRO Applikationsverzeichnis auswählen", 
        "resetPathMiroapp": "Auf Standard zurücksetzen", 
        "pathGams": "GAMS", 
        "pathGamsSelect": "GAMS Verzeichnis auswählen", 
        "pathGamsReset": "Auf Standard zurücksetzen", 
        "pathLog": "Log Dateien", 
        "pathLogSelect": "Verzeichnis der Logdateien auswählen", 
        "pathLogReset": "Auf Standard zurücksetzen", 
        "pathR": "R", 
        "pathRSelect": "R Verzeichnis auswählen", 
        "pathRReset": "Auf Standard zurücksetzen", 
        "needHelp": "Hilfe?", 
        "btSave": "Anwenden",
        "btCancel": "Schließen",
        "dialogConfigPathHdr": "MIRO Applikationsverzeichnis auswählen",
        "dialogConfigPathMsg": "Bitte wählen Sie Ihr MIRO-Applikationsverzeichnis aus.",
        "dialogConfigPathBtn": "Auswählen",
        "dialogConfigPathLabel": "MIRO Applikationsverzeichnis auswählen",
        "dialogGamsPathHdr": "Bitte wählen Sie das Verzeichnis aus, in dem Sie GAMS installiert haben",
        "dialogGamsPathMsg": "Bitte wählen Sie das Verzeichnis aus, in dem Sie GAMS installiert haben.",
        "dialogGamsPathLabel": "GAMS Verzeichnis auswählen",
        "dialogGamsPathBtn": "Auswählen",
        "dialogRPathHdr": "Bitte wählen Sie das Verzeichnis aus, in dem Sie R installiert haben",
        "dialogRPathMsg": "Bitte wählen Sie das Verzeichnis aus, in dem Sie R installiert haben.",
        "dialogRPathLabel": "R Verzeichnis auswählen",
        "dialogRPathBtn": "Auswählen",
        "dialogLogPathHdr": "Bitte wählen Sie das Verzeichnis aus, in dem die Logdateien gespeichert werden sollen",
        "dialogLogPathMsg": "Bitte wählen Sie das Verzeichnis aus, in dem die Logdateien gespeichert werden sollen.",
        "dialogLogPathLabel": "Verzeichnis der Logdateien auswählen",
        "dialogLogPathBtn": "Auswählen",
        "dialogLogLifeErrHdr": "Ungültige Log-Lebensdauer",
        "dialogLogLifeErrMsg": "Der von Ihnen eingegebene Wert für die Anzahl der Tage, die die Logdateien gespeichert werden soll, ist ungültig! Bitte nur ganze Zahlen eingeben!",
        "dialogLogLifeErrBtn": "OK",
        "browseFiles": "Durchsuchen"
   }
}
const cn = {
    "update": {
        "title": "检查更新",
        "updateAvailable": "GAMS MIRO有新版本可用：",
        "downloadUpdate": "您可以下载这个版本",
        "here": "这里",
        "upToDate": "您正在使用最新版本的GAMS MIRO",
        "error": "出现意外错误，请稍后再试。",
        "btClose": "确定"
    },
    "main": {
        "ErrorUnexpectedHdr": "意外错误",
        "ErrorUnexpectedMsg": "MIRO的应用程序无法启动。如果问题仍然存在, 请联系GAMS！",
        "ErrorUnexpectedMsg2": "出现意外错误。错误信息如下：",
        "ErrorMsgLaunch": "启动MIRO应用程序时出现错误。错误信息如下：",
        "ErrorInvalidHdr": "无效的MIRO应用程序文件",
        "ErrorInvalidMsg": "选择的文件不是有效的MIRO应用程序！",
        "ErrorInvalidTwoMsg": "请只删除一个MIRO应用程序文件！",
        "ErrorInvalidThreeMsg": "无效的应用程序",
        "ErrorAPIHdr": "MIRO应用程序不兼容",
        "ErrorAPIMsg": "要添加的MIRO应用程序与当前安装的MIRO版本不兼容。请联系开发者更新应用并重试！",
        "ErrorVersionMsg": "要添加的MIRO应用程序是由更高的MIRO版本创建的，请更新本地MIRO版本再重试。",
        "ErrorAppIncompMsg": "要启动的MIRO应用程序与当前安装的MIRO版本不兼容。请联系开发者更新应用并重试！",
        "ErrorReadMsg": "读取MIRO应用程序文件时出现错误，错误信息如下：",
        "ErrorLogoHdr": "无效的MIRO应用程序标志",
        "ErrorLogoMsg": "该文件不是有效的MIRO标志，仅支持JPG，JPEG和PNG！",
        "ErrorLogoMultiMsg": "请只删除一个MIRO应用程序标志！",
        "ErrorLogoLargeHdr": "标志太大",
        "ErrorLogoLargeMsg": "标志的大小不超过10MB!",
        "ErrorExampleExistsHdr": "示例模型已存在",
        "ErrorModelExistsHdr": "模型已存在",
        "ErrorModelExistsMsg": "无法添加以下示例模型，因为它们已存在：",
        "ErrorModelExistsMsg2": "具有相同名称的模型已经存在，请先删除此模型再重试。",
        "ErrorWriteHdr": "无写入权限",
        "ErrorWriteMsg": "无法添加模型，因为没有此处的写入权限：",
        "ErrorWriteMsg2": "无法删除模型，因为没有此处的写入权限：",
        "ErrorWritePerm2Msg": "无法保存模型，因为没有此处的写入权限：",
        "ErrorNoWritePermMsg": "无法更新模型，因为没有此处的写入权限：",
        "ErrorAppRunningHdr": "正在运行该应用程序",
        "ErrorAppRunningMsg": "当前已有运行的MIRO进程。目前，一次只能启动一个MIRO进程。",
        "ErrorInstallStartMsg": "无法安装MIRO。如果问题仍然存在，请检查日志文件并联系GAMS！",
        "ErrorInstallPermHdr": "安装",
        "ErrorInstallPerm1Msg": "没有在此处安装库的权限：",
        "ErrorInstallPerm2Msg": ". \n 是否要在本地安装MIRO",
        "ErrorInstallPerm3Msg": "?\n 如果要全局安装MIRO，请使用sudo和--no-sandbox启动 AppImage： sudo ./GAMS-MIRO-",
        "ErrorInstallPerm4Msg": ".AppImage --no-sandbox",
        "ErrorInstallPermBtnYes": "是, 本地安装",
        "ErrorInstallPermBtnNo": "否, 退出",
        "ErrorInvalidPathHdr": "无效路径",
        "ErrorInvalidPathMsg": "： 该路径不是有效路径。要运行MIRO，至少需要以下版本：",
        "ErrorInvalidPathMsgMac": "： 该路径不是有效路径。要运行MIRO，需要以下版本：",
        "ErrorInvalidPathMsg2": "验证所选路径时发生意外错误：",
        "ErrorMessage": "错误信息：",
        "SuccessUpdateHdr": "配置已更新",
        "SuccessUpdateMsg": "配置已成功更新。必须重新启动MIRO才能使配置的更改生效。是否要立即重启MIRO？",
        "ErrorUnexpectedWriteMsg": "无法保存配置数据。您是否缺少此位置的写入权限：",
        "DeleteMsg": "要永久删除该应用程序吗？",
        "DeleteDataMsg": "你想永久删除属于这个应用程序的所有数据吗？这一点是无法挽回的。",
        "ErrorInit": "初始化MIRO时出错",
        "ErrorRInstallHdr": "无法安装R软件包",
        "ErrorRInstallMsg": "无法安装运行MIRO所需的R软件包。检查日志文件以获取更多信息。",
        "ErrorModelPathHdr": "无模型路径",
        "ErrorModelPathMsg": "您需要通过环境变量指定主gms文件的路径：MIRO_MODEL_PATH",
        "ErrorRNotFoundHdr": "未找到R",
        "ErrorRNotFoundMsg": "为了使用MIRO，你必须安装了R。你可以在 \"首选项 \"菜单中添加到你的R安装路径。",
        "BtnCancel": "取消",
        "BtnOk": "确认",
        "BtnRemove": "删除",
        "ErrorInconsistentDbTablesHdr": "数据不一致",
        "ErrorInconsistentDbTablesMsg": "您的数据库中包含的记录与您要添加的MIRO应用程序的新版本不一致。你想删除所有不一致的数据吗？需要删除的记录有： '{0}'."
    },
    "general": {
        "title": "MIRO库",
        "noApps": "无应用程序",
        "btEdit": "编辑", 
        "btEditDone": "完成",
        "btAddExamples": "添加示例应用程序",
        "appFilesPlaceholder": "将MIRO应用程序放在此处或单击以浏览。",
        "appNamePlaceholder": "应用程序标题",
        "appDescPlaceholder": "缩短模型的说明（可选）",
        "appDbPathPlaceholder": "MIRO数据库路径（可选）",
        "appLogoPlaceholder": "不同的应用程序的标志？将MIRO应用程序放在此处或单击以浏览。",
        "appDbPathReset": "重置为默认设置",
        "editAppInfoText": "点击应用进行修改",
        "btLaunch": "启动",
        "btLaunchBase": "基本模式",
        "btLaunchHcube": "Hypercube模式",
        "btCancel": "取消",
        "btSave": "保存",
        "btAddApp": "添加应用程序",
        "errNoAppTitleHdr": "无标题",
        "errNoAppTitleMsg": "请为MIRO应用程序输入标题！",
        "errInvalidDbPathHdr": "无效的数据库路径",
        "errInvalidDbPathMsg": "该数据库路径不存在。",
        "dialogSelectAppLogoHdr": "选择MIRO应用标志",
        "dialogSelectAppLogoMsg": "请选择MIRO应用的标志（支持JPG / JPEG / PNG）",
        "dialogSelectAppLogoBtn": "选择",
        "dialogSelectAppLogoFilter": "图片",
        "dialogSelectDbPathHdr": "选择数据库路径",
        "dialogSelectDbPathMsg": "请选择数据库应位于的目录。",
        "dialogSelectDbPathBtn": "选择",
        "dialogErrHdr": "意外错误",
        "dialogErrMsg": "找不到MIRO应用配置。如果问题仍然存在，请联系GAMS！",
        "dialogNewAppFilesHdr": "选择MIRO应用程序",
        "dialogNewAppFilesMsg": "请选择要添加的MIRO应用程序",
        "dialogNewAppFilesBtn": "添加应用程序",
        "dialogNewAppFilesFilter": "MIRO应用程序"
   },
   "menu": {
        "pref": "偏好设置",
        "about": "关于GAMS MIRO",
        "services": "服务",
        "hide": "隐藏GAMS MIRO",
        "unhide": "显示所有",
        "hideothers": "隐藏其他",
        "file": "文件",
        "edit": "编辑",
        "addApp": "➕ 添加MIRO应用程序",
        "editApp": "⚙️ 编辑应用程序",
        "addExampleApps": "添加示例应用程序",
        "undo": "撤消",
        "redo": "重做",
        "cut": "剪切",
        "copy": "复制",
        "paste": "粘贴",
        "selectAll": "全选",
        "view": "视图",
        "window": "窗口",
        "minimize": "最小化",
        "zoom": "放大",
        "front": "将所有窗口前置",
        "close": "关闭窗口",
        "quit": "退出",
        "fullscreen": "切换全屏",
        "resetZoom": "重置缩放",
        "zoomIn": "放大",
        "zoomOut": "缩小",
        "help": "帮助",
        "doc": "文档",
        "update": "检查更新"
   },
   "settings": {
        "title": "偏好设置",
        "general-tab": "通用", 
        "paths-tab": "路径", 
        "launchBrowser": "在浏览器中启动MIRO应用程序？", 
        "browserReset": "重置为默认设置", 
        "generalLanguage": "语言", 
        "languageReset": "重置为默认设置", 
        "generalRemoteExec": "在GAMS Engine上执行模型?",
        "remoteExecReset": "重置为默认设置",
        "generalLogging": "日志记录级别", 
        "loggingReset": "重置为默认设置", 
        "generalLoglife": "日志文件的存储天数", 
        "loglifeReset": "重置为默认设置", 
        "pathMiroapp": "MIRO应用程序路径", 
        "pathMiroappSelect": "选择MIRO应用程序路径", 
        "resetPathMiroapp": "重置为默认设置", 
        "pathGams": "GAMS路径", 
        "pathGamsSelect": "选择GAMS路径", 
        "pathGamsReset": "重置为默认设置", 
        "pathLog": "日志文件路径", 
        "pathLogSelect": "选择日志文件路径", 
        "pathLogReset": "重置为默认设置", 
        "pathR": "R路径", 
        "pathRSelect": "选择R路径", 
        "pathRReset": "重置为默认设置", 
        "needHelp": "需要帮助？", 
        "btSave": "应用",
        "btCancel": "关闭",
        "dialogConfigPathHdr": "选择MIRO应用程序路径",
        "dialogConfigPathMsg": "请选择该MIRO应用程序的目录",
        "dialogConfigPathBtn": "选择",
        "dialogConfigPathLabel": "选择MIRO应用程序路径",
        "dialogGamsPathHdr": "请选择GAMS的安装路径",
        "dialogGamsPathMsg": "请选择GAMS的安装路径",
        "dialogGamsPathLabel": "选择GAMS路径",
        "dialogGamsPathBtn": "选择",
        "dialogRPathHdr": "请选择R的安装路径",
        "dialogRPathMsg": "请选择R的安装路径",
        "dialogRPathLabel": "选择R路径",
        "dialogRPathBtn": "选择",
        "dialogLogPathHdr": "请选择日志存储路径",
        "dialogLogPathMsg": "请选择日志存储路径",
        "dialogLogPathLabel": "选择日志文件路径",
        "dialogLogPathBtn": "选择",
        "dialogLogLifeErrHdr": "无效的日志周期",
        "dialogLogLifeErrMsg": "输入的值无效（天\ 应储存的日志文件）！ 请仅输入整数！",
		"dialogLogLifeErrBtn": "确定",
        "browseFiles": "浏览"
	}
}

class LangParser {
    constructor (lang) {
        if ( lang === 'de' ) {
            this.lang = de;
        } else if ( lang === 'cn' ) {
            this.lang = cn;
        } else {
            this.lang = en;
        }
        if ( !ajv.validate(schema, this.lang) ) {
            console.log(ajv.errors);
            throw new Error(ajv.errors);
        }
      
  }
  get (subkey){
     if ( subkey ) {
        return this.lang[subkey];
     }
     return this.lang;
  } 
}
module.exports = LangParser;

