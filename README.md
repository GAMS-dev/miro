# MIRO

GAMS MIRO is a graphical user interface (GUI) for your GAMS models that runs in your browser.

You can find the documentation with examples [here](http://gams.com/miro).

If you have any questions about MIRO, please contact us via [mail](support@gams.com). For bug reports, please use the [issue tracker](https://git.gams.com/fproske/gmswebui/issues).

## Features

* Quick & automated deployment of GAMS models
* Data visualization with wide range of different diagrams and charts
* Generation, processing, evaluation & storing of scenario data
* Creation & analysis of performance statistics
* Intuitive use of a model without GAMS knowledge
* Easy to use & comfortable working environment
* Highly customizable

## Installation

To get the latest version from GITLAB, run the following from the console:

```
git pull command
```

## Getting started

For a running application you need R (v. 3.5.1 or later) with the following packages installed:

* stringi
* shiny
* shinydashboard
* processx
* V8
* dplyr
* readr
* readxl
* writexl
* rhandsontable
* jsonlite
* jsonvalidate
* rpivotTable
* futile.logger
* zip
* tidyr
* DBI
* RSQLite
* openssl
* DT

To start the application, run the following command from an R console:
```
shiny::runApp(launch.browser=TRUE)
```

## License

The GAMS WebUI as a whole is distributed under GPL-2 (GNU GENERAL PUBLIC LICENSE version 2), or (at your option) any later version. 
See the [LICENSE](LICENSE) file for more details.