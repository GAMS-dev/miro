# MIRO

GAMS MIRO is a graphical user interface (GUI) for your GAMS models that runs in your browser based on the [R/shiny package](https://github.com/rstudio/shiny).

You can find the documentation with examples [here](http://gams.com/miro).

If you have any questions about MIRO, please contact us via [mail](miro@gams.com). For bug reports, please use the [issue tracker](https://git.gams.com/fproske/gmswebui/issues).

## Features

* Quick & automated deployment of GAMS models
* Data visualization with wide range of different diagrams and charts
* Generation, processing, evaluation & storing of scenario data
* Creation & analysis of performance statistics
* Intuitive use of a model without GAMS knowledge
* Easy to use & comfortable working environment
* Highly customizable

## Installation

Simply pull the repository. Some example models that help you getting started can be found inside the *model* folder.

## Getting started

In order to launch GAMS MIRO you need R (v. 3.6.0 or later) with the following packages installed:

* shiny
* shinydashboard
* stringi
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
* digest

To start the application, run the following command from an R console:
```
shiny::runApp(launch.browser=TRUE)
```

## License

GAMS MIRO as a whole is distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3). 
See the [LICENSE](LICENSE) file for more details.