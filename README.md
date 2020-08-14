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
* DT
* sortable
* chartjs (https://github.com/GAMS-dev/miro_desktop/tree/master/r-src/chartjs)


MIRO will install these packages automatically when you first launch it. When using certain graphs and especially in the MIRO Configuration Mode, the following additional packages are required:

* plotly
* xts
* dygraphs
* leaflet
* leaflet.minicharts
* timevis

MIRO will install these when it detects that they are required.

If you want to use gdx as a file exchange with GAMS (which is the default for all sample models), you will also need to install either the gdxrrwMIO package, which you can find [here](https://github.com/GAMS-dev/gdxrrw-miro).

Some (optional) functions are provided by the package miro.util, which you can find [here](https://github.com/GAMS-dev/miro.util). If you do not have this package installed, fallback functions are used.

To start the application, run the following command from an R console:
```
shiny::runApp(launch.browser=TRUE)
```

## Build Javascript/CSS
We use Node.JS, npm and webpack to lint, bundle and minify Javascript and compile Less to CSS. You can install npm from [here](https://www.npmjs.com/get-npm). 
To install the required dependencies, run `npm install` from the root of the GAMS MIRO repository. To build CSS and Javascript files from the `srcjs` and `less` directories, run `npm run build`. This creates bundled and minified files inside the `www` directory. MIRO only reads these minified versions and not the source files.
While you are developing you can run `npm run watch` to rebuild the output whenever you change one of the source files. 

## License

GAMS MIRO as a whole is distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3). 
See the [LICENSE](LICENSE) file for more details.