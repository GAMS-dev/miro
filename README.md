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

In order to launch GAMS MIRO you need R (v. 4.0.2 or later) with the following packages installed:

* shiny (https://github.com/GAMS-dev/shiny)
* shinydashboard (https://github.com/GAMS-dev/shinydashboard)
* stringi
* processx
* V8
* dplyr
* readr
* readxl
* writexl
* rhandsontable (https://github.com/GAMS-dev/rhandsontable)
* jsonlite (https://github.com/GAMS-dev/jsonlite)
* jsonvalidate 
* futile.logger
* zip
* tidyr
* DBI (https://github.com/GAMS-dev/miro_desktop/tree/master/r-src/DBI)
* RSQLite (https://github.com/GAMS-dev/RSQLite)
* digest

MIRO will install these packages automatically when you first launch it. Note that some of the required packages have been customized to work seamlessly with MIRO. These packages are automatically downloaded from the corresponding GitHub repository. The URLs where you can find these packages are given in brackets. When using certain graphs and especially in the MIRO Configuration Mode, the following additional packages are required:

* plotly (https://github.com/GAMS-dev/plotly)
* xts
* dygraphs
* leaflet (https://github.com/GAMS-dev/leaflet)
* leaflet.minicharts
* timevis (https://github.com/GAMS-dev/timevis)
* DT (https://github.com/GAMS-dev/DT)

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