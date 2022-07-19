## Installation

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
* DBI
* RSQLite
* digest
* DT (https://github.com/GAMS-dev/DT)
* sortable
* chartjs (https://github.com/GAMS-dev/miro/tree/master/r-src/chartjs)


MIRO will install these packages automatically when you first launch it (e.g. `R -f app.R`). Note that some of the required packages have been customized to work seamlessly with MIRO. These packages are automatically downloaded from the corresponding GitHub repository. The URLs where you can find these packages are given in brackets. When using certain graphs and especially in the MIRO Configuration Mode, the following additional packages are required:

* plotly (https://github.com/GAMS-dev/plotly)
* xts
* dygraphs
* leaflet (https://github.com/GAMS-dev/leaflet)
* leaflet.minicharts
* timevis (https://github.com/GAMS-dev/timevis)

MIRO will install these when it detects that they are required.

If you want to use gdx as a file exchange with GAMS (which is the default for all sample models), you will also need to install the gdxrrwMIRO package, which you can find [here](https://github.com/GAMS-dev/gdxrrw-miro).

Some (optional) functions are provided by the package miroUtil, which you can find [here](https://github.com/GAMS-dev/miro.util). If you do not have this package installed, fallback functions are used.

To start the application, run the following command from an R console:
```
shiny::runApp(launch.browser=TRUE)
```
or directly from the command line:
```
R -e "shiny::runApp(launch.browser=TRUE)"
```

## Build Javascript/CSS
We use Node.JS, npm and webpack to lint, bundle and minify Javascript and compile Less to CSS. You can install npm from [here](https://www.npmjs.com/get-npm).
To install the required dependencies, run `npm install` from the root of the GAMS MIRO repository. To build CSS and Javascript files from the `srcjs` and `less` directories, run `npm run build`. This creates bundled and minified files inside the `www` directory. MIRO only reads these minified versions and not the source files.
While you are developing you can run `npm run watch` to rebuild the output whenever you change one of the source files.

## License

GAMS MIRO as a whole is distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3).
See the [LICENSE](LICENSE) file for more details.
