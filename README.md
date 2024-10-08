# MIRO

GAMS MIRO is a graphical user interface (GUI) for your GAMS models that runs in your browser based on the [R/Shiny package](https://github.com/rstudio/shiny).

You can find the documentation with examples [here](http://gams.com/miro).

If you have any questions about MIRO, please contact us via [mail](miro@gams.com). For bug reports, please use the [issue tracker](https://github.com/GAMS-dev/miro/issues).

GAMS MIRO consists of multiple components. To launch GAMS MIRO applications from your desktop, an [Electron](https://electronjs.org) application is used. The code for this component is located in the root of this repository.

The components needed to run GAMS MIRO on an HTTP server are located in the subdirectory `server`.

The source code of the R/Shiny-based MIRO core application is located in the subdirectory `src`.

## Features

* Quick & automated deployment of GAMS models
* Data visualization with wide range of different diagrams and charts
* Generation, processing, evaluation & storing of scenario data
* Intuitive use of a model without GAMS knowledge
* Easy to use & comfortable working environment
* Highly customizable

## Building

This repository contains a number of submodules. Thus, you need to clone it via: `git clone --recurse-submodules https://github.com/GAMS-dev/miro.git`.

For information about how to build GAMS MIRO Desktop as well as the GAMS MIRO Docker image from source, please see [build/README.md](build/README.md). In case you just want to run the R/Shiny MIRO app, find more information in [src/README.md](src/README.md).

## Contributing

This repository uses [pre-commit](https://pre-commit.com) to automatically lint the code you want to commit. This prevents many common errors and ensures that the CI pipeline passes the linting stage.
Please follow the installation instructions at: [https://pre-commit.com/#install](https://pre-commit.com/#install) to install `pre-commit`. To enable the hooks, run:
```
   pre-commit install
```
### Coding styles
We follow the [Tidyverse Style Guide](https://style.tidyverse.org) for R code and the [Airbnb JavaScript Style Guide](https://github.com/airbnb/javascript) for JavaScript.


To style R code automatically as part of our `pre-commit` hook, we require R and the R packages `precommit` and `styler` to be installed. You have to install them manually (`install.packages(c('precommit','styler'))`).

## License

GAMS MIRO as a whole is distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3).
Each MIRO component has its own license information. Information about the Electron based MIRO Library can be found in the file [LICENSE](LICENSE). More details about the software used for the R/Shiny application can be found in the file [src/LICENSE](src/LICENSE). The license information for GAMS MIRO Server can be found in the [server/LICENSE](server/LICENSE) file.
