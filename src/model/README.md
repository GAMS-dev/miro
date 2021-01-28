# Example models
This directory contains some examples of GAMS models that are used with MIRO. Most of these models are packages and come with MIRO, so you can import them when you open the MIRO library to get started. They are also zipped and uploaded to the [MIRO documentation](https://www.gams.com/miro/start.html#demo-applications)
This zip archive contains a GAMS library file (`glb`) and can be imported into GAMS Studio as a user model library.

# Adding a new example model
To add a new sample model, you need to do the following:
1. put your MIRO app in a new subdirectory with the same name as your main `gms` file.
1. edit the file `mirolib.glb` which is in this directory and add your model there
1. edit the `/scripts/create_miro_lib.sh` script (from the root of this repository), which is responsible for packaging the applications during the build process so that they end up in the [MIRO documentation](https://www.gams.com/miro/).
1. edit the `/components/example-apps.js` file (from the root of this repository) that defines the metadata of all the sample models in the MIRO library.

# Adding the example model to the MIRO gallery
If you want your example model to be visible in the [MIRO Gallery](https://miro.gams.com), you need to place the image you want to use as logo in the directory `/doc/gallery/data/logos`. Please note that the gallery currently only supports `png` files and the logo must have the same name as your app.
Also, your app must be added to the file `/doc/gallery/data/apps.json`.

# Things to consider
1. everything that is part of your app (including logos and other images) must not be copyrighted by third parties. We recommend using CC0 licensed images.
1. make sure your model runs on all target platforms: Linux, macOS, and Windows.
