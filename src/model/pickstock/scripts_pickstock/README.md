# Run custom analysis script

To run the custom analysis that comes with the pickstock example model, you will need to set up a Python environment first:

```
python -m venv venv
source venv/bin/activate
pip install jupyter pandas matplotlib
```

You also need the GAMS Python API. Install it via the following command, replacing `<GAMS version>` with the version of your GAMS installation (e.g. `54.6.1`):

```
pip install "gamsapi[control]==<GAMS version>"
```

Once you've done that you need to tell MIRO about your Python environment by adding it to the PATH (either system-wide or via [MIRO environment files](https://gams.com/miro/deployment.html#custom-environments)).
