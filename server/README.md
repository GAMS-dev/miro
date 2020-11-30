# Building from source
## Dependencies
To build GAMS MIRO Server from source, you need to have the following dependencies installed and added to the PATH:
1. Apache Maven (including Java)
1. Python >= 3.7
1. Docker
1. Docker Compose

## Build
To build GAMS MIRO Server from source, simply run `python miro_server.py build`.

# Launching GAMS MIRO Server
To launch GAMS MIRO Server, run `python miro_server.py up`.

# Stopping GAMS MIRO Server
To stop a running instance of GAMS MIRO Server, run `python miro_server.py down`. In case you want to remove all persistent volumes (and therefore all data), run `python miro_server.py down -v`.
