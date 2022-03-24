# Filigrane

## A Useful REST API diff tool

Filigrane is a small command line utility to compute useful diff between two API versions : the `reference` and the `candidate`. Each of the paths that need to be evaluated will output a line of JSON on the standard output.

### Setup

You can follow the usual setup for OCaml [here](https://ocaml.org/learn/tutorials/up_and_running.html).
Once ready, you can go to the project and enter : 
```
make deps
```
to fetch all the dependencies to complete the setup.

### Build

Just enter `make clean` then `make` to have a freshly compile binary.

### Usage

The basic usage is like this :
```
./filigrane --candidate <candidate> --reference <reference> <path1> [<path2>] ...
```

You can combine it with other utilities to have a practical report on a set of url :
```
cat urls.txt | xargs filigrane --candidate test.api.com --reference prod.api.com > report.json
```

