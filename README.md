# Filigrane

## A Useful REST API diff tool

Filigrane is a lightweight http server to compute useful diff between two API versions : the `reference` and the `candidate`. Each request will be routed to the two backends, a diff will be computed on the two responses payloads and the summary will be responded back.

### Setup

You can follow the usual setup for OCaml [here](https://ocaml.org/learn/tutorials/up_and_running.html).
Once ready, you can go to the project and enter : 
```
make deps
```
to fetch all the dependencies to complete the setup.

### Build

Just enter `make clean` then `make` to have a freshly compiled binary.

### Usage

The current flags arre available :
```
--candidate // required, the candidate host
--reference // required, the reference host
--host      // optional, default is 8000
--scheme    // optional, default https 
```

