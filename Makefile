all: filigrane

filigrane: _build/default/src/filigrane.exe
	cp _build/default/src/filigrane.exe filigrane


_build/default/src/filigrane.exe:
	dune build

deps:
	opam install cohttp-lwt-unix tls yojson

clean:
	rm -rf _build filigrane