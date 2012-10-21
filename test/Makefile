include ../../Makefile.def
PKGS:=unix threads num extlib cryptokit bitstring bitstring.syntax lwt.unix oUnit
SRCS:=\
	test_utils.ml\
	test.ml\

all: test

test: $(SRCS)
	$(OPT) -o $@ -linkpkg -package "$(PKGS)" -I ../ lwt-websocket.cmxa $(SRCS)

rebuild:
	make clean
	make all

clean:
	rm -f *.cm* *.o test

