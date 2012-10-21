PACKNAME:=Lwt_websocket
MODNAME:=lwt_websocket
LIBNAME:=lwt-websocket
PKGS:=unix threads num extlib cryptokit bitstring bitstring.syntax lwt.unix lwt.syntax
CAMLC:=ocamlfind ocamlc -g -thread
OPT:=ocamlfind ocamlopt -g -thread
#LWT_DEBUG:=-ppopt -lwt-debug
LWT_DEBUG:=
VERSION:=1.0.0

MLS:=\
	utils.ml\
	httpRequest.ml\
	frame.ml\
	channel.ml\

MLIS:=\
	httpRequest.mli\
	frame.mli\
	channel.mli\

CMXS:=$(MLS:.ml=.cmx)
CMOS:=$(MLS:.ml=.cmo)
CMIS:=$(MLIS:.mli=.cmi)
CMXS_DEBUG:=$(MLS:.ml=.debug.cmx)

all:$(LIBNAME).cma $(LIBNAME).cmxa META

$(MODNAME).cmo:$(CMIS) $(CMOS)
	$(CAMLC) -pack -o $@ $(CMOS)

$(MODNAME).cmx:$(CMIS) $(CMXS)
	$(OPT) -pack -o $@ $(CMXS)

$(LIBNAME).cma:$(MODNAME).cmo
	$(CAMLC) -a -o $@ $<

$(LIBNAME).cmxa:$(MODNAME).cmx
	$(OPT) -a -o $@ $<

%.cmi:%.mli
	$(CAMLC) -syntax camlp4o -package "$(PKGS)" -for-pack $(PACKNAME) -c $<

%.cmo:%.ml
	$(CAMLC) -syntax camlp4o -package "$(PKGS)" -for-pack $(PACKNAME) -c $<

%.cmx:%.ml
	$(OPT) -syntax camlp4o $(LWT_DEBUG) -package "$(PKGS)" -for-pack $(PACKNAME) -c $<

META:Makefile
	echo "description = \"lwt websocket\"" > $@
	echo "version = \"$(VERSION)\"" >> $@
	echo "archive(byte) = \"$(LIBNAME).cma\"" >> $@
	echo "archive(native) = \"$(LIBNAME).cmxa\"" >> $@
	echo "requires = \"$(PKGS)\"" >> $@

install:
	ocamlfind install $(LIBNAME) META $(MODNAME).* $(LIBNAME).* httpRequest.mli frame.mli channel.mli

uninstall:
	ocamlfind remove $(LIBNAME)

reinstall:
	make uninstall
	make install

clean:
	rm -f *.cm* *.o *.a *.out META

rebuild:
	make clean
	make all

#%.mli:%.ml
#	$(CAMLC) -syntax camlp4o -package "$(PKGS)" -i $< > $@

#channel.mli:channel.ml
#	$(CAMLC) -syntax camlp4o -package "$(PKGS)" -i $<

