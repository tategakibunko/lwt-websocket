all: server.exe client.exe

server.exe:server.ml
	ocamlfind ocamlopt -o $@ -thread -linkpkg -package "lwt-websocket" $<

client.exe:client.ml
	ocamlfind ocamlopt -o $@ -thread -linkpkg -package "lwt-websocket" $<

clean:
	rm -f *.cm* *.o *.a *.exe

rebuild:
	make clean
	make all
