.PHONY: 	all clean byte native profile debug sanity test

OCB_FLAGS   = -use-ocamlfind -pkg alcotest -yaccflag "-v" -use-menhir -package ppx_tools.metaquot -package compiler-libs.common -I tests -I src -I lib
OCB = rebuild $(OCB_FLAGS)

all: ppx test_ppx # profile debug

clean:
	$(OCB) -clean
	rm -Rvf demo/node_modules/stylite-ppx
	rm -Rvf demo/lib

sanity:
	which menhir

ppx.native: sanity
	$(OCB) ppx.native

demo/node_modules/stylite-ppx:
	cp -Rv stylite-ppx demo/node_modules/stylite-ppx

demo/node_modules/stylite-ppx/ppx: ppx.native demo/node_modules/stylite-ppx
	cp -v ppx.native demo/node_modules/stylite-ppx/ppx

ppx: demo/node_modules/stylite-ppx/ppx

test_ppx: ppx.native
	refmt -p ml demo/src/Demo.re > _build/Demo.ml
	echo "\n\n"
	ocamlfind ppx_tools/rewriter ppx.native _build/Demo.ml

test.native: sanity
	$(OCB) test.native

demo: ppx
	rm -Rvf demo/lib
	cp -Rv stylite-ppx demo/node_modules/stylite-ppx
	cp -v ppx.native demo/node_modules/stylite-ppx/ppx
	cd demo && yarn run build

test: test.native
	./test.native