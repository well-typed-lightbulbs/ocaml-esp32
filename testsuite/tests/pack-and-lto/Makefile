default: native warning

native:
	printf " ... testing native 'test.reference':"
	@$(OCAMLOPT) -c -for-pack Pack package/a.ml
	@$(OCAMLOPT) -c -for-pack Pack package/b.ml
	@$(OCAMLOPT) -pack package/a.cmx package/b.cmx -o pack.cmx
	@$(OCAMLOPT) -c test.ml
	@$(OCAMLOPT) pack.cmx test.cmx -o test.native
	@./test.native > test.result
	@$(DIFF) test.result test.reference >/dev/null \
	    && echo " => passed" || echo " => failed"

warning:
	printf " ... testing warning with lto 'warning.reference':"
	@$(OCAMLOPT) -c no_lto.ml # no lto
	@$(OCAMLOPT) -c main_warning.ml 2> warning.result
	@$(DIFF) warning.result warning.reference >/dev/null \
	    && echo " => passed" || echo " => failed"


promote: defaultpromote

clean: defaultclean
	@rm -f *.result
	@rm -f test.native

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
COMPFLAGS =
