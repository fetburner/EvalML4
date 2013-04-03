SOURCES = prim.ml exp.ml value.ml deriv.mli deriv.ml parser.mly lexer.mll main.ml
RESULT = EvalML4 
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
