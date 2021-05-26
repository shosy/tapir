SOURCES = utilities.mli utilities.ml s.mli s.ml m.mli m.ml \
piSyntax.mli piSyntax.ml piParser.ml piLexer.ml alphaconv.mli alphaconv.ml \
simpleType.mli simpleType.ml simpleTyping.mli simpleTyping.ml \
seqSyntax.mli seqSyntax.ml simpleTransform.mli simpleTransform.ml \
refinementType.mli refinementType.ml refinementTyping.mli refinementTyping.ml \
main.ml

RESULT = tapir

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile
