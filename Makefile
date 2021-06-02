SOURCES = utilities.mli utilities.ml s.mli s.ml m.mli m.ml \
piSyntax.mli piSyntax.ml piParser.ml piLexer.ml \
alphaconv.mli alphaconv.ml \
simpleType.mli simpleType.ml simpleTyping.mli simpleTyping.ml \
sort.mli sort.ml \
seqSyntax.mli seqSyntax.ml simpleTransform.mli simpleTransform.ml \
refinementType.mli refinementType.ml refinementTyping.mli refinementTyping.ml \
refinementTransform.mli refinementTransform.ml \
modelSyntax.mli modelSyntax.ml modelParser.mly modelLexer.mll \
main.ml

RESULT = tapir

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile
