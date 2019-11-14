INCLUDES=-I +zip -I +bz2
OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES)
SOURCES=xstring.ml file.ml int.ml xlist.ml xset.ml xmap.ml bitArray.ml xstd.ml xunicode.ml extArray.ml xlatex.ml lexer.ml xjson.mli xjson.ml
INSTALLDIR=`ocamlc -where`/xlib

all: xlib.cma

opt: xlib.cmxa

install: all opt
	mkdir -p $(INSTALLDIR)
	cp xlib.cmxa xlib.a xlib.cma $(INSTALLDIR)
	cp xstring.cmi file.cmi int.cmi xlist.cmi xset.cmi xmap.cmi bitArray.cmi xstd.cmi xunicode.cmi extArray.cmi xlatex.cmi lexer.cmi xjson.cmi $(INSTALLDIR)
	cp xstring.cmx file.cmx int.cmx xlist.cmx xset.cmx xmap.cmx bitArray.cmx xstd.cmx xunicode.cmx extArray.cmx xlatex.cmx lexer.cmx xjson.cmx $(INSTALLDIR)

xlib.cma: $(SOURCES)
	ocamlc -linkall -a -o xlib.cma $(OCAMLFLAGS) $^

xlib.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o xlib.cmxa $(OCAMLOPTFLAGS) $^

clean:
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a
