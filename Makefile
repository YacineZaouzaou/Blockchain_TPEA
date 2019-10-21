OUT = a.out

SOURCES = client.ml

CAMLC = ocamlc

WITHUNIX = unix.cma -cclib -lunix
LIBS = $(WITHUNIX)

OBJS = $(SOURCES:.ml=.cmo)

all:
	$(CAMLC) -c $(SOURCES)
	$(CAMLC) $(LIBS) $(OBJS) -o $(OUT)

run: all
	./$(OUT)

clean:
	rm -f $(OUT) *.cmi *.cmo *~
