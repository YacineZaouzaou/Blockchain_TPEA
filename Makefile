all:
	dune build author.exe politician.exe
	cp ./_build/default/author.exe author
	cp ./_build/default/politician.exe politician
run_author: all
	./author
run_politician: all
	./politician 12345
run : all
	./politician 12345 &
	./politician 12345 &
	./author &
	./author &
	./author &
	./author &
	./author &
	./author &
clean:
	dune clean
	rm -f dune-project author politician *~ \#* .\#*
