
all: bin/parser


bin:
	mkdir -p bin

bin/parser: bin src/*.pas
	fpc -obin/parser ./src/parser.pas

clean:
	rm -f bin/parser
	rm -f bin/*.o
	rm -f bin/*.ppu