
all: bin/hi


bin:
	mkdir -p bin

bin/hi: bin src/*.pas
	fpc -obin/hi ./src/hi.pas

clean:
	rm -f bin/hi
	rm -f bin/*.o
	rm -f bin/*.ppu