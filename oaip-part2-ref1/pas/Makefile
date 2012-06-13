
all: bin/parser bin/trees bin/exam


bin:
	mkdir -p bin

bin/parser: bin src/*.pas
	fpc -obin/parser ./src/parser.pas

bin/trees: bin src/*.pas
	fpc -obin/trees ./src/trees.pas

clean:
	rm -f bin/parser
	rm -f bin/trees
	rm -f bin/exam
	rm -f bin/*.o
	rm -f bin/*.ppu


parser-tests: bin/parser
	bin/parser 'x^(y+z*s)*d-c/s'
	bin/parser 'a+c-e^(x-y/z)*s'
	bin/parser 'a-b^d*(e+s/d)-w'
	bin/parser 's*(a^x+b^y)-z/f'
	bin/parser 'd^(u+v/c)*(s-w)'
	bin/parser '(d-e)^f*g+s/a+w'
	bin/parser 'b*((x-y/c)*(z^d+a))-e'
	bin/parser 't*(y^(z-u)+f)-h/m'
	bin/parser 'e+s*(a-t^e)+q/p'
	bin/parser 'g*(u+j*n)^s+a/p'
	bin/parser '(y-z/v)*a^c/d+u'
	bin/parser 'f-t^(k*(s-h)-n*d^a)'
	bin/parser '((t+v-a/b)+(s-z^d))*k'
	bin/parser 'k*(m+n*j)^i-v/u'
	bin/parser '(d-c/x^e)*z+f*e'


bin/exam:
	fpc -obin/exam ./exam.src/exam.pas