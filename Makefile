
OBJ_LIST = obj/Lexer.o obj/Expression.o obj/Tokenizer.o obj/List.o obj/main.o
CXX = g++ -c -O0 -g
LNK = g++ -O0

all: bin/arith

bin:
	mkdir -p bin

obj:
	mkdir -p obj

obj/Lexer.o: obj src/Lexer.h src/Lexer.cpp src/List.h
	$(CXX) -o obj/Lexer.o src/Lexer.cpp

obj/Tokenizer.o: obj src/Tokenizer.h src/Tokenizer.cpp src/List.h
	$(CXX) -o obj/Tokenizer.o src/Tokenizer.cpp

obj/List.o: obj src/List.h src/List.cpp
	$(CXX) -o obj/List.o src/List.cpp

obj/Expression.o: obj src/Expression.h src/Expression.cpp src/List.h
	$(CXX) -o obj/Expression.o src/Expression.cpp

obj/main.o: obj src/main.h src/main.cpp src/List.h
	$(CXX) -o obj/main.o src/main.cpp

bin/arith: bin $(OBJ_LIST)
	$(LNK) -o bin/arith \
		$(OBJ_LIST)


clean:
	rm -rf obj
	rm -rf bin
