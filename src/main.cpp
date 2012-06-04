
#include <stdio.h>

#include "main.h"
#include "List.h"
#include "Expression.h"
#include "Tokenizer.h"
#include "Lexer.h"

int main(int argc, char** argv) {
	using RG::Uni::DataStructures::List;
	using RG::Uni::ArithExpr::Tokenizer;
	using RG::Uni::ArithExpr::Token;
	using RG::Uni::ArithExpr::Lexer;
	using RG::Uni::ArithExpr::Expression;

	try {
		List<char> exprChars;
		
		for (int arg_i = 1; arg_i < argc; arg_i++) {
			exprChars.push_back(' ');
			int ch_i = 0;
			char* arg = argv[arg_i];
			while ( arg[ch_i] != '\0' ) {
				exprChars.push_back(arg[ch_i]);
				ch_i++;
			}
		}
		Tokenizer tok;
		List<Token>* tokens = tok.tokenize(&exprChars);

		Lexer lex;
		Expression* expr = lex.analyze( tokens );


		delete expr;
		delete tokens;
		printf("\n");
		return 0;
	}
	catch ( const char* error ) {
		printf("Caught exception: %s\n", error);
		return 1;
	}
}
