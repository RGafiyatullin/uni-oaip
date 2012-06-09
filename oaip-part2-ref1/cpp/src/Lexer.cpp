#include "Lexer.h"

namespace RG {
namespace Uni {
namespace ArithExpr {
	using RG::Uni::DataStructures::List;
	using RG::Uni::ArithExpr::Token;

	class Triple {
	private:
	public:
		Token T1;
		Token T2;
		Token T3;

		Triple( const Token& t1, const Token& t2, const Token& t3 ) :
			T1(t1), T2(t2), T3(t3)
		{}
	};

	Lexer::Lexer() {}
	Lexer::~Lexer() {}
	Expression* Lexer::analyze( List<Token>* tokenStream ) const {
		int tokenCount = tokenStream->count();
		Token* tokens = new Token[tokenCount];
		tokenStream->to_buffer(tokens);

		Expression* expr = process( NULL, tokens, 0, tokenCount - 1 );

		delete [] tokens;
		return expr;
	}

	void Lexer::ensure_is_operand( Token* tokens, int pos ) const {
		TokenType t = tokens[pos].type;
		if ( t != tok_Operand_Literal && t != tok_Operand_Binding ) {
			char* buf = new char[128];
			sprintf(buf, "syntax error: token #%d is expected to be a literal or a binding", pos);
			throw buf;
		}
	}
	void Lexer::ensure_is_operator( Token* tokens, int pos ) const {
		TokenType t = tokens[pos].type;
		if ( t == tok_Operand_Literal || t == tok_Operand_Binding ) {
			char* buf = new char[128];
			sprintf(buf, "syntax error: token #%d is expected to be an operator", pos);
			throw buf;
		}
	}

	int Lexer::find_closing_parenthesis( Token* tokens, int first, int rightMost ) const {
		assert(tokens[first].type == tok_ParenthesisOpen);

		int depth = 1;
		for ( int i = first + 1; i <= rightMost; i++ ) {
			Token tok = tokens[i];
			switch (tok.type) {
				case tok_ParenthesisOpen:
					depth++;
					break;
				case tok_ParenthesisClose:
					depth--;
					break;
			}
			if ( depth < 0 ) {
				char* buf = new char[128];
				sprintf(buf, "syntax error: token #%d an unmatched closing parenthesis", i);
				throw buf;
			}
			if ( depth == 0 ) {
				return i;
			}
		}
		return -1;
	}

	Expression* Lexer::process( Expression* leftExpression, Token* tokens, int leftMost, int rightMost ) {
		for ( int tokenPos = 0; tokenPos <= rightMost; tokenPos++ ) {
			Token first = tokens[tokenPos];
			switch ( first.type ) {
				case tok_ParenthesisOpen:
					int closingPos = find_closing_parenthesis( tokens, first, rightMost );
					int openingPos = tokenPos;
					
					break;
			}
		}
	}


}}}