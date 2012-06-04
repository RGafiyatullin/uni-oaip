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
	Expression* Lexer::analyze( List<Token>* tokenStream ) {
		int tokenCount = tokenStream->count();
		Token* tokens = new Token[tokenCount];
		tokenStream->to_buffer(tokens);

		Expression* expr = process( tokens, 0, tokenCount - 1 );

		delete [] tokens;
		return expr;
	}

	void Lexer::ensure_is_operand( Token* tokens, int pos ) {
		TokenType t = tokens[pos].type;
		if ( t != tok_Operand_Literal && t != tok_Operand_Binding ) {
			char* buf = new char[128];
			sprintf(buf, "syntax error: token #%d is expected to be literal/binding", pos);
			throw buf;
		}
	}
	void Lexer::ensure_is_operator( Token* tokens, int pos ) {

	}


	Expression* Lexer::process( Token* tokens, int leftMost, int rightMost ) {
		if ( rightMost - leftMost == 2 ) {
			ensure_is_operand( tokens, leftMost );
			ensure_is_operand( tokens, rightMost );
			ensure_is_operator( tokens, leftMost + 1 );
			Expression* e = new Expression();
			e->set_left_operand( new Value( tokens[leftMost].symbol ) );
			e->set_right_operand( new Value( tokens[rightMost].symbol ) );
			
		}
	}


}}}