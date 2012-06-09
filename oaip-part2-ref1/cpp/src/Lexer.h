#ifndef _RG_Uni_ArithExpr_Lexer_h
#define _RG_Uni_ArithExpr_Lexer_h

#include "Tokenizer.h"
#include "Expression.h"

namespace RG {
namespace Uni {
namespace ArithExpr {
	using RG::Uni::DataStructures::List;
	using RG::Uni::ArithExpr::Token;

	class Lexer {
	public:
		Lexer(); 
		virtual ~Lexer();

		Expression* analyze( List<Token>* tokens );

	private:
		Expression* process( Expression* leftExpression, Token* tokens, int leftMost, int rightMost ) const;

		void ensure_is_operand( Token* tokens, int pos ) const;
		void ensure_is_operator( Token* tokens, int pos ) const;

		int find_closing_parenthesis( Token* tokens, int first, int rightMost ) const;
	};
}}}

#endif // _RG_Uni_ArithExpr_Lexer_h
