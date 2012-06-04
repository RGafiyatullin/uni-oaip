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
		Expression* process( Token* tokens, int leftMost, int rightMost );

		void ensure_is_operand( Token* tokens, int pos );
	};
}}}

#endif // _RG_Uni_ArithExpr_Lexer_h
