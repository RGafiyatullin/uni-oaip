
#ifndef _RG_Uni_AirthExpr_Tokenizer_h
#define _RG_Uni_AirthExpr_Tokenizer_h

#include "List.h"
#include <string.h>
#include <stdio.h>

namespace RG {
namespace Uni {
namespace ArithExpr {
	using RG::Uni::DataStructures::List;

	enum TokenType {
		tok_Unknown,

		tok_Operator_1,
		tok_Operator_2,
		tok_Operator_3,

		tok_Operand_Literal,
		tok_Operand_Binding,

		tok_ParenthesisOpen,
		tok_ParenthesisClose
	};

	class Token {
	public:
		TokenType type;
		char symbol[128];

		Token() : type(tok_Unknown) {
			symbol[0] = '\0';
		}
		Token( TokenType t, const char* s ) : type(t) {
			//printf("token(%d, '%s')\n", t, s);
			strcpy( symbol, s );
		}
	};

	class Tokenizer {
	public:
		Tokenizer();
		virtual ~Tokenizer();

		List<Token>* tokenize( List<char>* chars ) const;

		static Token parse_literal( List<char>* chars );
		static Token parse_binding( List<char>* chars );

		static bool is_alpha( char ch );
		static bool is_digit( char ch );
	};
}}}


#endif // _RG_Uni_AirthExpr_Tokenizer_h
