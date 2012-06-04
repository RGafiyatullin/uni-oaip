
#include "Tokenizer.h"

namespace RG {
namespace Uni {
namespace ArithExpr {
	using RG::Uni::DataStructures::List;

	Tokenizer::Tokenizer() {}
	Tokenizer::~Tokenizer() {}

	List<Token>* Tokenizer::tokenize(List<char>* input) const {
		List<Token>* tokens = new List<Token>();
		while ( ! input->is_empty() ) {
			char ch = input->pop_front();

			switch (ch) {
				case '(': tokens->push_back( Token(tok_ParenthesisOpen, "(") ); break;
				case ')': tokens->push_back( Token(tok_ParenthesisClose, ")") ); break;
				case '+': tokens->push_back( Token(tok_Operator_3, "+") ); break;
				case '-': tokens->push_back( Token(tok_Operator_3, "-") ); break;
				case '*': tokens->push_back( Token(tok_Operator_2, "*" ) ); break;
				case '/': tokens->push_back( Token(tok_Operator_2, "/" ) ); break;
				case '^': tokens->push_back( Token(tok_Operator_1, "^") ); break;
				case ' ': continue;
				default:
					if ( is_digit( ch ) ) {
						input->push_front( ch );
						tokens->push_back( parse_literal( input ) );
					}
					else if ( is_alpha( ch ) ) {
						input->push_front( ch );
						tokens->push_back( parse_binding( input ) );
					}
			}
		}
		return tokens;
	}

	bool Tokenizer::is_alpha( char ch ) {
		return ( ch >= 'a' and ch <= 'z' ) or ( ch >= 'A' and ch <= 'Z' );
	}
	bool Tokenizer::is_digit( char ch ) {
		return ch >= '0' and ch <= '9';
	}

	Token Tokenizer::parse_literal( List<char>* chars ) {
		List<char> acc;
		do {
			char ch = chars->pop_front();
			if ( is_digit( ch ) ) {
				acc.push_back( ch );
			}
			else {
				chars->push_front( ch );
				break;
			}
		} while ( ! chars->is_empty() );
		char buf[128];
		acc.to_buffer(buf);
		return Token( tok_Operand_Literal, buf );
	}
	Token Tokenizer::parse_binding( List<char>* chars ) {
		List<char> acc;
		do {
			char ch = chars->pop_front();
			if ( is_digit( ch ) || is_alpha( ch ) ) {
				acc.push_back( ch );
			}
			else {
				chars->push_front( ch );
				break;
			}
		} while ( ! chars->is_empty() );
		char buf[128];
		acc.to_buffer(buf);
		return Token( tok_Operand_Binding, buf );
	}


}}}

