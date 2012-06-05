#include "Expression.h"

#include <cstddef>
#include <string.h>

namespace RG {
namespace Uni {
namespace ArithExpr {
	
	IOperand::IOperand() {}
	IOperand::~IOperand() {}

	Value::Value( const char* chars ) {
		strcpy( _chars, chars );
	}
	Value::~Value() {}

	Expression::Expression() : 
		_Operation(op_unknown), 
		_LOperand(NULL), 
		_ROperand(NULL)
	{}
	Expression::~Expression() {}
	
	void Expression::set_operation( Operation o ) {
		_Operation = o;
	}
	void Expression::set_left_operand( IOperand* o ) {
		_LOperand = o;
	}
	void Expression::set_right_operand( IOperand* o ) {
		_ROperand = o;
	}
	
}}}
