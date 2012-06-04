#ifndef _RG_Uni_ArithExpr_Expression_h
#define _RG_Uni_ArithExpr_Expression_h

namespace RG {
namespace Uni {
namespace ArithExpr {

	class IOperand {
	public:
		IOperand();
		virtual ~IOperand();
	};

	class Value {
	private:
		char _chars[128];
	public:
		Value( const char* chars );
		virtual ~Value();
	};

	class Expression : public IOperand {
	public:
		enum Operation {
			op_unknown,

			op_add,
			op_subtract,

			op_multiply,
			op_divide,

			op_power
		};
	private:
		Operation _Operation;
		IOperand* _LOperand;
		IOperand* _ROperand;
	public:
		
		Expression();
		virtual ~Expression();

		void set_operation( Operation o );
		void set_left_operand( IOperand* o );
		void set_right_operand( IOperand* o );
	};
 }}}

#endif // _RG_Uni_ArithExpr_Expression_h
