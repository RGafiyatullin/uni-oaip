{$mode objfpc}

unit AST;
	interface
		uses List, Tokenizer;

		type TToken = Tokenizer.TToken;
		type TTokenList = Tokenizer.TTokenList;
		type TTokenListIterator = Tokenizer.TTokenListIterator;
		type TType = Tokenizer.TType;

		type EAST = class
		end;

		type TNode = class
		public
			procedure PrintParenthesised; virtual;
			procedure PrintReversePolish; virtual;
		end;

		type TValueNode = class(TNode)
		private
			_Token : TToken;
		public
			constructor CreateFromToken( t: TToken );

			procedure PrintParenthesised; override;
			procedure PrintReversePolish; override;
		end;

		type TBinaryOperationNode = class(TNode)
		private
			_LeftOperand : TNode;
			_RightOperand : TNode;
			_Operation : TToken;
		public
			constructor CreateFromToken( t: TToken );
			procedure SetLeftOperand( n: TNode );
			procedure SetRightOperand( n: TNode );

			procedure PrintParenthesised; override;
			procedure PrintReversePolish; override;
		end;

		type TTree = class
		private
			_Root : TNode;

			function TreeFromTokenList( tli : TTokenListIterator; ctx : TNode ) : TNode;
			function GetRightOperand( priority : integer; tli : TTokenListIterator ) : TNode;
			function FetchParenthesisedSubexpression( tli: TTokenListIterator ) : TTokenList;
		public
			constructor CreateFromTokenList( toks : TTokenList );

			procedure PrintParenthesised;
			procedure PrintReversePolish;
		end;

	implementation
		procedure TNode.PrintParenthesised;
		begin
			WriteLn('PURE VIRTUAL CALL!!! WTF!!!');
		end;

		procedure TNode.PrintReversePolish;
		begin
			WriteLn('PURE VIRTUAL CALL!!! WTF!!!');
		end;

		procedure TTree.PrintParenthesised;
		begin
			Write('Parenthesised infix form: ');
			_Root.PrintParenthesised;
			WriteLn('');
		end;

		procedure TTree.PrintReversePolish;
		begin
			Write('Reverse polish form:     ');
			_Root.PrintReversePolish;
			WriteLn('');
		end;

		procedure TValueNode.PrintParenthesised;
		begin
			Write(' ', _Token.Original, ' ');
		end;
		procedure TValueNode.PrintReversePolish;
		begin
			Write(' ', _Token.Original, ' ');
		end;

		procedure TBinaryOperationNode.PrintParenthesised;
		begin
			Write('(');
			_LeftOperand.PrintParenthesised;
			Write(' ', _Operation.Original, ' ');
			_RightOperand.PrintParenthesised;
			Write(')');
		end;

		procedure TBinaryOperationNode.PrintReversePolish;
		begin
			_LeftOperand.PrintReversePolish;
			_RightOperand.PrintReversePolish;
			Write(' ', _Operation.Original, ' ');
		end;

		constructor TValueNode.CreateFromToken( t: TToken );
		begin
			_Token := t;
		end;

		constructor TBinaryOperationNode.CreateFromToken( t: TToken );
		begin
			_Operation := t;
		end;

		procedure TBinaryOperationNode.SetLeftOperand( n: TNode );
		begin
			_LeftOperand := n;
		end;

		procedure TBinaryOperationNode.SetRightOperand( n: TNode );
		begin
			_RightOperand := n;
		end;

		constructor TTree.CreateFromTokenList( toks : TTokenList );
		var tli : TTokenListIterator;
		begin
			tli := toks.GetConstIterator;
			_Root := TreeFromTokenList( tli, nil );
		end;

		function TTree.GetRightOperand( priority : integer; tli : TTokenListIterator ) : TNode;
		var firstTok: TToken;
		var secondTok: TToken;
		var subExpression: TTokenList;
		var subExpressionIterator: TTokenListIterator;
		begin
			firstTok := tli.GetNext;
			case firstTok.TokenType of
				TType.literal, TType.identifier: begin
					if tli.HasNext then begin
						secondTok := tli.Peek;
						
						if secondTok.Priority > priority then begin
							tli.StepBack;
							Result := TreeFromTokenList( tli, nil );
						end else begin
							Result := TValueNode.CreateFromToken( firstTok );
						end;
					end else begin
						Result := TValueNode.CreateFromToken( firstTok );
					end;
				end;
				TType.parenthesis_l: begin
					tli.StepBack;
					subExpression := FetchParenthesisedSubexpression( tli );
					subExpressionIterator := subExpression.GetConstIterator;
					Result := TreeFromTokenList( subExpression.GetConstIterator, nil );
				end;
				else begin
					WriteLn('Expected either literal or identifier. Got ', firstTok.TokenType, ' at ', firstTok.Position);
					raise EAST.Create;
				end;
			end
		end;

		function TTree.FetchParenthesisedSubexpression( tli: TTokenListIterator ) : TTokenList;
		var depth: integer;
		var toks : TTokenList;
		var tok : TToken;
		begin
			if tli.Peek.TokenType <> TType.parenthesis_l then begin
				WriteLn('Expected parenthesis_l. Got ', tli.Peek.TokenType, ' at ', tli.Peek.Position);
				raise EAST.Create;
			end;
			tli.GetNext;
			depth := 1;
			toks := TTokenList.Create;
			while depth > 0 do begin
				tok := tli.GetNext;
				case tok.TokenType of
					TType.parenthesis_l: begin
						depth := depth + 1;
					end;
					TType.parenthesis_r: begin
						depth := depth - 1;
					end;
				end;
				toks.PushBack( tok );
			end;
			toks.PopBack;
			//WriteLn('Fetched ', toks.GetCount, ' tokens into subexpression');
			Result := toks;
		end;

		function TTree.TreeFromTokenList( tli : TTokenListIterator; ctx : TNode ) : TNode;
		var leftOperand: TNode;
		var rightOperand: TNode;
		var binaryOperation: TBinaryOperationNode;
		var opToken: TToken;
		var subExpression: TTokenList;
		var subExpressionIterator: TTokenListIterator;
		begin
			if ctx = nil then begin (* empty tree. Next token expected: either literal or identifier *)
				case tli.Peek.TokenType of
					TType.literal, TType.identifier: begin
							ctx := TValueNode.CreateFromToken( tli.GetNext );
							Result := TreeFromTokenList( tli, ctx );
						end;
					TType.parenthesis_l: begin
						subExpression := FetchParenthesisedSubexpression( tli );
						subExpressionIterator := subExpression.GetConstIterator;
						Result := TreeFromTokenList( tli, TreeFromTokenList( subExpression.GetConstIterator, nil ) );
					end;
					else
						WriteLn('Expected either literal or identifier. Got ', tli.Peek.TokenType, ' at ', tli.Peek.Position);
						raise EAST.Create;
				end;
			end else begin
				case tli.Peek.TokenType of
					TType.op_mult, TType.op_div,
					TType.op_plus, TType.op_minus,
					TType.op_pow : begin
						leftOperand := ctx;
						opToken := tli.GetNext;
						
						binaryOperation := TBinaryOperationNode.CreateFromToken( opToken );
						binaryOperation.SetLeftOperand( leftOperand );

						rightOperand := GetRightOperand( opToken.Priority, tli );
						binaryOperation.SetRightOperand( rightOperand );
						
						ctx := binaryOperation;

						if tli.HasNext then begin
							Result := TreeFromTokenList( tli, ctx );
						end else begin
							Result := ctx;
						end;
					end;
				end;
			end;
		end;
end.
