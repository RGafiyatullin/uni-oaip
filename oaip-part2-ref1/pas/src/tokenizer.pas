{$mode objfpc}

unit Tokenizer;
	interface
		uses List, SysUtils;

		type TCharList = specialize List.TList<char>;
		type TCharListIterator = specialize List.TConstIterator<char>;

		type TStringList = specialize List.TList<string>;
		type TStringListIterator = specialize List.TConstIterator<string>;

		type TType = (
			identifier, literal,
			parenthesis_l, parenthesis_r, 
			op_plus, op_minus, 
			op_div, op_mult, 
			op_pow
		);

		type TToken = class
		public
			TokenType : TType;
			Position : integer;
			Original : string;
		end;

		type TTokenList = specialize List.TList<TToken>;
		
		type TTokenizer = class
		private 
			_tokens : TTokenList;

			function CharsFromArgs : TCharList;
			function ProcessChars( chars : TCharList ) : TTokenList;

			procedure ProcessNumericLiteral( chIt : TCharListIterator; toks : TTokenList );
			procedure ProcessIdentifier( chIt : TCharListIterator; toks : TTokenList );

			function IsDigit( ch : char ) : boolean;
			function IsAlpha( ch : char ) : boolean;
		public
			constructor CreateFromArgs;

			function GetTokens : TTokenList;
		end;

	implementation

		constructor TTokenizer.CreateFromArgs;
		var chars: TCharList;
		begin
			chars := CharsFromArgs;
			_tokens := ProcessChars( chars );
		end;

		function TTokenizer.CharsFromArgs : TCharList;
		var argi: integer;
		var ci: integer;
		var arg: string;
		var argl: integer;
		var chars: TCharList;
		begin
			chars := TCharList.Create;
			for argi := 1 to ParamCount do begin
				arg := ParamStr(argi);
				argl := length(arg);
				for ci := 1 to argl do begin
					chars.PushBack( arg[ci] );
				end;
				chars.PushBack( ' ' );
			end;
			Result := chars;
		end;

		function TTokenizer.IsAlpha( ch : char ) : boolean;
		begin
			case ch of
				'A'..'Z','a'..'z','_': begin
					Result := true;
				end;
				else begin
					Result := false;
				end
			end;
		end;

		function TTokenizer.IsDigit( ch : char ) : boolean;
		begin
			case ch of
				'0','1'..'9': begin
					Result := true;
				end;
				else begin
					Result := false;
				end
			end;
		end;

		procedure TTokenizer.ProcessIdentifier( chIt : TCharListIterator; toks : TTokenList );
		var token : TToken;
		begin
			WriteLn('Processing identifier at ', chIt.GetPosition);
			token := TToken.Create;
			token.TokenType := TType.identifier;
			token.Position := chIt.GetPosition;
			token.Original := chIt.GetNext;
			while chIt.HasNext and ( IsDigit( chIt.Peek ) or IsAlpha( chIt.Peek ) ) do begin
				token.Original := token.Original + chIt.GetNext;
			end;

			toks.PushBack( token );
		end;

		procedure TTokenizer.ProcessNumericLiteral( chIt : TCharListIterator; toks : TTokenList );
		var token : TToken;
		begin
			WriteLn('Processing numeric literal at ', chIt.GetPosition);
			token := TToken.Create;
			token.TokenType := TType.literal;
			token.Position := chIt.GetPosition;
			token.Original := '';
			while chIt.HasNext and IsDigit( chIt.Peek ) do begin
				token.Original := token.Original + chIt.GetNext;
			end;

			toks.PushBack( token );
		end;

		function TTokenizer.ProcessChars( chars : TCharList ) : TTokenList;
		var toks: TTokenList;
		var chIt: TCharListIterator;
		var currentChar: char;
		var opToken: TToken; 
		begin
			toks := TTokenList.Create;
			chIt := chars.GetConstIterator;

			while chIt.HasNext do begin
				currentChar := chIt.Peek;
				case currentChar of
					'1'..'9','0': ProcessNumericLiteral( chIt, toks );
					'a'..'z','A'..'Z': ProcessIdentifier( chIt, toks );
					'*','/','+','-','^': begin
						WriteLn('Processing operation token at ', chIt.GetPosition);
						case chIt.Peek of
							'*': begin
								opToken := TToken.Create;
								opToken.Position := chIt.GetPosition;
								opToken.TokenType := TType.op_mult;
								opToken.Original := chIt.GetNext;
								toks.PushBack(opToken);
							end; 
							'/': begin
								opToken := TToken.Create;
								opToken.Position := chIt.GetPosition;
								opToken.TokenType := TType.op_div;
								opToken.Original := chIt.GetNext;
								toks.PushBack(opToken);
							end; 
							'-': begin
								opToken := TToken.Create;
								opToken.Position := chIt.GetPosition;
								opToken.TokenType := TType.op_minus;
								opToken.Original := chIt.GetNext;
								toks.PushBack(opToken);
							end;
							'+': begin
								opToken := TToken.Create;
								opToken.Position := chIt.GetPosition;
								opToken.TokenType := TType.op_plus;
								opToken.Original := chIt.GetNext;
								toks.PushBack(opToken);
							end;
							'^': begin
								opToken := TToken.Create;
								opToken.Position := chIt.GetPosition;
								opToken.TokenType := TType.op_pow;
								opToken.Original := chIt.GetNext;
								toks.PushBack(opToken);
							end;
						end
					end;
					' ': chIt.GetNext;
					else begin
						WriteLn('Unexpected char ''', currentChar, ''' at position ', chIt.GetPosition);
						Result := nil;
						break;
					end;
				end;
			end;
			Result := toks;
		end;

		function TTokenizer.GetTokens : TTokenList;
		var toks : TTokenList;
		begin
			Result := _tokens;
		end;
end.

