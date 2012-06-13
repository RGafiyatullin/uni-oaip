{$mode objfpc}

program MyPas;
	uses List, Tokenizer;
	var toker: Tokenizer.TTokenizer;
	var toks : Tokenizer.TTokenList;
	begin
		toker := Tokenizer.TTokenizer.CreateFromArgs;
		toks := toker.GetTokens;
		if toks = nil then begin
			WriteLn('Tokenizing failed. Quitting...');
			halt;
		end;
		WriteLn('Tokenizing complete. Tokens count: ', toks.GetCount);
	end.