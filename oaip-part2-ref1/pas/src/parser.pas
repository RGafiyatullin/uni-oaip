{$mode objfpc}

program Parser;
	uses List, Tokenizer, AST;

	var toker: Tokenizer.TTokenizer;
	var toks : Tokenizer.TTokenList;
	var asTree : AST.TTree;
	begin
		toker := Tokenizer.TTokenizer.CreateFromArgs;
		toks := toker.GetTokens;
		if toks = nil then begin
			WriteLn('Tokenizing failed. Quitting...');
			halt;
		end;
		WriteLn('Tokenizing complete. Tokens count: ', toks.GetCount);
		asTree := AST.TTree.CreateFromTokenList( toks );
		asTree.PrintParenthesised;
		asTree.PrintReversePolish;
	end.