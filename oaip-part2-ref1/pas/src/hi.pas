{$mode objfpc}

program MyPas;
	uses List;
	
	type TCharList = specialize List.TList<char>;
	
	var inputChars : TCharList;

	begin
		WriteLn('Hi!');
		inputChars := TCharList.Create;
		inputChars.PushBack('a');
		inputChars.PushBack('b');
		inputChars.PushBack('c');
		inputChars.PushFront('_');
		inputChars.Free;
	end.