{$mode objfpc}

program Parser;
	uses dumbBinaryTree;
	type MapIntToString = specialize dumbBinaryTree.TBTree<integer, string>;
	var i2s : MapIntToString;
	var i: integer;
	begin
		i2s := MapIntToString.Create;
		for i := 1 to 64 do begin
			i2s.SetAt( random(1024), 'something' );
		end;

		i2s.PrintLevels;

	end.