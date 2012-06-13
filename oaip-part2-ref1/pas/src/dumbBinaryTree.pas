{$mode objfpc}

unit dumbBinaryTree;
	interface
		type ENotFound = class
		end;

		type generic TBTreeNode<_K, _V> = class
		private
			_Key : _K;
			_Val : _V;
			
			_Left : TBTreeNode;
			_Right : TBTreeNode;
		public
			constructor CreateFromKV( k: _K; v: _V );
			function GetValue : _V;
			procedure SetValue( v : _V );
			procedure Inject( n : TBTreeNode );
			function Find( k : _K ) : TBTreeNode;
			function PrintLevel( level : integer; myLevel: integer ) : integer;
		end;

		type generic TBTree<_K, _V> = class
			type TNode = specialize TBTreeNode<_K, _V>;
		private
			_Root: TNode;
		public
			constructor Create;
			function Exists( key : _K ) : boolean;
			function GetAt( key: _K ) : _V;
			procedure SetAt( key: _K; val: _V );

			procedure PrintLevels;
		end;
	implementation
		constructor TBTreeNode.CreateFromKV( k: _K; v: _V );
		begin
			_Key := k;
			_Val := v;
			_Left := nil;
			_Right := nil;
		end;

		function TBTreeNode.PrintLevel( level : integer; myLevel: integer ) : integer;
		begin
			if myLevel = level then begin
				Write(' {', _Key, ': ', _Val, '} ');
				Result := 1;
			end else begin
				Result := 0;
				if _Left <> nil then begin
					Result := Result + _Left.PrintLevel( level, myLevel + 1 );
				end;
				if _Right <> nil then begin
					Result := Result + _Right.PrintLevel( level, myLevel + 1 );
				end;
			end;
		end;

		procedure TBTree.PrintLevels;
		var level: integer;
		var count: integer;
		begin
			level := 1;
			while true do begin
				count := _Root.PrintLevel( level, 1 );
				level := level + 1;
				WriteLn();
				WriteLn('Level #', level, ' => ', count, ' nodes');

				if count = 0 then halt;
			end;
		end;

		function TBTreeNode.GetValue : _V;
		begin
			Result := _Val;
		end;

		procedure TBTreeNode.SetValue( v : _V );
		begin
			_Val := v;
		end;
		
		function TBTreeNode.Find( k : _K ) : TBTreeNode;
		begin
			if k = _Key then begin
				Result := self;
			end else if k < _Key then begin
				if _Left = nil then begin
					Result := nil;
				end else begin
					_Left.Find( k );
				end;
			end else if k > _Key then begin
				if _Right = nil then begin
					Result := nil;
				end else begin
					_Right.Find( k );
				end;
			end;
		end;

		procedure TBTreeNode.Inject( n : TBTreeNode );
		begin
			if n._Key = _Key then begin
				_Val := n._Val;
				n.Free;
			end else if n._Key < _Key then begin
				if _Left = nil then begin
					_Left := n;
				end else begin
					_Left.Inject( n );
				end;
			end else if n._Key > _Key then begin
				if _Right = nil then begin
					_Right := n;
				end else begin
					_Right.Inject( n );
				end;
			end;
		end;

		constructor TBTree.Create;
		begin
			_Root := nil;
		end;

		function TBTree.Exists( key: _K ) : boolean;
		begin
			if _Root = nil then begin
				Result := false;
			end else begin
				Result := _Root.Find( key ) <> nil;
			end;
		end;

		function TBTree.GetAt( key: _K ) : _V;
		begin
			if not Exists( key ) then begin
				raise ENotFound.Create;
			end else begin
				_Root.Find( key ).GEtValue;
			end;
		end;

		procedure TBTree.SetAt( key: _K; val: _V );
		begin
			if _Root = nil then begin
				_Root := TNode.CreateFromKV( key, val );
			end else begin
				_Root.Inject( TNode.CreateFromKV( key, val ) );
			end;
		end;
end.
