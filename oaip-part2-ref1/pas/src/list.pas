{$mode objfpc}

unit List;
	interface
		type EListEmpty = class
		end;

		type generic TListNode<_T> = class
			_Data: _T;

			_Next: TListNode;
			_Prev: TListNode;
		public
			constructor Create( d : _T );
			destructor Free;

			procedure SetNext( n : TListNode );
			procedure SetPrev( p : TListNode );

			function GetNext : TListNode;
			function GetPrev : TListNode;

			procedure SetHead;
			procedure SetTail;

			function GetData : _T;
		end;

		type generic TConstIterator<_T> = class
			type TNode = specialize TListNode<_T>;
		private
			_currentNode : TNode;
			_position : integer;
		public
			constructor CreateFromNode( n : TNode );
			function HasNext : boolean;
			function GetNext : _T;
			function Peek : _T;
			function StepBack : boolean;
			function GetPosition : integer;
		end;

		type generic TList<_T> = class
			type TNode = specialize TListNode<_T>;
			type TCIterator = specialize TConstIterator<_T>;
		private
			_Head: TNode;
			_Tail: TNode;
			_Count: integer;
		public
			constructor Create;
			destructor Free;

			procedure PushBack ( Item : _T );
			procedure PushFront( Item : _T );

			function PopFront : _T ;
			function PopBack : _T ;

			function GetCount : integer;
			
			function GetConstIterator : TCIterator;

		private
			function CreateNode( Item : _T) : TNode;
		end;


	implementation
		constructor TConstIterator.CreateFromNode( n : TNode );
		begin
			_currentNode := n;
			_position := 1;
		end;

		function TConstIterator.HasNext : boolean;
		begin
			// WriteLn('Iterator.HasNext -> ', _currentNode <> nil);
			Result := _currentNode <> nil;
		end;

		function TConstIterator.GetNext : _T;
		begin
			Result := _currentNode.GetData;
			_currentNode := _currentNode.GetNext;
			_position := _position + 1;
		end;

		function TConstIterator.Peek : _T;
		begin
			Result := _currentNode.GetData;
		end;

		function TConstIterator.GetPosition : integer;
		begin
			Result := _position;
		end;

		function TConstIterator.StepBack : boolean;
		begin
			if _currentNode.GetPrev <> nil then begin
				_currentNode := _currentNode.GetPrev;
				_position := _position - 1;
				Result := true;
			end else begin
				Result := false;
			end;
		end;

		constructor TList.Create;
		begin
			_Count := 0;
			_Head := nil;
			_Tail := nil;
		end;

		destructor TList.Free;
		var node : TNode;
		begin
			while _Head <> nil do begin
				node := _Head;
				_Head := _Head.GetNext;
				node.Free;
			end
		end;

		function TList.GetConstIterator : TCIterator;
		begin
			Result := TCIterator.CreateFromNode( _Head );
		end;

		function TList.CreateNode( Item : _T ) : TNode;
		var node : TNode;
		begin
			node := TNode.Create ( Item );
			Result := node;
		end;

		procedure TList.PushBack ( Item : _T );
		var newNode : TNode;
		begin
			newNode := CreateNode( Item );
			if _Head = nil then begin
				_Head := newNode;
				_Tail := newNode;
			end else begin
				_Tail.SetNext( newNode );
				_Tail := newNode;
			end;
			_Count := _Count + 1;
		end;

		procedure TList.PushFront ( Item : _T );
		var newNode : TNode;
		begin
			newNode := CreateNode( Item );
			if _Tail = nil then begin
				_Head := newNode;
				_Tail := newNode;
			end else begin
				_Head.SetPrev( newNode );
				_Head := newNode;
			end;
			_Count := _Count + 1;
		end;

		function TList.PopFront : _T;
		var node : TNode;
		begin
			if _Count = 0 then
				raise EListEmpty.Create
			else begin
				if _Head = _Tail then begin
					node := _Head;
					_Head := nil;
					_Tail := nil;
				end else begin
					node := _Head;
					_Head := _Head.GetNext;
					_Head.SetHead;
				end;
				Result := node.GetData;
				_Count := _Count - 1;
				node.Free;
			end
		end;

		function TList.PopBack : _T;
		var node : TNode;
		begin
			if _Count = 0 then
				raise EListEmpty.Create
			else begin
				if _Head = _Tail then begin
					node := _Head;
					_Head := nil;
					_Tail := nil;
				end else begin
					node := _Tail;
					_Tail := _Tail.GetPrev;
					_Tail.SetTail;
				end;
				Result := node.GetData;
				_Count := _Count - 1;
				node.Free;
			end
		end;

		function TList.GetCount : integer;
		begin
			Result := _Count;
		end;

		{ * * * * * * * * * * * * * * * * * * * * * }

		constructor TListNode.Create ( d : _T );
		begin
			_Data := d;
			_Next := nil;
			_Prev := nil;
		end;

		destructor TListNode.Free;
		begin
		end;

		procedure TListNode.SetNext( n : TListNode );
		begin
			_Next := n;
			n._Prev := self;
		end;

		procedure TListNode.SetPrev( p : TListNode );
		begin
			_Prev := p;
			p._Next := self;
		end;

		procedure TListNode.SetHead;
		begin
			_Prev := nil;
		end;

		procedure TListNode.SetTail;
		begin
			_Next := nil;
		end;

		function TListNode.GetNext : TListNode;
		begin
			Result := _Next;
		end;

		function TListNode.GetPrev : TListNode;
		begin
			Result := _Prev;
		end;

		function TListNode.GetData : _T;
		begin
			Result := _Data;
		end;

	end.
