{$mode objfpc}

unit List;
	interface
		type generic TListNode<_T> = class
			Data: _T;

			Next: TListNode;
			Prev: TListNode;
		public
			constructor Create( d : _T );
			destructor Free;

			procedure SetNext( n : TListNode );
			procedure SetPrev( p : TListNode );

			procedure SetHead;
			procedure SetTail;
		end;

		type generic TList<_T> = class
			type TNode = specialize TListNode<_T>;
		private
			Head: TNode;
			Tail: TNode;
		public
			constructor Create;
			destructor Free;

			procedure PushBack ( Item : _T );
			procedure PushFront( Item : _T );

			{
			function PopBack ( Item : _T );
			function PopFront( Item : _T );
			}

			function CreateNode( Item : _T) : TNode;
		end;


	implementation
		constructor TList.Create;
		begin
			WriteLn('TList<_>.Create');
			Head := nil;
			Tail := nil;
		end;

		destructor TList.Free;
		begin
			WriteLn('TList<_>.Free');
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
			if Head = nil then begin
				Head := newNode;
				Tail := newNode;
			end else begin
				Tail.SetNext( newNode );
				Tail := newNode;
			end;
		end;

		procedure TList.PushFront ( Item : _T );
		var newNode : TNode;
		begin
			newNode := CreateNode( Item );
			if Tail = nil then begin
				Head := newNode;
				Tail := newNode;
			end else begin
				Head.SetPrev( newNode );
				Head := newNode;
			end;
		end;


		{ * * * * * * * * * * * * * * * * * * * * * }

		constructor TListNode.Create ( d : _T );
		begin
			WriteLn('TListNode.Create');
			Data := d;
			Next := nil;
			Prev := nil;
		end;

		destructor TListNode.Free;
		begin
			WriteLn('TListNode.Free');
		end;

		procedure TListNode.SetNext( n : TListNode );
		begin
			Next := n;
			n.Prev := self;
		end;

		procedure TListNode.SetPrev( p : TListNode );
		begin
			Prev := p;
			p.Next := self;
		end;

		procedure TListNode.SetHead;
		begin
			Prev := nil;
		end;

		procedure TListNode.SetTail;
		begin
			Next := nil;
		end;

	end.
