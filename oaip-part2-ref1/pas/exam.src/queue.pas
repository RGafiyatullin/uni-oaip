{$mode objfpc}

unit Queue;
	interface
		type EEmpty = class
		end;

		type generic TQueueSingleNode<_T> = class
		private
			_Value: _T;
			_Next : TQueueSingleNode;
		public
			constructor CreateFromValue( item : _T );
			procedure SetNext( node : TQueueSingleNode );
			function GetNext : TQueueSingleNode;
			function GetValue : _T;
		end;

		type generic TQueueSingle<_T> = class
			type TNode = specialize TQueueSingleNode<_T>;
		private
			_Head: TNode;
			_Tail: TNode;
			_Count: integer;
		public
			constructor Create;
			destructor Free;

			procedure Enqueue( item : _T );
			function Dequeue : _T;
			function GetCount : integer;

		end;


	implementation
		constructor TQueueSingleNode.CreateFromValue( item : _T );
		begin
			_Value := item;
			_Next := nil;
		end;

		constructor TQueueSingle.Create;
		begin
			_Head := nil;
			_Tail := nil;
			_Count := 0;
		end;

		destructor TQueueSingle.Free;
		begin
			while _Head <> nil do begin
				Dequeue;
			end;
		end;

		procedure TQueueSingleNode.SetNext( node : TQueueSingleNode );
		begin
			_Next := node;
		end;

		function TQueueSingleNode.GetNext : TQueueSingleNode;
		begin
			Result := _Next;
		end;

		function TQueueSingle.GetCount : integer;
		begin
			Result := _Count;
		end;

		function TQueueSingleNode.GetValue : _T;
		begin
			Result := _Value;
		end;

		procedure TQueueSingle.Enqueue( item : _T );
		var appendee: TNode;
		begin
			if _Head = nil then begin
				_Head := TNode.CreateFromValue( item );
				_Tail := _Head;
			end else begin
				appendee := TNode.CreateFromValue( item );
				_Tail.SetNext( appendee );
				_Tail := appendee;
			end;
			_Count := _Count + 1;
		end;

		function TQueueSingle.Dequeue : _T;
		var exHead: TNode;
		begin
			if _Head = nil then begin
				raise EEmpty.Create;
			end else begin
				exHead := _Head;
				_Head := _Head.GetNext;
				Result := exHead.GetValue;
				exHead.Free;
				_Count := _Count - 1;

				if _Head = nil then
					_Tail := nil;
			end;
		end;

	end.





