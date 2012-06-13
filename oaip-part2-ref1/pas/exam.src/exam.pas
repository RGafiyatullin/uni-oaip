{$mode objfpc}

program Exam;
	uses Queue;
	type QofChars = specialize Queue.TQueueSingle<char>;
	var sInput : string;
	var bKeepGoing : boolean;
	var q : QofChars;
	var i : integer;
	var ch : char;
	var qCount : integer;
	begin
		bKeepGoing := true;
		sInput := '';

		WriteLn('Creating the queue.');
		q := QofChars.Create;

		while bKeepGoing do begin
			ReadLn(sInput);
			
			if sInput[1] = 'a' then begin
				if length(sInput) = 2 then begin
					q.Enqueue(sInput[2]);
					WriteLn('character "', sInput[2], '" has been enqueued.');
				end else begin
					WriteLn('user error: inststruction "a*" must consist of exactly two chars');
				end;
			end;

			if sInput = 'd' then begin
				if q.GetCount = 0 then begin
					WriteLn('user error: Cannot dequeue: the queue is already empty');
				end else begin
					ch := q.Dequeue;
					WriteLn('character "', ch, '" has been dequeued.');
				end;
			end;

			if sInput = 's' then begin
				WriteLn('Current queue''s size is ', q.GetCount);
			end;

			if sInput = 'i' then begin
				WriteLn('Iterating through the queue...');
				qCount := q.GetCount;
				for i := 1 to qCount do begin
					ch := q.Dequeue;
					WriteLn('> ', ch);
					q.Enqueue( ch );
				end;
				WriteLn('Complete!');
			end;

			if sInput = 'q' then begin
				bKeepGoing := false;
				WriteLn('Quitting. Bye!');
			end;
		end;
		WriteLn('Destructing the queue.');
		q.Free;
	end.