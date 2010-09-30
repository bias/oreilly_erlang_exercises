-module(index).

-export([index/1,processFile/1,prettyEntry/1]). 

index(File) ->
  ets:new(indexTable, [ordered_set, named_table]),
  processFile(File),
  prettyIndex().

processFile(File) ->
  {ok,IoDevice} = file:open(File,[read]),
  processLines(IoDevice,1).

processLines(IoDevice,N) ->
  case io:get_line(IoDevice,"") of
    eof ->
      ok;
    Line ->
      processLine(Line,N),
      processLines(IoDevice,N+1)
  end.
					 
-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

processLine(Line,N) ->
  case regexp:split(Line,?Punctuation) of
    {ok,Words} ->
      processWords(Words,N) ;
    _ -> []
  end.

processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      if
        length(Word) > 3 ->
          Normalise = string:to_lower(Word),
          ets:insert(indexTable,{{Normalise , N}});
        true -> ok
      end,
      processWords(Rest,N)
  end.

prettyIndex() ->
  case ets:first(indexTable) of
    '$end_of_table' ->
      ok;
    First  ->
      case First of
        {Word, N} ->
          IndexEntry = {Word, [N]}
      end,
      prettyIndexNext(First,IndexEntry)
  end.


prettyIndexNext(Entry,{Word, Lines}=IndexEntry) ->
  Next = ets:next(indexTable,Entry),
  case Next of
    '$end_of_table' ->
      prettyEntry(IndexEntry);
    {NextWord, M}  ->
      if
        NextWord == Word ->
          prettyIndexNext(Next,{Word, [M|Lines]});
        true ->
          prettyEntry(IndexEntry),
          prettyIndexNext(Next,{NextWord, [M]})
      end
  end.

prettyEntry({Word, Lines}=IndexEntry) ->
	io:format("~p ", [Word]),
	prettyList(accumulate(Lines)),
	io:format("~n"),
    ok.
    
accumulate(L) ->
	accumulate(lists:sort(L), []).
accumulate([], P) -> 
	lists:reverse(P);
accumulate([H|T], [{H}|_]=PrettyList) ->
	accumulate(T, PrettyList);
accumulate([H|T], [{PH}|PT]) when H == PH + 1 ->
	accumulate(T, [{PH,H}|PT]);
accumulate([H|T], [{_,H}|_]=PrettyList) ->
	accumulate(T, PrettyList);
accumulate([H|T], [{PH,PHR}|PT]) when H == PHR + 1 ->
	accumulate(T, [{PH,H}|PT]);
accumulate([H|T], P) ->
	accumulate(T, [{H}|P]).
	
prettyList([{X}|T]) ->
	io:format("~p", [X]),
	prettyList(T,comma);
prettyList([{X,Y}|T]) ->
	io:format("~p-~p", [X,Y]),
	prettyList(T,comma).
prettyList([],comma) ->
	io:format(".");
prettyList([{X}|T],comma) ->
	io:format(",~p", [X]),
	prettyList(T,comma);
prettyList([{X,Y}|T],comma) ->
	io:format(",~p-~p", [X,Y]),
	prettyList(T,comma).

pad(N, Word) -> ok.
	 
