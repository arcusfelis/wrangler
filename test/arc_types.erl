-module(arc_types).
-export([sublist/1,
         sublist/2,
         sublist1/1,
         exect_weighted_union/1,
         unique_key/2,
         unique_list/1,
         range_list/3]).

-include_lib("proper/include/proper.hrl").

%% @doc Make a sublist
-spec sublist(ListOfTypes :: [proper_types:type()]) ->
    proper_types:type().
sublist(ListOfTypes) ->
    Len = length(ListOfTypes),
    Random = shuffle(ListOfTypes),
    ?LET(NewLen, range(0, Len), lists:sublist(Random, NewLen)).

sublist(MaxLen, ListOfTypes) ->
    Len = min(length(ListOfTypes), MaxLen),
    Random = ?LET(X, ListOfTypes, shuffle(X)),
    ?LET({NewLen, RandomValue},
         {range(0, Len), ListOfTypes},
         lists:sublist(RandomValue, NewLen)).

range_list(MinLen, MaxLen, Type) ->
    ?SIZED(Size, range_list(MinLen, MaxLen, Type, Size)).

range_list(MinLen, MaxLen, Type, 0) ->
    lists:duplicate(MinLen, Type);
range_list(MinLen, MaxLen, Type, Size) ->
    Len = min(MinLen + Size, MaxLen),
    ?LET(Values, lists:duplicate(Len, Type),
         ?SHRINK(Values, [shrink_sublist(MinLen, Values, Type)])).

shrink_sublist(MinLen, Values, Type) when length(Values) =< MinLen ->
    [Type || _ <- Values];
shrink_sublist(MinLen, Values, Type) ->
    Sublist = lists:sublist(Values, length(Values)-1),
    ?SHRINK(Sublist, [shrink_sublist(MinLen, Sublist, Type)]).

%% http://roberto-aloi.com/erlang/notes-on-erlang-quickcheck/
sublist1(L0) ->
  ?LET(L, [{E, bool()} || E <- L0], [X || {X, true} <- L]).

%% @doc weighted_union/1 that has number of occurences
%% `Weight' means position
%% `Freq' is exect number of occurences of the raw type
-spec exect_weighted_union(ListOfTypes :: [{MinLen, MaxLen, Weight, RawType}]) ->
    proper_types:type() when
      MinLen :: integer(),
      MaxLen :: integer(),
      Weight :: proper_types:type() | integer(),
      RawType :: proper_types:type().
exect_weighted_union(ListOfTypes) ->
%   [{range_list(MinLen, MaxLen, RawType), Weight} || {MinLen, MaxLen, _Weight, RawType} <- ListOfTypes].
%   Vectors = ?LET(Freqs,
%        [{Freq, {Weight, RawType}} || {Freq, Weight, RawType} <- ListOfTypes],
%        lists:flatten([lists:duplicate(FreqValue, element(2, Delay)) || {FreqValue, Delay} <- Freqs])),
%   Elems = ?LET(VectorsValue, Vectors, lists:flatten(VectorsValue)),
%   ?LET(ElemsValue, Elems, elements(2, lists:keysort(1, ElemsValue))).

    Vectors = ?LET(Freqs,
         [proper_types:tuple([Freq, ?DELAY({Weight, RawType})]) || {Freq, Weight, RawType} <- ListOfTypes],
         [lists:duplicate(FreqValue, ?FORCE(Delay)) || {FreqValue, Delay} <- Freqs]),
    Elems = ?LET(VectorsValue, Vectors, lists:flatten(VectorsValue)),
    ?LET(ElemsValue, Elems, elements(2, lists:keysort(1, ElemsValue))).

elements(N, List) ->
    [element(N, X) || X <- List].

enumerate(L) ->
    enumerate(L, 1).

enumerate([H|T], N) ->
    [{N, H} | enumerate(T, N+1)];
enumerate([], _N) ->
    [].

%% @doc Returns a list in random order.
shuffle(List) when is_list(List) ->
    random:seed(now()),
    WithKey = [ {random:uniform(), X} || X <- List ],
    Sorted  = lists:keysort(1, WithKey),
    elements(2, Sorted).

unique_key(N, ListGen) ->
    ?LET(ListValue, ListGen, shuffle(lists:ukeysort(N, ListValue))).

unique_list(ListGen) ->
    ?LET(ListValue, ListGen, shuffle(lists:usort(ListValue))).
