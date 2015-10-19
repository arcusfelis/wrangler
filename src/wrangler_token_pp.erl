-module(wrangler_token_pp).
-export([erase_matched/4,
         replace_matched/5,
         append_list_element/5,
         insert_another_list_element_after/6]).

-export([get_delimiter_forms/1]).

%% Search for `MatchAST' in `AnnAST' to delete it.
%% FileFormat = wrangler_misc:file_format(Filename)
%% FileFormat = dos.
%% `MatchAST' should exist in `AnnAST'.
erase_matched(MatchAST, AnnAST, FileFormat, TabWidth) ->
    %% Get range to erase
    {MatchS, MatchE} = get_range(MatchAST),
    SP = get_syntax_path(MatchAST),
    %% Get tokens
    Toks = ast_to_tokens(FileFormat, AnnAST, TabWidth),
    {BeforeToks, _MatchedToks, AfterToks} = partition_by_range(MatchS, MatchE, Toks),
    Toks1 = fix_and_join_toks(SP, BeforeToks, AfterToks),
    tokens_to_ast(Toks1, TabWidth, FileFormat).

tokens_to_ast(Toks, TabWidth, FileFormat) ->
    %% Convert result tokens back to ast
    Bin = toks_to_binary(Toks),
    TmpFilename = tmp_filename(),
    ok = file:write_file(TmpFilename, Bin),
    {ok, {AnnAST1,_Info}} = try
            wrangler_ast_server:parse_annotate_file(TmpFilename, false, [], TabWidth, FileFormat)
        after
            ok = file:delete(TmpFilename)
        end,
    AnnAST1.

%% Search for `MatchAST' in `AnnAST' and replace it with `NewValue'.
%% NewValue is term, not form or tree.
%% `MatchAST' should exist in `AnnAST'.
replace_matched(MatchAST, AnnAST, NewValue, FileFormat, TabWidth) ->
    NewValueToks = term_to_tokens(NewValue, TabWidth, FileFormat),
    replace_matched_tokens(MatchAST, AnnAST, NewValueToks, FileFormat, TabWidth).

replace_matched_tokens(MatchAST, AnnAST, NewValueToks, FileFormat, TabWidth) ->
    %% Get range to erase
    {MatchS, MatchE} = get_range(MatchAST),
    %% Get tokens
    Toks = ast_to_tokens(FileFormat, AnnAST, TabWidth),
    {BeforeToks, _MatchedToks, AfterToks} = partition_by_range(MatchS, MatchE, Toks),
    Toks1 = BeforeToks ++ NewValueToks ++ AfterToks,
    tokens_to_ast(Toks1, TabWidth, FileFormat).

ast_to_tokens(FileFormat, AnnAST, TabWidth) ->
%    Toks = wrangler_misc:get_toks(AnnAST),
%    ast_to_tokens_2(FileFormat, AnnAST, TabWidth, Toks).
%
%ast_to_tokens_2(FileFormat, AnnAST, TabWidth, []) ->
    Str = ast_to_string(FileFormat, AnnAST, TabWidth),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    Toks.

%% MatchAST = a, AnnAST = [a,b,c], Type = list_prefix, BeforeToks = "[", AfterToks = ",b,c]"
fix_and_join_toks(list_prefix, BeforeToks, AfterToks) ->
    %% Try to delete next separator
    case remove_first_list_separator(AfterToks) of
        AfterToks ->
            %% Delete previos separator
            BeforeToks1 = remove_first_list_separator_r(BeforeToks),
            BeforeToks1 ++ AfterToks;
        AfterToks1 ->
            BeforeToks ++ AfterToks1
    end.

%% Go backward
remove_first_list_separator_r(Toks) ->
    ToksR = lists:reverse(Toks),
    ToksR1 = remove_first_list_separator(ToksR),
    lists:reverse(ToksR1).
    
%% Go forward
remove_first_list_separator([{']',_}|_]=Toks) ->
    Toks; %% no more list separators, stop
remove_first_list_separator([{'[',_}|_]=Toks) ->
    Toks; %% no more list separators, stop
remove_first_list_separator([{',',_}|Toks]) ->
    Toks;
remove_first_list_separator([Tok|Toks]) ->
    [Tok|remove_first_list_separator(Toks)].

get_range(Node) ->
     As = wrangler_syntax:get_ann(Node),
     {value, {range, {S, E}}} = lists:keysearch(range, 1, As),
     {S, E}.

get_syntax_path(Node) ->
     As = wrangler_syntax:get_ann(Node),
     {value, {syntax_path, SP}} = lists:keysearch(syntax_path, 1, As),
     SP.

token_loc(T) ->
    case T of
      {_, L, _V} -> L;
      {_, L1} -> L1
    end.

set_token_loc(T, L) ->
    case T of
      {Type, _, V} -> {Type, L, V};
      {Type, _} -> {Type, L}
    end.

partition_by_range(MatchS, MatchE, Toks) ->
    {MatchS_BeforeToks, MatchS_MatchToks, MatchS_AfterToks} = partition_by_location(MatchS, Toks),
    {MatchE_BeforeToks, MatchE_MatchToks, MatchE_AfterToks} = partition_by_location(MatchE, MatchS_AfterToks),
    BeforeToks = MatchS_BeforeToks,
    MatchedToks = MatchS_MatchToks ++ MatchE_BeforeToks ++ MatchE_MatchToks,
    AfterToks = MatchE_AfterToks,
    {BeforeToks, MatchedToks, AfterToks}.

insert_tokens_before(Loc, ElemValueToks, Toks) ->
    {BeforeToks, MatchToks, AfterToks} = partition_by_location(Loc, Toks),
    BeforeToks ++ ElemValueToks ++ MatchToks ++ AfterToks.

insert_tokens_after(Loc, ElemValueToks, Toks) ->
    {BeforeToks, MatchToks, AfterToks} = partition_by_location(Loc, Toks),
    BeforeToks ++ MatchToks ++ ElemValueToks ++ AfterToks.

%% Returns {BeforeToks, MatchedToks, AfterToks}
%% when MatchedToks have exact Loc match.
%% MatchedToks has one or zero elements.
partition_by_location(Loc, Toks) ->
    partition_by_location(Loc, Toks, []).

partition_by_location(Loc, [], Acc) ->
    {lists:reverse(Acc), [], []};
partition_by_location(Loc, [Tok|Toks], Acc) ->
    case token_loc(Tok) of
        Loc ->
            {lists:reverse(Acc), [Tok], Toks};
        TokLoc when TokLoc < Loc ->
            partition_by_location(Loc, Toks, [Tok|Acc]);
        _ -> % TokLoc > Loc
            {lists:reverse(Acc), [], [Tok|Toks]}
    end.

tmp_filename() ->
    string:strip(os:cmd("mktemp"), right, $\n).

term_to_tokens(Value, TabWidth, FileFormat) ->
    Str = pretty_print_term(Value),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    Toks.

%% Pretty-printed `io_lib:format("~p", [Value])'
pretty_print_term(Value) ->
    Str = iolist_to_list(io_lib:format("~p.", [Value])),
    {ok,Forms,_} = erl_scan:string(Str),
    {ok,Exprs} = erl_parse:parse_exprs(Forms),
    Result = erl_prettypr:format(hd(Exprs)),
    iolist_to_list(Result).

%% `ElemValue' is term, not form or tree.
%% `ListTree' is list, in which value should be inserted.
%% This function appends at the end of the list.
%% `Tree' is the whole tree, that should be edited.
append_list_element(ListTree, Tree, ElemValue, FileFormat, TabWidth) ->
    ElemValueToks = term_to_tokens(ElemValue, TabWidth, FileFormat),
    ListTreeElems = wrangler_syntax:list_elements(ListTree),
    append_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, ListTreeElems).

insert_another_list_element_after(ListTree, Tree, ElemValue, PrevElem, FileFormat, TabWidth) ->
    ElemValueToks = term_to_tokens(ElemValue, TabWidth, FileFormat),
    ListTreeElems = wrangler_syntax:list_elements(ListTree),
    add_another_list_element_after(ListTree, Tree, ElemValueToks, PrevElem, FileFormat, TabWidth, ListTreeElems).

append_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, []) ->
    append_first_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth);
append_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, [_|_]=ListTreeElems) ->
    append_another_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, ListTreeElems).

append_first_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth) ->
    %% Get range to erase
    {_MatchS, MatchE} = get_range(ListTree),
    %% Get tokens
    Toks = ast_to_tokens(FileFormat, Tree, TabWidth),
    Toks1 = insert_tokens_before(MatchE, ElemValueToks, Toks),
    tokens_to_ast(Toks1, TabWidth, FileFormat).

append_another_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, ListTreeElems) ->
    LastElem = lists:last(ListTreeElems),
    add_another_list_element_after(ListTree, Tree, ElemValueToks, LastElem, FileFormat, TabWidth, ListTreeElems).

add_another_list_element_after(ListTree, Tree, ElemValueToks, PrevElem, FileFormat, TabWidth, ListTreeElems) ->
    {PrevS,PrevE} = get_range(PrevElem),
    %% Get tokens
    Toks = ast_to_tokens(FileFormat, Tree, TabWidth),
    %% If we have left comment, that we assume that this comment is for
    %% the last argument of the list.
    %% We don't care about multiline comments in the case
    {CommentToks, Toks1} = cut_line_comment_and_leading_whitespaces(PrevE, Toks),
    IndentToks = indent_new_elem(PrevS, Toks, TabWidth),
    ElemValueToks1 = [{',', {1,1}}]
            ++ CommentToks
            ++ get_delimiter_forms(FileFormat)
            ++ IndentToks
            ++ ElemValueToks,
    Toks2 = insert_tokens_after(PrevE, ElemValueToks1, Toks1),
    tokens_to_ast(Toks2, TabWidth, FileFormat).


indent_new_elem(LastS, Toks, TabWidth) ->
    %% Tokens that separate last element from something before
    %% [SomethingBefore,    Last]
    %%                  ^^^^
    LastSpaces = get_whitespaces_between(token_loc(go_left_skipping_comments_and_whitespaces(LastS, Toks)), LastS, Toks),
    indent_new_elem(has_new_lines(LastSpaces), LastS, Toks, LastSpaces, TabWidth).

%% One line version, we should build our own indention
indent_new_elem(false, LastS, Toks, LastSpaces, TabWidth) ->
    %% Take last element
    %% Copy all tabs before last element
    %% New element indention should contain Tabs plus whitespaces.
    %% Both new and old element should has same offset.
    {LastLine, LastOffset} = LastS,
    %% How many whitespaces to have?
    Indention = LastOffset - 1,
    TabToks = same_line_tabs_before_loc(LastS, Toks),
    TabOffset = length(TabToks) * TabWidth,
    SpaceOffset = Indention - TabOffset,
    TabToks ++ whitespaces(SpaceOffset);
%% Multiline version, proper indention, use indention of the last element
indent_new_elem(true, LastS, Toks, LastSpaces, TabWidth) ->
    filter_last_list_toks(LastSpaces).
    
filter_last_list_toks([]) ->
    [];
filter_last_list_toks(Toks) ->
    LastTok = lists:last(Toks),
    {_, LastLineToks,_} = partition_by_location_line(token_loc(LastTok), Toks),
    LastLineToks.

get_whitespaces_between(MatchS, MatchE, Toks) ->
    {BeforeToks, MatchToks, AfterToks} = partition_by_range(MatchS, MatchE, Toks),
    filter_whitespaces(MatchToks).

filter_whitespaces(Toks) ->
    [T||T={whitespace,_,_} <- Toks].

get_delimiter_forms(dos) ->
    [{whitespace,{1,1},'\r'}, {whitespace,{1,1},'\n'}];
get_delimiter_forms(mac) ->
    [{whitespace,{1,1},'\r'}];
get_delimiter_forms(unix) ->
    [{whitespace,{1,1},'\n'}].

%% Goes from Loc left skipping all comments or whitespaces
%% Stop and return token
go_left_skipping_comments_and_whitespaces(Loc, Toks) ->
    {BeforeToks, _MatchToks, _AfterToks} = partition_by_location(Loc, Toks),
    BeforeToksR = lists:reverse(BeforeToks),
    hd(skip_comments_and_whitespaces(BeforeToksR)).

go_left_skipping_whitespaces(Loc, Toks) ->
    {BeforeToks, _MatchToks, _AfterToks} = partition_by_location(Loc, Toks),
    BeforeToksR = lists:reverse(BeforeToks),
    hd(skip_whitespaces(BeforeToksR)).

skip_comments_and_whitespaces([]) ->
    [];
skip_comments_and_whitespaces([T|Toks]) ->
    case is_comment_or_whitespace(T) of
        true -> skip_comments_and_whitespaces(Toks);
        false -> [T|Toks]
    end.

is_comment_or_whitespace({comment,_,_}) ->
    true;
is_comment_or_whitespace({whitespace,_,_}) ->
    true;
is_comment_or_whitespace(_) ->
    false.

is_new_line({whitespace,_,'\n'}) ->
    true;
is_new_line({whitespace,_,'\r'}) ->
    true;
is_new_line(_) ->
    false.

skip_whitespaces([]) ->
    [];
skip_whitespaces([T|Toks]) ->
    case is_whitespace(T) of
        true -> skip_whitespaces(Toks);
        false -> [T|Toks]
    end.

is_whitespace({whitespace,_,_}) ->
    true;
is_whitespace(_) ->
    false.


%% shift_tokens_left_after/3 that stops ones new line is reached
shift_tokens_loc_left_singleline(Loc, ShiftChars, Toks) ->
    {BeforeToks, MatchToks, AfterToks} = partition_by_location_line(Loc, Toks),
    BeforeToks ++ shift_tokens_left_after(Loc, ShiftChars, MatchToks) ++ AfterToks.

shift_tokens_left_after(Loc, ShiftChars, Toks) ->
    {BeforeToks, MatchToks, AfterToks} = partition_by_location(Loc, Toks),
    BeforeToks ++ MatchToks ++ shift_tokens_left(ShiftChars, AfterToks).

shift_tokens_left(ShiftChars, Toks) ->
    [set_token_loc(T, shift_token_loc_left(ShiftChars, token_loc(T))) || T <- Toks].

shift_tokend_down(ShiftLines, Toks) ->
    [set_token_loc(T, shift_token_loc_down(ShiftLines, token_loc(T))) || T <- Toks].

shift_token_loc_left(ShiftChars, {Line, Offset}) ->
    {Line, Offset+ShiftChars}.

shift_token_loc_down(ShiftLines, {Line, Offset}) ->
    {Line+ShiftLines, Offset}.

partition_by_location_line({Line,_Offset}, Toks) ->
    MatchS = {Line, 0},
    MatchE = {Line+1, 0},
    partition_by_range(MatchS, MatchE, Toks).

%% cut_leading_whitespaces + cut_line_comment
cut_line_comment_and_leading_whitespaces(LineLoc, Toks) ->
    {CommentToks, OtherToks} = cut_line_comment(LineLoc, Toks),
    case CommentToks of
        [] ->
            {[], Toks};
        [CommentH|_] ->
            {WsToks, OtherToks1} = cut_leading_whitespaces(token_loc(CommentH), OtherToks),
            {WsToks ++ CommentToks, OtherToks1}
    end.

%% Cuts "Start      Loc"
%%            ^^^^^^
cut_leading_whitespaces(Lok, Toks) ->
    Start = shift_token_loc_left(1, token_loc(go_left_skipping_comments_and_whitespaces(Lok, Toks))),
    {BeforeToks, MatchedToks, AfterToks} = partition_by_range(Start, Lok, Toks),
    {MatchedToks, BeforeToks ++ AfterToks}.

%% ListLoc is Line or Loc
cut_line_comment(LineLoc, Toks) ->
    CommentToks = match_line_comment(LineLoc, Toks),
    OtherToks = Toks -- CommentToks,
    {CommentToks, OtherToks}.
    
match_line_comment({Line,_}, Toks) ->
    match_line_comment(Line, Toks);
match_line_comment(Line, Toks) ->
    [T||T={comment, {L,_}, _} <- Toks, Line =:= L].

match_line_tabs({Line,_}, Toks) ->
    match_line_tabs(Line, Toks);
match_line_tabs(Line, Toks) ->
    [T||T={whitespace, {L,_}, '\t'} <- Toks, Line =:= L].


has_new_lines(Toks) ->
    lists:any(fun is_new_line/1, Toks).

%% Get tabs before Loc on the Loc's line
same_line_tabs_before_loc(Loc, Toks) ->
    %% All tabs on the line
    LineTabs = match_line_tabs(Loc, Toks),
    %% Tabs before
    {BeforeToks, _MatchToks, _AfterToks} = partition_by_location(Loc, LineTabs),
    BeforeToks.

whitespaces(Len) ->
    lists:duplicate(Len, {whitespace, {1,1}, ' '}).

iolist_to_list(X) ->
    erlang:binary_to_list(erlang:iolist_to_binary(X)).

ast_to_string(FileFormat, Tree, TabWidth) ->
    io:format("s ast_to_string ~p~n", [now()]),
    X = iolist_to_list(wrangler_prettypr:print_ast(FileFormat, Tree, TabWidth)),
    io:format("e ast_to_string ~p~n", [now()]),
    X.

toks_to_binary(Toks) ->
    erlang:iolist_to_binary(wrangler_misc:concat_toks(Toks)).
