-module(wrangler_token_pp).
-export([erase_matched/4,
         replace_matched/5,
         append_list_element/5]).

%% Search for `MatchAST' in `AnnAST' to delete it.
%% FileFormat = wrangler_misc:file_format(Filename)
%% FileFormat = dos.
%% `MatchAST' should exist in `AnnAST'.
erase_matched(MatchAST, AnnAST, FileFormat, TabWidth) ->
    %% Get range to erase
    {MatchS, MatchE} = get_range(MatchAST),
    SP = get_syntax_path(MatchAST),
    %% Get tokens
    Bin = erlang:iolist_to_binary(wrangler_prettypr:print_ast(FileFormat, AnnAST, TabWidth)),
    Str = erlang:binary_to_list(Bin),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    {BeforeToks, _MatchedToks, AfterToks} = partition_by_range(MatchS, MatchE, Toks),
    Toks1 = fix_and_join_toks(SP, BeforeToks, AfterToks),
    tokens_to_ast(Toks1, TabWidth, FileFormat).

tokens_to_ast(Toks, TabWidth, FileFormat) ->
    %% Convert result tokens back to ast
    Bin = erlang:iolist_to_binary(wrangler_misc:concat_toks(Toks)),
    TmpFilename = tmp_filenate(),
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
    Bin = erlang:iolist_to_binary(wrangler_prettypr:print_ast(FileFormat, AnnAST, TabWidth)),
    Str = erlang:binary_to_list(Bin),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    {BeforeToks, _MatchedToks, AfterToks} = partition_by_range(MatchS, MatchE, Toks),
    Toks1 = BeforeToks ++ NewValueToks ++ AfterToks,
    tokens_to_ast(Toks1, TabWidth, FileFormat).

%% MatchAST = a, AnnAST = [a,b,c], Type = list_prefix, BeforeToks = "[", AfterToks = ",b,c]"
fix_and_join_toks(list_prefix, BeforeToks, AfterToks) ->
    BeforeToks ++ remove_first_list_separator(AfterToks).

remove_first_list_separator([{']',_}|Toks]) ->
    Toks;
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

partition_by_location(Loc, [Tok|Toks], Acc) ->
    case token_loc(Tok) of
        Loc ->
            {lists:reverse(Acc), [Tok], Toks};
        TokLoc when TokLoc < Loc ->
            partition_by_location(Loc, Toks, [Tok|Acc]);
        _ -> % TokLoc > Loc
            {lists:reverse(Acc), [], [Tok|Toks]}
    end.

tmp_filenate() ->
    string:strip(os:cmd("mktemp"), right, $\n).

term_to_tokens(Value, TabWidth, FileFormat) ->
    Io = io_lib:format("~p", [Value]),
    Bin = erlang:iolist_to_binary(Io),
    Str = erlang:binary_to_list(Bin),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    Toks.



%% `ElemValue' is term, not form or tree.
%% `ListTree' is list, in which value should be inserted.
%% This function appends at the end of the list.
%% `Tree' is the whole tree, that should be edited.
append_list_element(ListTree, Tree, ElemValue, FileFormat, TabWidth) ->
    ElemValueToks = term_to_tokens(ElemValue, TabWidth, FileFormat),
    ListTreeElems = wrangler_syntax:list_elements(ListTree),
    append_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, ListTreeElems).

append_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, []) ->
    append_first_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth);
append_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, [_|_]=ListTreeElems) ->
    append_another_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, ListTreeElems).

append_first_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth) ->
    %% Get range to erase
    {_MatchS, MatchE} = get_range(ListTree),
    %% Get tokens
    Bin = erlang:iolist_to_binary(wrangler_prettypr:print_ast(FileFormat, Tree, TabWidth)),
    Str = erlang:binary_to_list(Bin),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    Toks1 = insert_tokens_before(MatchE, ElemValueToks, Toks),
    tokens_to_ast(Toks1, TabWidth, FileFormat).

append_another_list_element(ListTree, Tree, ElemValueToks, FileFormat, TabWidth, ListTreeElems) ->
    LastElem = lists:last(ListTreeElems),
    io:format("LastElem ~p~n", [LastElem]),
    {LastS,_} = get_range(LastElem),
    %% Get range to erase
    {_MatchS, MatchE} = get_range(ListTree),
    %% Get tokens
    Bin = erlang:iolist_to_binary(wrangler_prettypr:print_ast(FileFormat, Tree, TabWidth)),
    Str = erlang:binary_to_list(Bin),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    ElemValueToks1 = [{',',{1,1}}]
%           ++ get_delimitor_forms(FileFormat)
            ++ get_whitespaces_between(token_loc(go_left_skipping_whitespaces(LastS, Toks)), LastS, Toks)
            ++ ElemValueToks,
    AfterLoc = token_loc(go_left_skipping_whitespaces(MatchE, Toks)),
    Toks1 = insert_tokens_after(AfterLoc, ElemValueToks1, Toks),
    tokens_to_ast(Toks1, TabWidth, FileFormat).

get_whitespaces_between(MatchS, MatchE, Toks) ->
    {BeforeToks, MatchToks, AfterToks} = partition_by_range(MatchS, MatchE, Toks),
    filter_whitespaces(MatchToks).

filter_whitespaces(Toks) ->
    [T||T={whitespace,_,_} <- Toks].

get_delimitor_forms(dos) ->
    [{whitespace,{1,1},'\r'}, {whitespace,{1,1},'\n'}];
get_delimitor_forms(mac) ->
    [{whitespace,{1,1},'\r'}];
get_delimitor_forms(mac) ->
    [{whitespace,{1,1},'\n'}].

%% Goes from Loc left skipping all comments or whitespaces
%% Stop and return token
go_left_skipping_whitespaces(Loc, Toks) ->
    {BeforeToks, _MatchToks, _AfterToks} = partition_by_location(Loc, Toks),
    BeforeToksR = lists:reverse(BeforeToks),
    hd(skip_comments_and_whitespaces(BeforeToksR)).

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


shift_tokens_left(ShiftChars, Toks) ->
    [set_token_loc(T, shift_token_loc_left(ShiftChars, token_loc(T))) || T <- Toks].

shift_tokend_down(ShiftLines, Toks) ->
    [set_token_loc(T, shift_token_loc_down(ShiftLines, token_loc(T))) || T <- Toks].

shift_token_loc_left(ShiftChars, {Line, Offset}) ->
    {Line, Offset+ShiftChars}.

shift_token_loc_down(ShiftLines, {Line, Offset}) ->
    {Line+ShiftLines, Offset}.

