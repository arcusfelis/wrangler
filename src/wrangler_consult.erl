-module(wrangler_consult).
-export([consult/1,
         consult_terms/1,
         consult_string/1,
         write_file/2,
         write_file/3]).

-export([terms_to_tokens/2]).

-export([string_to_consult/3,
         string_to_ast/3]).

string_to_consult(Str, TabWidth, FileFormat) ->
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    Exprs = replace_dots_with_comma_keep_last(Toks),
    erlang:iolist_to_binary("consult()->\n" ++ toks_to_binary(Exprs)).

consult(Filename) ->
    FileFormat = wrangler_misc:file_format(Filename),
    TabWidth = 4,
    {ok, Bin} = file:read_file(Filename),
    Str = erlang:binary_to_list(Bin),
    Bin2 = string_to_consult(Str, TabWidth, FileFormat),
    Str2 = erlang:binary_to_list(Bin2),
    string_to_ast(Str2, TabWidth, FileFormat).

string_to_ast(Str, TabWidth, FileFormat) ->
    {ok, Toks, _} = wrangler_scan:string(Str, {1,1}, TabWidth, FileFormat),
    Toks1 = [Tok || Tok <- Toks, not is_comment_or_whitespace(Tok)],
    io:format(user, "Toks ~p~n", [Toks]),
    {ok, Form} = wrangler_parse:parse(Toks),
    Forms = [Form],
    SyntaxTree = wrangler_recomment:recomment_forms(Forms, []),
    AnnAst = annotate_bindings(Str, SyntaxTree, TabWidth, FileFormat),
    {ok, AnnAst}.

annotate_bindings(Str, AST, TabWidth, FileFormat) ->
    Info = wrangler_syntax_lib:analyze_forms(AST),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    Ann = wrangler_ast_server:add_token_and_ranges(AST, Toks),
    AnnAST0 = wrangler_syntax_lib:annotate_bindings(Ann, ordsets:new()),
    Comments = wrangler_comment_scan:string(Str),
    AnnAST1 = wrangler_recomment:recomment_forms(AnnAST0, Comments),
    AnnAST2 = wrangler_ast_server:update_toks(Toks,AnnAST1),
    wrangler_annotate_ast:add_fun_define_locations(AnnAST2, Info).

is_comment_or_whitespace({whitespace,_,_}) -> true;
is_comment_or_whitespace({comment,_,_}) -> true;
is_comment_or_whitespace(_) -> false.


string_to_ast1(Str, TabWidth, FileFormat) ->
    with_tmpfile(fun(TmpFilename) ->
        ok = file:write_file(TmpFilename, Str),
        {ok, {AnnAST,_Info}} =
                %% Does not work correctly with consult_binary4()
                wrangler_ast_server:parse_annotate_file(TmpFilename, false, [], TabWidth, FileFormat),
%               wrangler_ast_server:quick_parse_annotate_file(TmpFilename, [], 4),
        {ok, AnnAST}
        end).

%% @doc Convert terms to AST
consult_terms(Terms) ->
    String = terms_to_string(Terms),
    consult_string(String).

%% @doc Convert string to AST
consult_string(String) ->
    TmpFilename = tmp_filename(),
    ok = file:write_file(TmpFilename, String),
    try
        consult(TmpFilename)
    after
        ok = file:delete(TmpFilename)
    end.

terms_to_tokens(Terms, FileFormat) ->
    TabWidth = 4,
    Str = iolist_to_list(terms_to_string(Terms)),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    {ok, Toks}.

terms_to_string(Terms) ->
    [io_lib:format("~p.~n", [X]) || X <- Terms].


unlift_funs_from_ast(AnnAST) ->
    Elems = wrangler_syntax:form_list_elements(AnnAST),
    [rewrite_expression(E) || E <- Elems].

rewrite_expression(E) ->
    case wrangler_syntax:type(E) of
        function ->
            hd(wrangler_syntax:function_clauses(E));
        _ ->
            E
    end.

write_file(FilenameOrig, AnnAST) ->
    FileNameOut = FilenameOrig ++ ".new",
    write_file(FilenameOrig, FileNameOut, AnnAST).

write_file(FilenameOrig, FileNameOut, AnnAST) ->
    TabWidth = 4,
    FileFormat = wrangler_misc:file_format(FilenameOrig),
    Bin = list_to_binary(wrangler_prettypr:print_ast(FileFormat, AnnAST, TabWidth)),
    Bin2 = replace_commas_with_dots(Bin, TabWidth, FileFormat),
    Bin3 = remove_consult(Bin2),
    file:write_file(FileNameOut, Bin3).

remove_consult(<<"consult()->",Bin/binary>>) -> Bin;
%% something is wrong, it was prettyprinted
remove_consult(<<"\r\n\r\nconsult() -> ",Bin/binary>>) -> Bin;
remove_consult(<<"\r\rconsult() -> ",Bin/binary>>) -> Bin;
remove_consult(<<"\n\nconsult() -> ",Bin/binary>>) -> Bin;
remove_consult(<<"\r\n\r\nconsult() ->\n",Bin/binary>>) -> Bin;
remove_consult(<<"\r\rconsult() ->\n",Bin/binary>>) -> Bin;
remove_consult(<<"\n\nconsult() ->\n",Bin/binary>>) -> Bin.

tmp_filename() ->
    string:strip(os:cmd("mktemp"), right, $\n).


replace_dots_with_comma_keep_last(Toks) when is_list(Toks) ->
    Count = count_dots(Toks),
    Limit = Count - 1,
    replace_dots_with_comma(Toks, Limit).

count_dots(Toks) ->
    count_dots(Toks, 0).

count_dots([{dot,_}|Toks], Count) ->
    count_dots(Toks, Count+1);
count_dots([_|Toks], Count) ->
    count_dots(Toks, Count);
count_dots([], Count) ->
    Count.

%% The second argument Limit limits number of dots to be replaced
replace_dots_with_comma(Toks, 0) ->
    Toks;
replace_dots_with_comma([{dot,Pos}|Toks], Limit) ->
    [{',', Pos}|replace_dots_with_comma(Toks, Limit-1)];
replace_dots_with_comma([Tok|Toks], Limit) ->
    [Tok|replace_dots_with_comma(Toks, Limit)];
replace_dots_with_comma([], _Limit) ->
    ok.

get_delimitor(FileFormat) ->
    case FileFormat of
        dos -> "\r\n";
        mac -> "\r";
        unix -> "\n"
    end.

%% Replaces dots on the top level only
replace_commas_with_dots(Bin, TabWidth, FileFormat) ->
    io:format(user, "replace_commas_with_dots ~p~n", [Bin]),
    Str = binary_to_list(Bin),
    %% Get actual AST
    {ok, AnnAST} = string_to_ast(Str, TabWidth, FileFormat),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    io:format(user, "Node types ~p~n", [[wrangler_syntax:type(Node)
                                         || Node <- wrangler_syntax:form_list_elements(AnnAST)]]),
    FunAST = hd([Node || Node <- wrangler_syntax:form_list_elements(AnnAST),
                         lists:member(wrangler_syntax:type(Node), [function,eof_marker])]),
    ClauseAST = hd(wrangler_syntax:function_clauses(FunAST)),
    ExprASTs = wrangler_syntax:clause_body(ClauseAST),
    EndLocs = [get_range_end(ExprAST) || ExprAST <- ExprASTs],
    AllCommaLocs = comma_locs(Toks),
    ExpectedCommasLocs = expected_comma_logs(list_init(EndLocs), AllCommaLocs),

%   io:format("~nComLocs ~10000p ~nEndLocs ~10000p ~n", [AllCommaLocs, EndLocs]),

    Toks2 = replace_commas(Toks, ExpectedCommasLocs),
    toks_to_binary(Toks2).

%% Replace commans based on their location
replace_commas([{',', CommaLoc}|Toks], MatchLocs) ->
    case lists:member(CommaLoc, MatchLocs) of
        true ->
            [{dot, CommaLoc}|replace_commas(Toks, MatchLocs)];
        false ->
            [{',', CommaLoc}|replace_commas(Toks, MatchLocs)]
    end;
replace_commas([Tok|Toks], MatchLocs) ->
    [Tok|replace_commas(Toks, MatchLocs)];
replace_commas([], _MatchLocs) ->
    [].

comma_locs([{',',CommaLoc}|Toks]) ->
    [CommaLoc|comma_locs(Toks)];
comma_locs([_|Toks]) ->
    comma_locs(Toks);
comma_locs([]) ->
    [].

get_range(Node) ->
     As = wrangler_syntax:get_ann(Node),
     {value, {range, {S, E}}} = lists:keysearch(range, 1, As),
     {S, E}.

get_range_end(Node) ->
    {_S, E} = get_range(Node),
    E.

%% Expected comma is the first comma after end of a term
%% Config examples:
%% {term}.
%% or
%% {term}      .
expected_comma_logs([E|_]=EndLocs, [A|AllCommaLocs]) when A < E ->
    expected_comma_logs(EndLocs, AllCommaLocs);
expected_comma_logs([_|EndLocs], [A|AllCommaLocs]) ->
    [A|expected_comma_logs(EndLocs, AllCommaLocs)];
expected_comma_logs([], _) ->
    [].

list_init(List) ->
    [_|R] = lists:reverse(List),
    lists:reverse(R).

toks_to_binary(Toks) ->
    erlang:iolist_to_binary(wrangler_misc:concat_toks(Toks)).

iolist_to_list(X) ->
    erlang:binary_to_list(erlang:iolist_to_binary(X)).

with_tmpfile(F) ->
    Filename = tmp_filename(),
    try
        F(Filename)
    after
        file:delete(Filename)
    end.
