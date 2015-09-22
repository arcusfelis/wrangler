-module(wrangler_consult).
-export([consult/1,
         write_file/2,
         write_file/3]).

consult(Filename) ->
    FileFormat = wrangler_misc:file_format(Filename),
    TabWidth = 4,
    {ok, Bin} = file:read_file(Filename),
    Terms = parse_terms(Bin, TabWidth, FileFormat),
    Str = erlang:iolist_to_binary(add_auto_funs(Terms, FileFormat)),
    TmpFilename = tmp_filenate(),
    ok = file:write_file(TmpFilename, Str),
    FileFormat = wrangler_misc:file_format(TmpFilename),
    {ok, {AnnAST,_Info}} = wrangler_ast_server:parse_annotate_file(TmpFilename, false, [], TabWidth, FileFormat),
    AnnTrees = unlift_funs_from_ast(AnnAST),
    {ok, AnnTrees}.

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
    Bin2 = list_to_binary(wrangler_prettypr:print_ast(FileFormat, AnnAST, TabWidth)),
    Terms2 = parse_terms(Bin2, TabWidth, FileFormat),
    Bin = iolist_to_binary(rem_auto_funs(Terms2, FileFormat)),
    file:write_file(FileNameOut, Bin).

tmp_filenate() ->
    string:strip(os:cmd("mktemp"), right, $\n).

parse_terms(Bin, TabWidth, FileFormat) ->
    Str = erlang:binary_to_list(Bin),
    {ok, Toks, _} = wrangler_scan_with_layout:string(Str, {1,1}, TabWidth, FileFormat),
    split_by_dot(Toks).

%% Return a list of expression lists
split_by_dot(Toks) ->
    split_by_dot2(Toks, []).

split_by_dot2([{dot,_}=Dot|Toks], Acc) ->
    Acc2 = [Dot|Acc],
    [lists:reverse(Acc2)|split_by_dot(Toks)];
split_by_dot2([Tok|Toks], Acc) ->
    split_by_dot2(Toks, [Tok|Acc]);
split_by_dot2([], []) ->
    [];
split_by_dot2([], Acc) ->
    [lists:reverse(Acc)].


%% Convert expressions into forms by adding `auto_00001() ->'
add_auto_funs(ExprList, FileFormat) ->
    Sep = get_delimitor(FileFormat),
    add_auto_funs(ExprList, Sep, 1).

add_auto_funs([Exprs|ExprList], Sep, N) ->
    case ends_with_dot(Exprs) of
        true ->
            %% Add three extra lines (new line, fun def, new line)
            Str = io_lib:format("~s~s()->~s", [Sep, fun_name(N), Sep])
                ++ wrangler_misc:concat_toks(Exprs),
            [Str|add_auto_funs(ExprList, Sep, N+1)];
        false ->
            Str = wrangler_misc:concat_toks(Exprs),
            [Str|add_auto_funs(ExprList, Sep, N)]
    end;
add_auto_funs([], _Sep, _N) ->
    [].


rem_auto_funs(ExprList, FileFormat) ->
    Sep = get_delimitor(FileFormat),
    remove_auto_funs(ExprList, Sep).

remove_auto_funs([Exprs|ExprList], Sep) ->
    case ends_with_dot(Exprs) of
        true ->
            %% Add three extra lines (new line, fun def, new line)
            Str = remove_auto_fun(wrangler_misc:concat_toks(Exprs), Sep),
            [Str|remove_auto_funs(ExprList, Sep)];
        false ->
            Str = wrangler_misc:concat_toks(Exprs),
            [Str|remove_auto_funs(ExprList, Sep)]
    end;
remove_auto_funs([], _Sep) ->
    [].

remove_auto_fun(I, Sep) ->
    B = erlang:iolist_to_binary(I),
    Str = erlang:binary_to_list(B),
    Str2 = remove_separator(Str, Sep),
    Str3 = remove_auto_fun_body(Str2),
    remove_separator(Str3, Sep).

remove_separator([H|Str], [H|Sep]) ->
    remove_separator(Str, Sep);
remove_separator(Str, []) ->
    Str.

remove_auto_fun_body([$c, $o, $n, $s, $u, $l, $t, $_, _, _, _, _, _, $(, $), $-, $> | Str]) ->
    Str.

fun_name(N) when is_integer(N) ->
   L = io_lib:format("consult_~5..0B", [N]),
   B = erlang:iolist_to_binary(L),
   erlang:binary_to_list(B).

ends_with_dot([{dot,_}]) ->
    true;
ends_with_dot([_|T]) ->
    ends_with_dot(T);
ends_with_dot(_) ->
    false.

get_delimitor(FileFormat) ->
    case FileFormat of
        dos -> "\r\n";
        mac -> "\r";
        unix -> "\n"
    end.
