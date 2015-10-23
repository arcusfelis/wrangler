-module(ejd_cfg_proper_tests).
-compile([export_all]).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Specific bugs
%% ------------------------------------------------------------------

abstract_concrete_test_() ->
    io:format(user, "~p~n", [string_to_binary_tree("\\n")]),
    [?_assertEqual(1, wrangler_syntax:concrete(wrangler_syntax:abstract(1))),
     ?_assertEqual(<<"mz">>, wrangler_syntax:concrete(wrangler_syntax:abstract(<<"mz">>))),
     ?_assertEqual(<<"mz">>, wrangler_syntax:concrete(string_to_binary_tree("mz"))),
     ?_assertEqual(<<"\n">>, wrangler_syntax:concrete(string_to_binary_tree("\n"))),
     ?_assertEqual(<<"\r">>, wrangler_syntax:concrete(string_to_binary_tree("\r"))),
     ?_assertEqual(<<"\\n">>, wrangler_syntax:concrete(string_to_binary_tree("\\n"))),
     ?_assertEqual("\\n", wrangler_syntax:string_concrete(
                                wrangler_syntax:binary_field_body(
                                  hd(wrangler_syntax:binary_fields(
                                      string_to_binary_tree("\\n")))))),

     ?_assertEqual(simple_fold_parse(wrangler_syntax:abstract(<<"mz">>)),
                   simple_fold_concrete(wrangler_syntax:abstract(<<"mz">>))),
     ?_assertEqual(simple_fold_parse(wrangler_syntax:abstract(<<0:1>>)),
                   simple_fold_concrete(wrangler_syntax:abstract(<<0:1>>))),
     ?_assertEqual(fold_parse(string_to_binary_tree("mz")),
                   fold_concrete(string_to_binary_tree("mz"))),
     ?_assertEqual(fold_parse(string_to_binary_tree("\n")),
                   fold_concrete(string_to_binary_tree("\n"))),
     ?_assertEqual(fold_parse(string_to_binary_tree("\\n")),
                   fold_concrete(string_to_binary_tree("\\n"))),
     ?_assertEqual(fold_parse(string_to_binary_tree("\\r")),
                   fold_concrete(string_to_binary_tree("\\r"))),
     ?_assertEqual(fold_parse(string_to_binary_tree("\\e")),
                   fold_concrete(string_to_binary_tree("\\e"))),
     ?_assertEqual(<<"\n\n<<\"\\\\n\">>">>,
                   erlang:iolist_to_binary(wrangler_prettypr:pp_a_form(string_to_binary_tree("\\n"), unix, [], 4)))].


tree_to_binary(Bin, Tree) ->
    with_tmpfile(fun(Filename) ->
                    with_tmpfile(fun(FilenameOut) ->
                        ok = file:write_file(Filename, Bin),
                        ok = wrangler_consult:write_file(Filename, FilenameOut, Tree),
                        {ok, BinWritten} = file:read_file(FilenameOut),
                        BinWritten
                    end)
                end).

config_binary1() ->
    <<"{loglevel,5}.\n"
            "{hosts,[]}.\n"
            "{listen,[{80,mod_test,[]}]}.\n"
            "{modules,[{mod_test,[]}]}.">>.

config_binary2() ->
    <<"{loglevel,1}.\n"
      "{hosts,[]}.\n"
      "{listen,[]} % does\n"
      "            % does\n"
      "            % does\n"
      ".\n"
      "{modules,[]}.">>.

config_binary3() ->
    <<"{loglevel,1}.\n"
      "{hosts,[]}.\n"
      "{listen,[]} % does\n"
      "            % does\n"
      "            % does\n"
      ".\n"
      "{modules,[{mod_odbc, []}]}.">>.

config_binary4() ->
    <<"{loglevel,1}.\n"
      "{hosts,[]}.\n"
      "{listen,[]} % does\n"
      "            % does\n"
      "            % does\n"
      ".\n"
      "{modules,[{mod_odbc, []},\n"
      "          {mod_odbc_mysql, []}]}.">>.

consult_binary4() ->
    <<"consult()->\n"
      "{loglevel,1},\n"
      "{hosts,[]},\n"
      "{listen,[]} % does\n            % does\n            % does\n,"
      "\n{modules,[]}.">>.

write_config_test() ->
    Bin = <<"config.">>,
    {ok, Tree} = wrangler_consult:consult_string(Bin),
    BinOut = tree_to_binary(Bin, Tree),
    io:format("Old ~p~nNew ~p~n", [Bin, BinOut]),
    ?assertEqual(Bin, BinOut).

write_config1_test() ->
    Bin = config_binary1(),
    {ok, Tree} = wrangler_consult:consult_string(Bin),
    BinOut = tree_to_binary(Bin, Tree),
    io:format("Old ~p~nNew ~p~n", [Bin, BinOut]),
    ?assertEqual(Bin, BinOut).

write_config2_test() ->
    Bin = config_binary2(),
    {ok, Tree} = wrangler_consult:consult_string(Bin),
    BinOut = tree_to_binary(Bin, Tree),
    io:format("~nOld ~p~nNew ~p~n", [Bin, BinOut]),
    ?assertEqual(Bin, BinOut).

add_module_after_x_x_test() ->
    Bin = config_binary1(),
    Commands = [{add_module_after,mod_test,mod_test}],
    {ok, Tree} = wrangler_consult:consult_string(Bin),
    {ok, Tree2, _} = ejabberd_cfg_editor:run_commands(Commands, Tree, unix, 4),
    BinOut = tree_to_binary(Bin, Tree2),
    io:format("~nBinOut   ~p~nExpected ~p~n", [BinOut, Bin]),
    ?assertEqual(Bin, BinOut).

add_module_test() ->
    Bin = config_binary2(),
    Commands = [{add_module,mod_odbc}],
    {ok, Tree} = wrangler_consult:consult_string(Bin),
    {ok, Tree2, Result} = ejabberd_cfg_editor:run_commands(Commands, Tree, unix, 4),
    io:format("Result ~p~n", [Result]),
    BinOut = tree_to_binary(Bin, Tree2),
    Expected = config_binary3(),
    io:format("~nBinOut   ~p~nExpected ~p~n", [BinOut, Expected]),
    ?assertEqual(Expected, BinOut).

add_two_modules_test() ->
    Bin = config_binary2(),
    Commands = [{add_module,mod_odbc},{add_module,mod_odbc_mysql}],
    {ok, Tree} = wrangler_consult:consult_string(Bin),
    {ok, Tree2, _} = ejabberd_cfg_editor:run_commands(Commands, Tree, unix, 4),
    BinOut = tree_to_binary(Bin, Tree2),
    Expected = config_binary4(),
    io:format("~nBinOut   ~p~nExpected ~p~n", [BinOut, Expected]),
    ?assertEqual(Expected, BinOut).

string_to_binary_tree(S) ->
    EscapedString = io_lib:write_string(S),
    CroppedString = tl(lists:reverse(tl(lists:reverse(EscapedString)))),
    wrangler_syntax:binary([wrangler_syntax:binary_field(wrangler_syntax:string(CroppedString))]).

simple_fold_concrete(Tree) ->
    Trees = api_ast_traverse:fold(fun(T,A) -> [T|A] end, [], Tree),
    [{wrangler_syntax:type(T), simplify_error_reason(api_ast_traverse2:tree_to_term_using_concrete(T))} || T <- Trees].

simple_fold_parse(Tree) ->
    Trees = api_ast_traverse:fold(fun(T,A) -> [T|A] end, [], Tree),
    [{wrangler_syntax:type(T), simplify_error_reason(api_ast_traverse2:tree_to_term_using_parser(T))} || T <- Trees].

fold_concrete(Tree) ->
    Trees = api_ast_traverse:fold(fun(T,A) -> [T|A] end, [], Tree),
    [{wrangler_syntax:type(T), api_ast_traverse2:tree_to_term_using_concrete(T)} || T <- Trees].

fold_parse(Tree) ->
    Trees = api_ast_traverse:fold(fun(T,A) -> [T|A] end, [], Tree),
    [{wrangler_syntax:type(T), api_ast_traverse2:tree_to_term_using_parser(T)} || T <- Trees].


%% ------------------------------------------------------------------
%% Call test generators
%% ------------------------------------------------------------------


wrapper(Name) ->
    [{setup,
      fun() -> ok end,
      fun(_) -> [{atom_to_list(Name), ?MODULE:Name()}] end}].


run_test_generators_once_test_() ->
    AllFuns = ?MODULE:module_info(exports),
    [wrapper(Name) || {Name, Arity} <- AllFuns,
                       Arity =:= 0,
                       lists:suffix("_gen", atom_to_list(Name))].

%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_gen() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{constraint_tries, 500}]),
    erlang:group_leader(EunitLeader, self()),
    ?_assertEqual([], Res).


prop_simple() ->
    ?FORALL(Tree,
            ejd_config_tree,
        begin
        equals(1, 1)
        end).

prop_abstract_concrete() ->
    ?FORALL(Term, term(),
        ?WHENFAIL(io:format("Terms:~n~p~n",
                            [Term]),
                  equals(Term, wrangler_syntax:concrete(wrangler_syntax:abstract(Term))))).

prop_overwrite_concrete() ->
    ?FORALL(Term, term(),
        ?WHENFAIL(io:format("Terms:~n~p~n",
                            [Term]),
                  equals(Term, wrangler_syntax:concrete(
                                 api_ast_traverse2:overwrite_concretes(
                                   wrangler_syntax:abstract(Term)))))).

prop_to_binary() ->
    ?FORALL({Terms, Comments}, {ejd_config(), comments()},
            begin
            Str = to_binary_with_random_comments(Terms, Comments),
            ?WHENFAIL(io:format("String:~n~tsTerms:~n~w~nConsulted:~n~w~n",
                                [Str, Terms, (catch element(2, string_consult(Str)))]),
                      equals({ok, Terms}, string_consult(Str)))
            end).


prop_tree_to_term() ->
    ?FORALL({Bin, TermsIn}, ejd_config_binary_and_terms(),
        ?WHENFAIL(io:format("TermsIn:~n~p~nBinary:~n~ts~n",
                            [TermsIn, Bin]),
            with_tmpfile(fun(Filename) ->
                ok = file:write_file(Filename, Bin),
                {ok, Tree} = wrangler_consult:consult(Filename),
                Trees = api_ast_traverse:fold(fun(T,A) -> [T|A] end, [], Tree),
                Terms1 = [api_ast_traverse2:tree_to_term_using_parser(T) || T <- Trees],
                Terms2 = [api_ast_traverse2:tree_to_term_using_concrete(T) || T <- Trees],
                Terms3 = [simplify_error_reason(X) || X <- Terms1],
                Terms4 = [simplify_error_reason(X) || X <- Terms2],
                %% T1 parser, T2 concrete
                ?WHENFAIL([io:format("T1 ~p~nT2 ~p~n", [T1, T2])
                           ||{T1,T2} <- lists:zip(Terms1, Terms2),
                             simplify_error_reason(T1) =/= simplify_error_reason(T2)],
                          equals(Terms3, Terms4))
        end))).

simplify_error_reason({error, _Reason}) ->
    {error, error};
simplify_error_reason(X) ->
    X.


%% Execute random commands for random configs with
%% simple_ejabberd_cfg_editor and ejabberd_cfg_editor and compare
prop_editor() ->
    ?FORALL({{Bin, TermsIn}, Commands}, {ejd_config_binary_and_terms(), list(command())},
        ?WHENFAIL(io:format("TermsIn:~n~p~nBinary:~n~ts~nCommands:~n~p~n",
                            [TermsIn, Bin, Commands]),
            with_tmpfile(fun(FilenameOut) ->
                with_tmpfile(fun(Filename) ->
                    io:format("~n~nS ~p~n", [now()]),
                    io:format("Commands ~p / Bin ~p~n", [length(Commands), byte_size(Bin)]),
                    ok = file:write_file(Filename, Bin),
                    io:format("x consult ~p~n", [now()]),
                    {ok, Terms} = file_consult(Filename),
                    {ok, Tree} = wrangler_consult:consult(Filename),
                    io:format("x run ~p~n", [now()]),
                    {ok, Tree2, _} = ejabberd_cfg_editor:run_commands(Commands, Tree, unix, 4),
                    SimpleTermsOut0 = simple_ejabberd_cfg_editor:run_commands(Commands, Terms),
                    io:format("x reconsult ~p~n", [now()]),
                    %% okey, we can compare SimpleTermsOut0 with TermsOut. Oh, no. unicode.
                    %% file:consult/1 returns code points in r17
                    %% but bytes in r13. So lets write it and read again.
                    {ok, SimpleTermsOut} = terms_reconsult(SimpleTermsOut0),
                    io:format("x write ~p~n", [now()]),
                    ok = wrangler_consult:write_file(Filename, FilenameOut, Tree2),
                    io:format("x file_consult ~p~n", [now()]),
                    {ok, TermsOut} = file_consult(FilenameOut),
                    io:format("E ~p~n~n~n", [now()]),
                    ?WHENFAIL((catch io:format("Result file:~n~ts~nSimpleTermsOut:~n~p~nTermsOut:~n~p~n",
                                               [element(2, file:read_file(FilenameOut)), SimpleTermsOut, TermsOut])),
                              equals(SimpleTermsOut, TermsOut))
                 end)
            end))).

with_tmpfile(F) ->
    Filename = tmp_filename(),
    try
        F(Filename)
    after
        file:delete(Filename)
    end.

string_consult(Str) ->
    {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(erlang:iolist_to_binary(Str))),
    Values = [erl_parse_value(erl_parse:parse_term(Toks), Toks)
              || Toks <- splitwithdot(Tokens)],
    {ok, Values}.

file_consult(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    string_consult(Bin).

erl_parse_value({ok, Value}, _Toks) ->
    Value.

splitwithdot(Tokens) ->
    MatchedNot = splitwith_all(fun({'.',_}) -> false; ({dot,_}) -> false; (_) -> true end, Tokens),
    [add_dot(X) || {X,_} <- MatchedNot].

add_dot(Toks) -> Toks ++ [{dot, {0,0}}].

naive_string_consult(Str) ->
    Filename = tmp_filename(),
    try
        ok = file:write_file(Filename, Str),
        file:consult(Filename)
    of {ok, Out} ->
           {ok, Out};
       {error, Error} ->
           error_logger:error_msg("issue=\"Failed to string_consult\", string=~w", [Str]),
           {error, Error}
    after
        file:delete(Filename)
    end.

tmp_filename() ->
    string:strip(os:cmd("mktemp"), right, $\n).

terms_reconsult(Terms) ->
    Binary = terms_to_binary(Terms),
    string_consult(Binary).

terms_to_binary(Terms) ->
    erlang:iolist_to_binary([io_lib:format("~p.~n", [X]) || X <- Terms]).


host() -> string().
inet_port() -> range(0, 65535).
module() -> atom().
listen_module() -> readable_module().
module_module() -> readable_module().

readable_module() ->
    ?LET(Names, non_empty(arc_types:sublist(3, module_names())),
         list_to_atom(string:join(lists:map(fun atom_to_list/1, [mod|Names]), "_"))).

readable_key() ->
    ?LET(Names, non_empty(arc_types:sublist(3, module_opt_keys())),
         list_to_atom(string:join(lists:map(fun atom_to_list/1, Names), "_"))).

readable_host() ->
    ?LET({Names, Domain},
         {non_empty(arc_types:sublist(2, module_opt_keys())), oneof(domains())},
         string:join(lists:map(fun atom_to_list/1, Names ++ [Domain]), ".")).

readable_value() ->
    oneof(module_opt_values()).


module_names() ->
    [odbc, mysql, nosql, mssql, redis, mongo, cassa, elastic, kv, riak,
     cache, queries, http, snmp, bosh, metrics, graphite, js, websockets,
     iqs, messages, linux, kernel, core, log, event, disco, last,
     private, privacy, muc, room, vcard, fsm, mnesia, stream, db, auth,
     open, sasl, hook, node, lib, error, handler, session, user].

domains() ->
    [com, pl, ru, org, io].

module_opt_keys() ->
    [set, get, password, delay, interval, timeout, user, db, table, token].

module_opt_values() ->
    [integer(1, 5000), integer(1, 255), ?LAZY(oneof(module_opt_values())),
     ?LET(X, ?LAZY(oneof(module_names())), atom_to_list(X)),
     bool(), binary()].

ejd_config() ->
    arc_types:exect_weighted_union(ejd_config1()).

module_prop() ->
    {module_module(), arc_types:unique_key(1, list({readable_key(), readable_value()}))}.

ejd_config1() ->
    [{1, 1, {loglevel, range(1,5)}},
     {1, 2, {hosts, arc_types:unique_list(list(readable_host()))}},
     {1, 3, {listen, arc_types:unique_key(1, list({inet_port(), listen_module(), []}))}},
     {1, 4, {modules, arc_types:unique_key(1, list(module_prop()))}}
    ].

ejd_config_binary() ->
    ?LET({Terms, Comments}, {ejd_config(), comments()},
        to_binary_with_random_comments(Terms, Comments)).

ejd_config_binary_and_terms() ->
    ?LET({Terms, Comments}, {ejd_config(), comments()},
        {to_binary_with_random_comments(Terms, Comments), Terms}).

comments() ->
    list(comment()).

comment() ->
    oneof([{pre, comment_text()}, {post, comment_text()}]).

comment_text() ->
    arc_types:range_list(1, 3, comment_line()).

comment_line() ->
    ?LET(Words, arc_types:range_list(1, 10, oneof(comment_word())), string:join(Words, " ")).

comment_word() ->
    ["does", "not", "work", "config", "admin", "secret", "log", "directory",
     "access", "result", "error", "disable", "enable", "version", "port",
     "firewall", "table", "connection", "timeout", "600 seconds", "connects",
     "reconnects", "help", "me", "plz"].

command() ->
    oneof([{add_module, module_module()},
           {add_module_after, module_module(), module_module()},
           {delete_module, module_module()},
           {set_option, module_module(), readable_key(), readable_value()},
           {unset_option, module_module(), readable_key()},
          
           {add_listener, inet_port(), listen_module()},
           {add_listener_after, inet_port(), listen_module(), inet_port(), listen_module()},
           {delete_listener, inet_port(), listen_module()},
           {set_listener_option, inet_port(), listen_module(), readable_key(), readable_value()},
           {unset_listener_option, inet_port(), listen_module(), readable_key()},
           {add_listener_element, inet_port(), listen_module(), readable_key()},
           {delete_listener_element, inet_port(), listen_module(), readable_key()},
           {set_global_option, readable_key(), readable_value()},
           {unset_global_option, readable_key()},
           {set_global_prefixed_option, readable_key(), readable_key(), readable_value()},
           {unset_global_prefixed_option, readable_key(), readable_key()},
           {add_global_element, readable_key()},
           {delete_global_element, readable_key()}
          ]).

%% Spread comments across tokens
to_binary_with_random_comments(Terms, Comments) ->
    Filetype = unix,
    NewLineToks = wrangler_token_pp:get_delimiter_forms(Filetype),
    {ok, Toks} = wrangler_consult:terms_to_tokens(Terms, Filetype),
    Toks2 = add_comments(Toks, Comments, NewLineToks),
    erlang:iolist_to_binary(wrangler_misc:concat_toks(Toks2)).

add_comments(Toks, [], NewLineToks) ->
    Toks;
add_comments(Toks, Comments, NewLineToks) ->
    Toks1 = add_comments0(pre, Toks, Comments, NewLineToks),
    Toks2 = add_comments0(post, Toks1, Comments, NewLineToks),
    move_comma_before_comment(Toks2).

add_comments0(PosType, Toks, Comments, NewLineToks) ->
    add_comments1(PosType, Toks, skip_comments(PosType, Comments), NewLineToks).

add_comments1(PosType, Toks, Comments, NewLineToks) ->
    ToksCount = length(skip_toks(PosType, Toks)),
    CommentsCount = length(Comments),
    CommentsAfter = [round((N/CommentsCount) * ToksCount)+1 || N <- lists:seq(1, CommentsCount)],
    add_comments2(1, Toks, Comments, CommentsAfter, NewLineToks).

add_comments2(_N, Toks, [], _CommentsAfter, _NewLineToks) ->
    Toks;
add_comments2(N, [Tok|Toks], [{PosType,_}|_]=Comments, CommentsAfter, NewLineToks) ->
    case is_valid(PosType, Tok) of
        true ->
            add_comments3(N, [Tok|Toks], Comments, CommentsAfter, NewLineToks);
        false ->
            [Tok|add_comments2(N, Toks, Comments, CommentsAfter, NewLineToks)]
    end;
add_comments2(_N, [], _Comments, _CommentsAfter, _NewLineToks) ->
    [].

add_comments3(N, [Tok|Toks], [Comment|Comments], [N|CommentsAfter], NewLineToks) ->
    insert_comment(Comment, Tok, NewLineToks)
    ++ add_comments2(N, Toks, Comments, CommentsAfter, NewLineToks);
add_comments3(N, [Tok|Toks], Comments, CommentsAfter, NewLineToks) ->
    [Tok]
    ++ add_comments2(N+1, Toks, Comments, CommentsAfter, NewLineToks);
add_comments3(_N, [], _Comments, _CommentsAfter, _NewLineToks) ->
    [].

insert_comment({_, []}, Tok, _NewLineToks) ->
    [Tok];
insert_comment({pre, Lines}, Tok, NewLineToks) ->
    NewLineToks
    ++ lists:append([indend_toks(Tok) ++ [{comment,{0,0}, "% " ++ Line}|NewLineToks] || Line <- Lines])
    ++ indend_toks(Tok)
    ++ [Tok];
insert_comment({post, Lines}, Tok, NewLineToks) ->
    H = [{comment,{0,0}, "% " ++ hd(Lines)}|NewLineToks],
    T = [post_indend_toks(Tok) ++ [{comment,{0,0}, "% " ++ Line}|NewLineToks] || Line <- tl(Lines)],
    [Tok]
    ++ [{whitespace,{0,0},' '}]
    ++ lists:append([H|T]).

token_offset({_,{_,Pos},_}) ->
    Pos;
token_offset({_,{_,Pos}}) ->
    Pos.

indend_toks(Tok) ->
    lists:duplicate(token_offset(Tok)-1, {whitespace,{0,0}, ' '}).

post_indend_toks(Tok) ->
    lists:duplicate(token_offset(Tok)+1, {whitespace,{0,0}, ' '}).

skip_toks(PosType, Toks) ->
    [Tok || Tok <- Toks, is_valid(PosType, Tok)].

skip_comments(PosType, Comments) ->
    [Comment || {PosType0, _}=Comment <- Comments, PosType0 =:= PosType].

is_valid(pre, {'{',_}) ->
    true;
is_valid(post, {'}',_}) ->
    true;
is_valid(_, _) ->
    false.

%% Before:
%% {test} % comment
%% ,
%% After:
%% {test}, % comment
%%
move_comma_before_comment([{'}',_}=H|T]) ->
    case has_comma_after_comment(T) of
        true ->
            [H,{',',{0,0}}|move_comma_before_comment(delete_comma_after_comment(delete_first_whitespace(indent_comment_shift(T))))];
        false ->
            [H|move_comma_before_comment(T)]
    end;
move_comma_before_comment([H|T]) ->
    [H|move_comma_before_comment(T)];
move_comma_before_comment([]) ->
    [].

has_comma_after_comment([{whitespace,_,_}|T]) ->
    has_comma_after_comment(T);
has_comma_after_comment([{comment,_,_}|T]) ->
    has_comma_after_comment1(T);
has_comma_after_comment(_) ->
    false.

has_comma_after_comment1([{whitespace,_,_}|T]) ->
    has_comma_after_comment1(T);
has_comma_after_comment1([{comment,_,_}|T]) ->
    has_comma_after_comment1(T);
has_comma_after_comment1([{',',_}|T]) ->
    true;
has_comma_after_comment1(_) ->
    false.

delete_comma_after_comment([{whitespace,_,_}=H|T]) ->
    [H|delete_comma_after_comment(T)];
delete_comma_after_comment([{comment,_,_}=H|T]) ->
    [H|delete_comma_after_comment(T)];
delete_comma_after_comment([{',',_}|T]) ->
    T.

indent_comment_shift([{whitespace,_,_}=H|T]) ->
    [H|indent_comment_shift(T)];
indent_comment_shift([{comment,_,_}=H|T]) ->
    [{whitespace,{0,0}, ' '}, H|indent_comment_shift(T)];
indent_comment_shift(T) ->
    T.

delete_first_whitespace([{whitespace,_,_}|T]) ->
    T;
delete_first_whitespace([H|T]) ->
    delete_first_whitespace(T).

%% @doc Recursive lists:splitwith/2 (kind of)
%%
%% Returns list of `{Matched, NotMatched}' tuples.
%% Property:
%% `List = lists:append([[Matched, NotMatched] || {Matched, NotMatched} <- Result]'
%%
%% Example:
%% `lists2:splitwith_all(fun(X) -> X =/= $. end, "1234.54565..").'
%%
%% Result:
%% `[{"1234","."},{"54565",".."}]'
%%
%% Source https://github.com/arcusfelis/lists2
splitwith_all(_Pred, []) ->
    [];
splitwith_all(Pred, List) ->
    {Matched, Others} = lists:splitwith(Pred, List),
    {NotMatched, Others2} = not_splitwith(Pred, Others),
    [{Matched, NotMatched}|splitwith_all(Pred, Others2)].

not_splitwith(Pred, List) ->
    lists:splitwith(fun(X) -> not Pred(X) end, List).
