-module(ejd_cfg_proper_tests).
-compile([export_all]).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

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


%% Execute random commands for random configs with
%% simple_ejabberd_cfg_editor and ejabberd_cfg_editor and compare
prop_simple() ->
    ?FORALL(Tree,
            ejd_config_tree,
        begin
        equals(1, 1)
        end).

host() -> string().
inet_port() -> range(0, 65535).
module() -> atom().
listen_module() -> readable_module().
module_module() -> readable_module().

readable_module() ->
    ?LET(Names, non_empty(arc_types:sublist(5, module_names())),
         list_to_atom(string:join(lists:map(fun atom_to_list/1, [mod|Names]), "_"))).

readable_key() ->
    ?LET(Names, non_empty(arc_types:sublist(5, module_opt_keys())),
         list_to_atom(string:join(lists:map(fun atom_to_list/1, Names), "_"))).

readable_host() ->
    ?LET({Names, Domain},
         {non_empty(arc_types:sublist(3, module_opt_keys())), oneof(domains())},
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
