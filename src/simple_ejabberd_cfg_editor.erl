-module(simple_ejabberd_cfg_editor).
-export([run_commands/2]).

run_commands(Commands, OldConfigTerm) ->
    lists:foldl(fun run_command/2, OldConfigTerm, Commands).

run_command({add_module, M}, C) ->
    Mods = proplists:get_value(modules, C),
    case skeymember(2, M, 1, Mods) of
        true -> C;
        false ->
            Mods2 = Mods ++ [{M, []}],
            skeyreplace(2, modules, 1, C, {modules, Mods2})
    end;
run_command({add_module_after, M, A}, C) ->
    Mods = proplists:get_value(modules, C),
    case skeymember(2, M, 1, Mods) of
        true -> C;
        false ->
            Mods2 = skeyaddafter(2, A, 1, Mods, {M, []}),
            skeyreplace(2, modules, 1, C, {modules, Mods2})
    end;
run_command({delete_module, M}, C) ->
    Mods = proplists:get_value(modules, C),
    case skeymember(2, M, 1, Mods) of
        false -> C;
        true ->
            Mods2 = skeydelete(2, M, 1, Mods),
            skeyreplace(2, modules, 1, C, {modules, Mods2})
    end;
run_command({set_option, M, K, V}, C) ->
    Mods = proplists:get_value(modules, C),
    case skeymember(2, M, 1, Mods) of
        false -> C;
        true ->
            Opts = proplists:get_value(M, Mods),
            case skeymember(2, K, 1, Opts) of
                true ->
                    Opts2 = skeyreplace(2, K, 1, Opts, {K,V}),
                    Mods2 = skeyreplace(2, M, 1, Mods, {M, Opts2}),
                    skeyreplace(2, modules, 1, C, {modules, Mods2});
                false ->
                    Opts2 = Opts ++ [{K,V}], %% append new option
                    Mods2 = skeyreplace(2, M, 1, Mods, {M, Opts2}),
                    skeyreplace(2, modules, 1, C, {modules, Mods2})
            end
    end;
run_command({unset_option, M, K}, C) ->
    Mods = proplists:get_value(modules, C),
    case skeymember(2, M, 1, Mods) of
        false -> C;
        true ->
            Opts = proplists:get_value(M, Mods),
            case skeymember(2, K, 1, Opts) of
                true ->
                    Opts2 = skeydelete(2, K, 1, Opts),
                    Mods2 = skeyreplace(2, M, 1, Mods, {M, Opts2}),
                    skeyreplace(2, modules, 1, C, {modules, Mods2});
                false ->
                    C
           end
    end;
run_command({add_listener, P, M}, C) ->
    add_listener(P, M, C);
run_command({add_listener_after, P, M, AP, AM}, C) ->
    add_listener_after(P, M, AP, AM, C);
run_command({delete_listener, P, M}, C) ->
    delete_listener(P, M, C);
run_command({set_listener_option, P, M, K, V}, C) ->
    set_listener_option(P, M, K, V, C);
run_command({unset_listener_option, P, M, K}, C) ->
    unset_listener_option(P, M, K, C);
run_command({add_listener_element, P, M, E}, C) ->
    add_listener_element(P, M, E, C);
run_command({delete_listener_element, P, M, E}, C) ->
    delete_listener_element(P, M, E, C);
run_command({set_global_option, K, V}, C) ->
    set_global_option(K, V, C);
run_command({unset_global_option, K}, C) ->
    unset_global_option(K, C);
run_command({set_global_prefixed_option, P, K, V}, C) ->
    set_global_prefixed_option(P, K, V, C);
run_command({unset_global_prefixed_option, P, K}, C) ->
    unset_global_prefixed_option(P, K, C);
run_command({add_global_element, E}, C) ->
    add_global_element(E, C);
run_command({delete_global_element, E}, C) ->
    delete_global_element(E, C).


add_listener(P, M, C) ->
    Listeners = proplists:get_value(listen, C),
    case skeymember2(3, P, M, 1, 2, Listeners) of
        true -> C;
        false ->
            Listeners2 = Listeners ++ [{P, M, []}],
            skeyreplace(2, listen, 1, C, {listen, Listeners2})
    end.

add_listener_after(P, M, AP, AM, C) ->
    Listeners = proplists:get_value(listen, C),
    case skeymember2(3, P, M, 1, 2, Listeners) of
        true -> C;
        false ->
            Listeners2 = skeyaddafter2(3, AP, AM, 1, 2, Listeners, {P, M, []}),
            skeyreplace(2, listen, 1, C, {listen, Listeners2})
    end.

delete_listener(P, M, C) ->
    Listeners = proplists:get_value(listen, C),
    case skeymember2(3, P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Listeners2 = skeydelete2(3, P, M, 1, 2, Listeners),
            skeyreplace(2, listen, 1, C, {listen, Listeners2})
    end.

set_listener_option(P, M, K, V, C) ->
    Listeners = proplists:get_value(listen, C),
    case skeymember2(3, P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case skeymember(2, K, 1, Opts) of
                true ->
                    Opts2 = skeyreplace(2, K, 1, Opts, {K,V}),
                    Listeners2 = skeyreplace2(3, P, M, 1, 2, Listeners, {P, M, Opts2}),
                    skeyreplace(2, listen, 1, C, {listen, Listeners2});
                false ->
                    Opts2 = Opts ++ [{K,V}], %% append new listener_option
                    Listeners2 = skeyreplace2(3, P, M, 1, 2, Listeners, {P, M, Opts2}),
                    skeyreplace(2, listen, 1, C, {listen, Listeners2})
            end
    end.

unset_listener_option(P, M, K, C) ->
    Listeners = proplists:get_value(listen, C),
    case skeymember2(3, P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case skeymember(2, K, 1, Opts) of
                true ->
                    Opts2 = skeydelete(2, K, 1, Opts),
                    Listeners2 = skeyreplace2(3, P, M, 1, 2, Listeners, {P, M, Opts2}),
                    skeyreplace(2, listen, 1, C, {listen, Listeners2});
                false ->
                    C
           end
    end.

add_listener_element(P, M, E, C) ->
    Listeners = proplists:get_value(listen, C),
    case skeymember2(3, P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case lists:member(E, Opts) of
                true ->
                    C;
                false ->
                    Opts2 = Opts ++ [E], %% append new element
                    Listeners2 = skeyreplace2(3, P, M, 1, 2, Listeners, {P, M, Opts2}),
                    skeyreplace(2, listen, 1, C, {listen, Listeners2})
            end
    end.

delete_listener_element(P, M, E, C) ->
    Listeners = proplists:get_value(listen, C),
    case skeymember2(3, P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case lists:member(E, Opts) of
                true ->
                    Opts2 = lists:delete(E, Opts),
                    Listeners2 = skeyreplace2(3, P, M, 1, 2, Listeners, {P, M, Opts2}),
                    skeyreplace(2, listen, 1, C, {listen, Listeners2});
                false ->
                    C
           end
    end.



set_global_option(K, V, C) ->
    case skeymember(2, K, 1, C) of
        true ->
            skeyreplace(2, K, 1, C, {K,V});
        false ->
            C ++ [{K,V}] %% append new global_option
    end.

unset_global_option(K, C) ->
    case skeymember(2, K, 1, C) of
        true ->
            skeydelete(2, K, 1, C);
        false ->
            C
    end.

set_global_prefixed_option(P, K, V, C) ->
    case skeymember2(3, P, K, 1, 2, C) of
        true ->
            skeyreplace2(3, P, K, 1, 2, C, {P,K,V});
        false ->
            C ++ [{P,K,V}] %% append new global_prefixed_option
    end.

unset_global_prefixed_option(P, K, C) ->
    case skeymember2(3, P, K, 1, 2, C) of
        true ->
            skeydelete2(3, P, K, 1, 2, C);
        false ->
            C
    end.

add_global_element(E, C) ->
    case lists:member(E, C) of
        true ->
            C;
        false ->
            C ++ [E] %% append new element
    end.

delete_global_element(E, C) ->
    case lists:member(E, C) of
        true ->
            lists:delete(E, C);
        false ->
            C
    end.


%% keysomething2/? functions copied from arcusfelis/lists2

%% @doc `proplists:get_value/2' for two keys
get_value2(A, B, [{A,B,V}|_]) ->
    V;
get_value2(A, B, [_|T]) ->
    get_value2(A, B, T);
get_value2(_, _, []) ->
    undefined.


skeyaddafter(S, V, N, [H|T], NewTuple) when tuple_size(H) =:= S, element(N, H) =:= V ->
    [H,NewTuple|T];
skeyaddafter(S, V, N, [H|T], NewTuple) ->
    [H|skeyaddafter(S, V, N, T, NewTuple)];
skeyaddafter(_,_,_, [], NewTuple) ->
    [NewTuple]. % not found, insert at the end

skeyaddafter2(S, A, B, N, M, [H|T], NewTuple)
  when tuple_size(H) =:= S, element(N, H) =:= A, element(M, H) =:= B ->
    [H,NewTuple|T];
skeyaddafter2(S, A, B, N, M, [H|T], NewTuple) ->
    [H|skeyaddafter2(S, A, B, N, M, T, NewTuple)];
skeyaddafter2(_,_,_,_,_, [], NewTuple) ->
    [NewTuple]. % not found, insert at the end

%% @doc `lists:keymember/3' for two elements with size check
skeymember2(S, A, B, N, M, [H|_])
  when tuple_size(H) =:= S, element(N, H) =:= A, element(M, H) =:= B ->
    true;
skeymember2(S, A, B, N, M, [_|T]) ->
    skeymember2(S, A, B, N, M, T);
skeymember2(_, _, _, _, _, []) ->
    false.

%% @doc `lists:keydelete/3' for two elements with size check
skeydelete2(S, A, B, N, M, [H|T])
  when tuple_size(H) =:= S, element(N, H) =:= A, element(M, H) =:= B ->
    T;
skeydelete2(S, A, B, N, M, [H|T]) ->
    [H|skeydelete2(S, A, B, N, M, T)];
skeydelete2(_, _, _, _, _, []) ->
    [].

%% @doc `lists:keyreplace/4' for two elements with size check
skeyreplace2(S, A, B, N, M, [H|T], New)
  when tuple_size(H) =:= S, element(N, H) =:= A, element(M, H) =:= B ->
    [New|T];
skeyreplace2(S, A, B, N, M, [H|T], New) ->
    [H|skeyreplace2(S, A, B, N, M, T, New)];
skeyreplace2(_, _, _, _, _, [], _) ->
    [].

%% @doc `lists:keymember/3' with size check
skeymember(S, A, N, [H|_]) when tuple_size(H) =:= S, element(N, H) =:= A ->
    true;
skeymember(S, A, N, [_|T]) ->
    skeymember(S, A, N, T);
skeymember(_, _, _, []) ->
    false.

%% @doc `lists:keydelete/3' with size check
skeydelete(S, A, N, [H|T]) when tuple_size(H) =:= S, element(N, H) =:= A ->
    T;
skeydelete(S, A, N, [H|T]) ->
    [H|skeydelete(S, A, N, T)];
skeydelete(_, _, _, []) ->
    [].

%% @doc `lists:keyreplace/4' with size check
skeyreplace(S, A, N, [H|T], New) when tuple_size(H) =:= S, element(N, H) =:= A ->
    [New|T];
skeyreplace(S, A, N, [H|T], New) ->
    [H|skeyreplace(S, A, N, T, New)];
skeyreplace(_, _, _, [], _) ->
    [].
