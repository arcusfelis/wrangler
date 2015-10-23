-module(simple_ejabberd_cfg_editor).
-export([run_commands/2]).

run_commands(Commands, OldConfigTerm) ->
    lists:foldl(fun run_command/2, OldConfigTerm, Commands).

run_command({add_module, M}, C) ->
    Mods = proplists:get_value(modules, C),
    case lists:keymember(M, 1, Mods) of
        true -> C;
        false ->
            Mods2 = Mods ++ [{M, []}],
            lists:keyreplace(modules, 1, C, {modules, Mods2})
    end;
run_command({add_module_after, M, A}, C) ->
    Mods = proplists:get_value(modules, C),
    case lists:keymember(M, 1, Mods) of
        true -> C;
        false ->
            Mods2 = keyaddafter(A, 1, Mods, {M, []}),
            lists:keyreplace(modules, 1, C, {modules, Mods2})
    end;
run_command({delete_module, M}, C) ->
    Mods = proplists:get_value(modules, C),
    case lists:keymember(M, 1, Mods) of
        false -> C;
        true ->
            Mods2 = lists:keydelete(M, 1, Mods),
            lists:keyreplace(modules, 1, C, {modules, Mods2})
    end;
run_command({set_option, M, K, V}, C) ->
    Mods = proplists:get_value(modules, C),
    case lists:keymember(M, 1, Mods) of
        false -> C;
        true ->
            Opts = proplists:get_value(M, Mods),
            case lists:keymember(K, 1, Opts) of
                true ->
                    Opts2 = lists:keyreplace(K, 1, Opts, {K,V}),
                    Mods2 = lists:keyreplace(M, 1, Mods, {M, Opts2}),
                    lists:keyreplace(modules, 1, C, {modules, Mods2});
                false ->
                    Opts2 = Opts ++ [{K,V}], %% append new option
                    Mods2 = lists:keyreplace(M, 1, Mods, {M, Opts2}),
                    lists:keyreplace(modules, 1, C, {modules, Mods2})
            end
    end;
run_command({unset_option, M, K}, C) ->
    Mods = proplists:get_value(modules, C),
    case lists:keymember(M, 1, Mods) of
        false -> C;
        true ->
            Opts = proplists:get_value(M, Mods),
            case lists:keymember(K, 1, Opts) of
                true ->
                    Opts2 = lists:keydelete(K, 1, Opts),
                    Mods2 = lists:keyreplace(M, 1, Mods, {M, Opts2}),
                    lists:keyreplace(modules, 1, C, {modules, Mods2});
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
run_command({add_global_element, E}, C) ->
    add_global_element(E, C);
run_command({delete_global_element, E}, C) ->
    delete_global_element(E, C).


add_listener(P, M, C) ->
    Listeners = proplists:get_value(listen, C),
    case keymember2(P, M, 1, 2, Listeners) of
        true -> C;
        false ->
            Listeners2 = Listeners ++ [{P, M, []}],
            lists:keyreplace(listen, 1, C, {listen, Listeners2})
    end.

add_listener_after(P, M, AP, AM, C) ->
    Listeners = proplists:get_value(listen, C),
    case keymember2(P, M, 1, 2, Listeners) of
        true -> C;
        false ->
            Listeners2 = keyaddafter2(AP, AM, 1, 2, Listeners, {P, M, []}),
            lists:keyreplace(listen, 1, C, {listen, Listeners2})
    end.

delete_listener(P, M, C) ->
    Listeners = proplists:get_value(listen, C),
    case keymember2(P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Listeners2 = keydelete2(P, M, 1, 2, Listeners),
            lists:keyreplace(listen, 1, C, {listen, Listeners2})
    end.

set_listener_option(P, M, K, V, C) ->
    Listeners = proplists:get_value(listen, C),
    case keymember2(P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case lists:keymember(K, 1, Opts) of
                true ->
                    Opts2 = lists:keyreplace(K, 1, Opts, {K,V}),
                    Listeners2 = keyreplace2(P, M, 1, 2, Listeners, {P, M, Opts2}),
                    lists:keyreplace(listen, 1, C, {listen, Listeners2});
                false ->
                    Opts2 = Opts ++ [{K,V}], %% append new listener_option
                    Listeners2 = keyreplace2(P, M, 1, 2, Listeners, {P, M, Opts2}),
                    lists:keyreplace(listen, 1, C, {listen, Listeners2})
            end
    end.

unset_listener_option(P, M, K, C) ->
    Listeners = proplists:get_value(listen, C),
    case keymember2(P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case lists:keymember(K, 1, Opts) of
                true ->
                    Opts2 = lists:keydelete(K, 1, Opts),
                    Listeners2 = keyreplace2(P, M, 1, 2, Listeners, {P, M, Opts2}),
                    lists:keyreplace(listen, 1, C, {listen, Listeners2});
                false ->
                    C
           end
    end.

add_listener_element(P, M, E, C) ->
    Listeners = proplists:get_value(listen, C),
    case keymember2(P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case lists:member(E, Opts) of
                true ->
                    C;
                false ->
                    Opts2 = Opts ++ [E], %% append new element
                    Listeners2 = keyreplace2(P, M, 1, 2, Listeners, {P, M, Opts2}),
                    lists:keyreplace(listen, 1, C, {listen, Listeners2})
            end
    end.

delete_listener_element(P, M, E, C) ->
    Listeners = proplists:get_value(listen, C),
    case keymember2(P, M, 1, 2, Listeners) of
        false -> C;
        true ->
            Opts = get_value2(P, M, Listeners),
            case lists:member(E, Opts) of
                true ->
                    Opts2 = lists:delete(E, Opts),
                    Listeners2 = keyreplace2(P, M, 1, 2, Listeners, {P, M, Opts2}),
                    lists:keyreplace(listen, 1, C, {listen, Listeners2});
                false ->
                    C
           end
    end.



set_global_option(K, V, C) ->
    case lists:keymember(K, 1, C) of
        true ->
            lists:keyreplace(K, 1, C, {K,V});
        false ->
            C ++ [{K,V}] %% append new global_option
    end.

unset_global_option(K, C) ->
    case lists:keymember(K, 1, C) of
        true ->
            lists:keydelete(K, 1, C);
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

%% @doc `lists:keymember/3' for two elements
keymember2(A, B, N, M, [H|_]) when element(N, H) =:= A, element(M, H) =:= B ->
    true;
keymember2(A, B, N, M, [_|T]) ->
    keymember2(A, B, N, M, T);
keymember2(_, _, _, _, []) ->
    false.

%% @doc `lists:keydelete/3' for two elements
keydelete2(A, B, N, M, [H|T]) when element(N, H) =:= A, element(M, H) =:= B ->
    T;
keydelete2(A, B, N, M, [H|T]) ->
    [H|keydelete2(A, B, N, M, T)];
keydelete2(_, _, _, _, []) ->
    [].

%% @doc `lists:keyreplace/4' for two elements
keyreplace2(A, B, N, M, [H|T], New) when element(N, H) =:= A, element(M, H) =:= B ->
    [New|T];
keyreplace2(A, B, N, M, [H|T], New) ->
    [H|keyreplace2(A, B, N, M, T, New)];
keyreplace2(_, _, _, _, [], _) ->
    [].

%% @doc `proplists:get_value/2' for two keys
get_value2(A, B, [{A,B,V}|_]) ->
    V;
get_value2(A, B, [_|T]) ->
    get_value2(A, B, T);
get_value2(_, _, []) ->
    undefined.


keyaddafter(V, N, [H|T], NewTuple) when element(N, H) =:= V ->
    [H,NewTuple|T];
keyaddafter(V, N, [H|T], NewTuple) ->
    [H|keyaddafter(V, N, T, NewTuple)];
keyaddafter(_,_, [], NewTuple) ->
    [NewTuple]. % not found, insert at the end

keyaddafter2(A, B, N, M, [H|T], NewTuple) when element(N, H) =:= A, element(M, H) =:= B ->
    [H,NewTuple|T];
keyaddafter2(A, B, N, M, [H|T], NewTuple) ->
    [H|keyaddafter2(A, B, N, M, T, NewTuple)];
keyaddafter2(_,_,_,_, [], NewTuple) ->
    [NewTuple]. % not found, insert at the end
