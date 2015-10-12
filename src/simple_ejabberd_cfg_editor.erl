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
                    Opts2 = [{K,V}|Opts],
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
    end.

keyaddafter(V, N, [H|T], NewTuple) when element(N, H) =:= V ->
    [H,NewTuple|T];
keyaddafter(V, N, [H|T], NewTuple) ->
    [H|keyaddafter(V, N, T, NewTuple)];
keyaddafter(_,_, [], NewTuple) ->
    [NewTuple]. % not found, insert at the end
