-module(ejabberd_cfg_editor).
-export([run_commands/4,
         add_module/4,
         add_module_after/5,
         delete_module/4,
         add_listener/5,
         add_listener_after/7,
         delete_listener/5,
         set_option/6,
         unset_option/5,
         set_listener_option/7,
         unset_listener_option/6]).

run_commands(Commands, Tree, FileFormat, TabWidth) ->
    wrangler_consult:assert_tree_without_errors(Tree, {run_commands, input}),
    run_commands(Commands, Tree, FileFormat, TabWidth, []).


run_commands([Command|Commands], Tree, FileFormat, TabWidth, Results) ->
    io:format("s run_commands ~p~n", [Command]),
    {ok, Tree2, Res} = run_command(Command, Tree, FileFormat, TabWidth),
    wrangler_consult:assert_tree_without_errors(Tree2, {run_command, Command}),
    io:format("e run_commands ~p~n", [Command]),
    run_commands(Commands, Tree2, FileFormat, TabWidth, [Res|Results]);
run_commands([], Tree, _FileFormat, _TabWidth, Results) ->
    {ok, Tree, Results}.

run_command({add_module, Module}, Tree, FileFormat, TabWidth) ->
    add_module(Module, Tree, FileFormat, TabWidth);
run_command({add_module_after, Module, AfterModule}, Tree, FileFormat, TabWidth) ->
    add_module_after(Module, AfterModule, Tree, FileFormat, TabWidth);
run_command({delete_module, Module}, Tree, FileFormat, TabWidth) ->
    delete_module(Module, Tree, FileFormat, TabWidth);
run_command({add_listener, Port, Module}, Tree, FileFormat, TabWidth) ->
    add_listener(Port, Module, Tree, FileFormat, TabWidth);
run_command({add_listener_after, Port, Module, AfterPort, AfterModule}, Tree, FileFormat, TabWidth) ->
    add_listener_after(Port, Module, AfterPort, AfterModule, Tree, FileFormat, TabWidth);
run_command({delete_listener, Port, Module}, Tree, FileFormat, TabWidth) ->
    delete_listener(Port, Module, Tree, FileFormat, TabWidth);
run_command({set_option, Module, OptKey, OptValue}, Tree, FileFormat, TabWidth) ->
    set_option(Module, OptKey, OptValue, Tree, FileFormat, TabWidth);
run_command({unset_option, Module, OptKey}, Tree, FileFormat, TabWidth) ->
    unset_option(Module, OptKey, Tree, FileFormat, TabWidth);
run_command({set_listener_option, Port, Module, OptKey, OptValue}, Tree, FileFormat, TabWidth) ->
    set_listener_option(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth);
run_command({unset_listener_option, Port, Module, OptKey}, Tree, FileFormat, TabWidth) ->
    unset_listener_option(Port, Module, OptKey, Tree, FileFormat, TabWidth).


add_module_after(Module, AfterModule, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_atom(AfterModule), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_module(Module, Tree2),
    AfterAcc = match_module(AfterModule, Tree2),
    add_module_after_2(Module, Tree, Tree2, FileFormat, TabWidth, Acc, AfterAcc).

add_module_after_2(Module, Tree, Tree2, FileFormat, TabWidth, [], []) ->
    %% Not found, insert at the end
%   add_module(Module, Tree, FileFormat, TabWidth);
    add_module_2(Module, Tree, Tree2, FileFormat, TabWidth, []);
add_module_after_2(Module, Tree, Tree2, FileFormat, TabWidth, [], [AfterModTree]) ->
    NewElem = {Module, []},
    Acc = match_modules(Tree2),
    case Acc of
        [] ->
            %% We assume that modules key exists in the file
            error_logger:error_msg("issue=\"Failed to find 'modules' section\", tree:~n~p",
                                   [Tree]),
            erlang:error(missing_modules_section);
        [ModsTree] ->
            NewTree = wrangler_token_pp:insert_another_list_element_after(ModsTree, Tree, NewElem, AfterModTree, FileFormat, TabWidth),
            {ok, NewTree, module_added_after}
    end;
add_module_after_2(Module, Tree, Tree2, FileFormat, TabWidth, [ModTree], _) ->
    {ok, Tree, module_already_defined}.




%% Add module if not yet added
%% Do nothing if it exists
add_module(Module, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_module(Module, Tree2),
    io:format("add_module~n",[]),
    add_module_2(Module, Tree, Tree2, FileFormat, TabWidth, Acc).

add_module_2(Module, Tree, Tree2, FileFormat, TabWidth, []) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    NewElem = {Module, []},
    Acc = match_modules(Tree2),
    case Acc of
        [] ->
            %% We assume that modules key exists in the file
            error_logger:error_msg("issue=\"Failed to find 'modules' section\", tree:~n~p",
                                   [Tree]),
            erlang:error(missing_modules_section);
        [ModsTree] ->
            NewTree = wrangler_token_pp:append_list_element(ModsTree, Tree, NewElem, FileFormat, TabWidth),
            {ok, NewTree, module_added}
    end;
add_module_2(Module, Tree, _Tree2, FileFormat, TabWidth, [ModTree]) ->
    {ok, Tree, module_already_defined}.

%% Delete module
delete_module(Module, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_module(Module, Tree2),
    delete_module_2(Module, Tree, FileFormat, TabWidth, Acc).

delete_module_2(Module, Tree, FileFormat, TabWidth, []) ->
    {ok, Tree, module_is_missing};
delete_module_2(Module, Tree, FileFormat, TabWidth, [ModTree]) ->
    NewTree = wrangler_token_pp:erase_matched(ModTree, Tree, FileFormat, TabWidth),
    {ok, NewTree, module_deleted}.


unset_option(Module, OptKey, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_option(Module, OptKey, Tree2),
    unset_option_2(Module, OptKey, Tree, FileFormat, TabWidth, Acc).

unset_option_2(_Module, _OptKey, Tree, _FileFormat, _TabWidth, []) ->
    {ok, Tree, option_is_missing};
unset_option_2(_Module, _OptKey, Tree, FileFormat, TabWidth, [OptTree]) ->
    NewTree = wrangler_token_pp:erase_matched(OptTree, Tree, FileFormat, TabWidth),
    {ok, NewTree, option_deleted}.
    

%% Add or replace option
%% Do nothing if module does not exists
%% Returns {ok, NewTree, ResultComment}
%% ResultComment describes which actions were made
%% OptValue is term, not tree
%% OptKey is atom
%% Module is atom
set_option(Module, OptKey, OptValue, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    %% Find Options list
    Acc = match_options(Module, Tree2),
    set_option_2(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, Acc).

%% Module is not defined
set_option_2(_Module, _OptKey, _OptValue, Tree, _FileFormat, _TabWidth, []) ->
    {ok, Tree, module_is_missing};
%% If suddenly we have two modules with the same name -- change only the first one
set_option_2(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, [OptsTree|_]) ->
    OptValueAcc = match_option_value(Module, OptKey, OptsTree),
    set_option_3(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree, OptValueAcc).


%% Module is defined, but option is not
set_option_3(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree, []) ->
%% Module is defined, option is defined too, but it's value is the same
    add_option(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree);

%% Compare OptValue
set_option_3(_Module, _OptKey, OptValue, Tree, _FileFormat, _TabWidth, _OptsTree, [{tree_value,_ValueT,OptValue}]) ->
    {ok, Tree, option_already_updated};

%% Module is defined, option is defined too, but it's value is different
set_option_3(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, _OptsTree, [{tree_value,ValueT,ValueX}]) ->
    error_logger:info_msg("option_value_replaced, old_value=~p, new_value=~p", [ValueX, OptValue]),
    replace_option(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, ValueT).

replace_option(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, ValueT) ->
    NewTree = wrangler_token_pp:replace_matched(ValueT, Tree, OptValue, FileFormat, TabWidth),
    {ok, NewTree, option_value_replaced}.

add_option(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree) ->
    %% OptsTree is list, Tree is the whole list
    NewElem = {OptKey, OptValue},
    NewTree = wrangler_token_pp:append_list_element(OptsTree, Tree, NewElem, FileFormat, TabWidth),
    {ok, NewTree, option_value_added}.




add_listener_after(Port, Module, AfterPort, AfterModule, Tree, FileFormat, TabWidth)
    when is_integer(Port), is_atom(Module),
         is_integer(AfterPort), is_atom(AfterModule),
         is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_listener(Port, Module, Tree2),
    AfterAcc = match_listener(AfterPort, AfterModule, Tree2),
    add_listener_after_2(Port, Module, Tree, Tree2, FileFormat, TabWidth, Acc, AfterAcc).

add_listener_after_2(Port, Module, Tree, Tree2, FileFormat, TabWidth, [], []) ->
    %% Not found, insert at the end
%   add_listener(Module, Tree, FileFormat, TabWidth);
    add_listener_2(Port, Module, Tree, Tree2, FileFormat, TabWidth, []);
add_listener_after_2(Port, Module, Tree, Tree2, FileFormat, TabWidth, [], [AfterListenerTree]) ->
    NewElem = {Port, Module, []},
    Acc = match_listeners(Tree2),
    case Acc of
        [] ->
            %% We assume that listeners key exists in the file
            error_logger:error_msg("issue=\"Failed to find 'listeners' section\", tree:~n~p",
                                   [Tree]),
            erlang:error(missing_listeners_section);
        [ListenersTree] ->
            NewTree = wrangler_token_pp:insert_another_list_element_after(ListenersTree, Tree, NewElem, AfterListenerTree, FileFormat, TabWidth),
            {ok, NewTree, listener_added_after}
    end;
add_listener_after_2(Port, Module, Tree, Tree2, FileFormat, TabWidth, [ListenerTree], _) ->
    {ok, Tree, listener_already_defined}.


%% Add listener if not yet added
%% Do nothing if it exists
add_listener(Port, Module, Tree, FileFormat, TabWidth)
    when is_integer(Port), is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_listener(Port, Module, Tree2),
    io:format("add_listener~n",[]),
    add_listener_2(Port, Module, Tree, Tree2, FileFormat, TabWidth, Acc).

add_listener_2(Port, Module, Tree, Tree2, FileFormat, TabWidth, []) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    NewElem = {Port, Module, []},
    Acc = match_listeners(Tree2),
    case Acc of
        [] ->
            %% We assume that listeners key exists in the file
            error_logger:error_msg("issue=\"Failed to find 'listeners' section\", tree:~n~p",
                                   [Tree]),
            erlang:error(missing_listeners_section);
        [ListenersTree] ->
            NewTree = wrangler_token_pp:append_list_element(ListenersTree, Tree, NewElem, FileFormat, TabWidth),
            {ok, NewTree, listener_added}
    end;
add_listener_2(Port, Module, Tree, _Tree2, FileFormat, TabWidth, [ListenerTree]) ->
    {ok, Tree, listener_already_defined}.

%% Delete listener
delete_listener(Port, Module, Tree, FileFormat, TabWidth)
    when is_integer(Port), is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_listener(Port, Module, Tree2),
    delete_listener_2(Port, Module, Tree, FileFormat, TabWidth, Acc).

delete_listener_2(Port, Module, Tree, FileFormat, TabWidth, []) ->
    {ok, Tree, listener_is_missing};
delete_listener_2(Port, Module, Tree, FileFormat, TabWidth, [ListenerTree]) ->
    NewTree = wrangler_token_pp:erase_matched(ListenerTree, Tree, FileFormat, TabWidth),
    {ok, NewTree, listener_deleted}.




unset_listener_option(Port, Module, OptKey, Tree, FileFormat, TabWidth)
    when is_integer(Port), is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    Acc = match_listener_option(Port, Module, OptKey, Tree2),
    unset_listener_option_2(Port, Module, OptKey, Tree, FileFormat, TabWidth, Acc).

unset_listener_option_2(_Port, _Module, _OptKey, Tree, _FileFormat, _TabWidth, []) ->
    {ok, Tree, listener_option_is_missing};
unset_listener_option_2(_Port, _Module, _OptKey, Tree, FileFormat, TabWidth, [OptTree]) ->
    NewTree = wrangler_token_pp:erase_matched(OptTree, Tree, FileFormat, TabWidth),
    {ok, NewTree, listener_option_deleted}.


%% Add or replace listener_option
%% Do nothing if module does not exists
%% Returns {ok, NewTree, ResultComment}
%% ResultComment describes which actions were made
%% OptValue is term, not tree
%% OptKey is atom
%% Module is atom
set_listener_option(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth)
    when is_integer(Port), is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    %% Find Options list
    Acc = match_listener_options(Port, Module, Tree2),
    set_listener_option_2(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, Acc).

%% Module is not defined
set_listener_option_2(_Port, _Module, _OptKey, _OptValue, Tree, _FileFormat, _TabWidth, []) ->
    {ok, Tree, module_is_missing};
%% If suddenly we have two modules with the same name -- change only the first one
set_listener_option_2(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, [OptsTree|_]) ->
    OptValueAcc = match_listener_option_value(Port, Module, OptKey, OptsTree),
    set_listener_option_3(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree, OptValueAcc).


%% Module is defined, but listener_option is not
set_listener_option_3(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree, []) ->
%% Module is defined, listener_option is defined too, but it's value is the same
    add_listener_option(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree);

%% Compare OptValue
set_listener_option_3(_Port, _Module, _OptKey, OptValue, Tree, _FileFormat, _TabWidth, _OptsTree, [{tree_value,_ValueT,OptValue}]) ->
    {ok, Tree, listener_option_already_updated};

%% Module is defined, listener_option is defined too, but it's value is different
set_listener_option_3(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, _OptsTree, [{tree_value,ValueT,ValueX}]) ->
    error_logger:info_msg("listener_option_value_replaced, old_value=~p, new_value=~p", [ValueX, OptValue]),
    replace_listener_option(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, ValueT).

replace_listener_option(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, ValueT) ->
    NewTree = wrangler_token_pp:replace_matched(ValueT, Tree, OptValue, FileFormat, TabWidth),
    {ok, NewTree, listener_option_value_replaced}.

add_listener_option(Port, Module, OptKey, OptValue, Tree, FileFormat, TabWidth, OptsTree) ->
    %% OptsTree is list, Tree is the whole list
    NewElem = {OptKey, OptValue},
    NewTree = wrangler_token_pp:append_list_element(OptsTree, Tree, NewElem, FileFormat, TabWidth),
    {ok, NewTree, listener_option_value_added}.



match_modules(Tree) ->
    MatchModsF = fun(T, Mods, [{{modules,_}, _}], A) when is_list(Mods) -> [T|A];
                   (_,_,_, A) -> A end,
    api_ast_traverse2:fold_values_with_path_values(MatchModsF, [], Tree).

match_module(Module, Tree) ->
    MatchModF = fun(T,{M,_}, [{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= Module -> [T|A];
                   (_,_,_, A) -> A end,
    api_ast_traverse2:fold_values_with_path_values(MatchModF, [], Tree).


match_listeners(Tree) ->
    MatchListenersF = fun(T, Listeners, [{{listen,_}, _}], A) when is_list(Listeners) -> [T|A];
                   (_,_,_, A) -> A end,
    api_ast_traverse2:fold_values_with_path_values(MatchListenersF, [], Tree).

match_listener(Port, Module, Tree) ->
    MatchListenerF = fun(T,{P,M,_}, [{AllListeners,_},{{listen,AllListeners},_}], A)
                        when P =:= Port, M =:= Module -> [T|A];
                   (_,_,_, A) -> A end,
    api_ast_traverse2:fold_values_with_path_values(MatchListenerF, [], Tree).


match_option_value(Module, OptKey, OptsTree) ->
    MatchOptValueF = fun(T,V,[{{K, V},_},_], A)
                        when K =:= OptKey -> [{tree_value,T,V}|A];
                   (_,_,_, A) -> A end,
    api_ast_traverse2:fold_values_with_path_values(MatchOptValueF, [], OptsTree).

match_option(Module, OptKey, Tree) ->
    MatchOptF = fun(T,{K,_},[{Opts,_},{{M,_},_},{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= Module, is_list(Opts), K =:= OptKey -> [T|A];
                    (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchOptF, [], Tree).

%% Find Options list
match_options(Module, Tree) ->
    MatchOptsF = fun(T,Opts,[{{M,_},_},{AllListeners,_},{{modules,AllListeners},_}], A)
                        when M =:= Module, is_list(Opts) -> [T|A];
                    (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchOptsF, [], Tree).


match_listener_option_value(Port, Module, OptKey, OptsTree) ->
    MatchOptValueF = fun(T,V,[{{K, V},_},_], A)
                        when K =:= OptKey -> [{tree_value,T,V}|A];
                   (_,_,_, A) -> A end,
    api_ast_traverse2:fold_values_with_path_values(MatchOptValueF, [], OptsTree).

match_listener_option(Port, Module, OptKey, Tree) ->
    MatchOptF = fun(T,{K,_},[{Opts,_},{{P,M,_},_},{AllListeners,_},{{listen,AllListeners},_}], A)
                        when P =:= Port, M =:= Module, is_list(Opts), K =:= OptKey -> [T|A];
                    (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchOptF, [], Tree).

%% Find Options list
match_listener_options(Port, Module, Tree) ->
    MatchOptsF = fun(T,Opts,[{{P,M,_},_},{AllListeners,_},{{listen,AllListeners},_}], A)
                        when P =:= Port, M =:= Module, is_list(Opts) -> [T|A];
                    (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchOptsF, [], Tree).
