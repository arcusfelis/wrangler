-module(ejabberd_cfg_editor).
-export([run_commands/4,
         add_module/4,
         add_module_after/5,
         delete_module/4,
         set_option/6,
         unset_option/5]).

run_commands(Commands, Tree, FileFormat, TabWidth) ->
    run_commands(Commands, Tree, FileFormat, TabWidth, []).


run_commands([Command|Commands], Tree, FileFormat, TabWidth, Results) ->
    io:format("s run_commands ~p~n", [Command]),
    {ok, Tree2, Res} = run_command(Command, Tree, FileFormat, TabWidth),
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
run_command({set_option, Module, OptKey, OptValue}, Tree, FileFormat, TabWidth) ->
    set_option(Module, OptKey, OptValue, Tree, FileFormat, TabWidth);
run_command({unset_option, Module, OptKey}, Tree, FileFormat, TabWidth) ->
    unset_option(Module, OptKey, Tree, FileFormat, TabWidth).


add_module_after(Module, AfterModule, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_atom(AfterModule), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    MatchModF = fun(T,{M,_}, [{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= Module -> [T|A];
                   (_,_,_, A) -> A end,
    MatchAfterModF = fun(T,{M,_}, [{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= AfterModule -> [T|A];
                   (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchModF, [], Tree2),
    AfterAcc = api_ast_traverse2:fold_values_with_path_values(MatchAfterModF, [], Tree2),
    add_module_after_2(Module, Tree, FileFormat, TabWidth, Acc, AfterAcc).

add_module_after_2(Module, Tree, FileFormat, TabWidth, [], []) ->
    %% Not found, insert at the end
    add_module(Module, Tree, FileFormat, TabWidth);
add_module_after_2(Module, Tree, FileFormat, TabWidth, [], [AfterModTree]) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    NewElem = {Module, []},
    %% We assume that modules key exists in the file
    MatchModsF = fun(T, Mods, [{{modules,_}, _}], A) when is_list(Mods) -> [T|A];
                   (_,_,_, A) -> A end,
    [ModsTree] = api_ast_traverse2:fold_values_with_path_values(MatchModsF, [], Tree2),
    NewTree = wrangler_token_pp:insert_another_list_element_after(ModsTree, Tree, NewElem, AfterModTree, FileFormat, TabWidth),
    {ok, NewTree, module_added_after};
add_module_after_2(Module, Tree, FileFormat, TabWidth, [ModTree], _) ->
    {ok, Tree, module_already_defined}.




%% Add module if not yet added
%% Do nothing if it exists
add_module(Module, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    MatchModF = fun(T,{M,_}, [{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= Module -> [T|A];
                   (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchModF, [], Tree2),
    io:format("add_module~n",[]),
    add_module_2(Module, Tree, FileFormat, TabWidth, Acc).

add_module_2(Module, Tree, FileFormat, TabWidth, []) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    NewElem = {Module, []},
    %% We assume that modules key exists in the file
    MatchModsF = fun(T, Mods, [{{modules,_}, _}], A) when is_list(Mods) -> [T|A];
                   (_,_,_, A) -> A end,
    case api_ast_traverse2:fold_values_with_path_values(MatchModsF, [], Tree2) of
        [] ->
            error_logger:error_msg("issue=\"Failed to find 'modules' section\", tree:~n~p",
                                   [Tree]),
            erlang:error(missing_modules_section);
        [ModsTree] ->
            NewTree = wrangler_token_pp:append_list_element(ModsTree, Tree, NewElem, FileFormat, TabWidth),
            {ok, NewTree, module_added}
    end;
add_module_2(Module, Tree, FileFormat, TabWidth, [ModTree]) ->
    {ok, Tree, module_already_defined}.

%% Delete module
delete_module(Module, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    Tree2 = api_ast_traverse2:overwrite_concretes(Tree1),
    MatchModF = fun(T,{M,_}, [{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= Module -> [T|A];
                   (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchModF, [], Tree2),
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
    %% Find Options list
    MatchOptF = fun(T,{K,_},[{Opts,_},{{M,_},_},{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= Module, is_list(Opts), K =:= OptKey -> [T|A];
                    (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchOptF, [], Tree2),
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
    MatchOptsF = fun(T,X,[{{M,_},_},{AllModules,_},{{modules,AllModules},_}], A)
                        when M =:= Module, is_list(X) -> [T|A];
                    (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchOptsF, [], Tree2),
    set_option_2(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, Acc).

%% Module is not defined
set_option_2(_Module, _OptKey, _OptValue, Tree, _FileFormat, _TabWidth, []) ->
    {ok, Tree, module_is_missing};
%% If suddenly we have two modules with the same name -- change only the first one
set_option_2(Module, OptKey, OptValue, Tree, FileFormat, TabWidth, [OptsTree|_]) ->
    MatchOptValueF = fun(T,V,[{{K, V},_},_], A)
                        when K =:= OptKey -> [{tree_value,T,V}|A];
                   (_,_,_, A) -> A end,
    OptValueAcc = api_ast_traverse2:fold_values_with_path_values(MatchOptValueF, [], OptsTree),
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
