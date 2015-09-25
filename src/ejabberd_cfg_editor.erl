-module(ejabberd_cfg_editor).
-export([delete_module/4,
         set_option/6]).

%% Delete module
delete_module(Module, Tree, FileFormat, TabWidth)
    when is_atom(Module), is_integer(TabWidth), is_atom(FileFormat) ->
    Tree1 =  api_ast_traverse:map(fun wrangler_syntax:compact_list/1, Tree),
    MatchModF = fun(T,{M,_}, [{AllModules,_},{{modules,AllModules},_}|_], A)
                        when M =:= Module -> [T|A];
                   (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchModF, [], Tree1),
    delete_module_2(Module, Tree, FileFormat, TabWidth, Acc).

delete_module_2(Module, Tree, FileFormat, TabWidth, []) ->
    {ok, Tree, module_is_missing};
delete_module_2(Module, Tree, FileFormat, TabWidth, [ModTree]) ->
    NewTree = wrangler_token_pp:erase_matched(ModTree, Tree, FileFormat, TabWidth),
    {ok, NewTree, module_deleted}.

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
    %% Find Options list
    MatchOptsF = fun(T,X,[{{M,_},_},{AllModules,_},{{modules,AllModules},_}|_], A)
                        when M =:= Module, is_list(X) -> [T|A];
                    (_,_,_, A) -> A end,
    Acc = api_ast_traverse2:fold_values_with_path_values(MatchOptsF, [], Tree1),
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
