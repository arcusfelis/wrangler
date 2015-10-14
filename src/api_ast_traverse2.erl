-module(api_ast_traverse2).
-export([flatmap/2,
         flatmap_values/2,
         flatmap_subtrees/2,
         flatmap_subvalues/2,
         flatmap_with_path/2,
         flatmap_values_with_path/2,
         flatmap_values_with_path_values/2,
         flatfoldmap_values_with_path_values/3,
         fold_values_with_path_values/3]).

%% debug
-export([eval_wrangler_tree/1]).

flatmap_subtrees(F, Tree) when is_function(F, 1) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> Tree;
      Gs ->
      Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
                          [[TT || T <- G, TT <- F(T)] || G <- Gs]),
      wrangler_syntax:copy_attrs(Tree, Tree1)
    end.

%% Recursive flatmap_subtrees/2
flatmap(F, Tree) when is_function(F, 1) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> F(Tree);
      Gs ->
      Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
                          [[X || T <- G, X <- flatmap(F, T)] || G <- Gs]),
      F(wrangler_syntax:copy_attrs(Tree, Tree1))
    end.


%% flatmap/2 but Iterator gets an extra arg: path
flatmap_with_path(F, Tree) when is_function(F, 2) ->
    flatmap_with_path(F, Tree, []).

flatmap_with_path(F, Tree, Path) when is_function(F, 2) ->
    case wrangler_syntax:subtrees(Tree) of
      [] -> F(Tree, Path);
      Gs ->
      Path1 = [Tree|Path],
      Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
                          [[X || T <- G, X <- flatmap_with_path(F, T, Path1)] || G <- Gs]),
      F(wrangler_syntax:copy_attrs(Tree, Tree1), Path)
    end.


%% Recursive flatmap_subtrees/2
flatmap_values(F, Tree) when is_function(F, 2) ->
    FF = fun(T) -> try eval_wrangler_tree(T) of {value, X, _} -> F(T, X); _ -> [T] catch _:_ -> [T] end end,
    api_ast_traverse2:flatmap(FF, Tree).

flatmap_subvalues(F, Tree) when is_function(F, 2) ->
    FF = fun(T) -> try eval_wrangler_tree(T) of {value, X, _} -> F(T, X); _ -> [T] catch _:_ -> [T] end end,
    api_ast_traverse2:flatmap_subtrees(FF, Tree).

%% Recursive flatmap_subtrees/2 + path
%% Path is [Parent, Grandparent, ...]
%% F is F(Tree, Value, Path)
flatmap_values_with_path(F, Tree) when is_function(F, 3) ->
    FF = fun(T, P) -> try eval_wrangler_tree(T) of {value, X, _} -> F(T, X, P); _ -> [T] catch _:_ -> [T] end end,
    api_ast_traverse2:flatmap_with_path(FF, Tree).

%% Recursive flatmap_subtrees/2 + path with values
%% Path is [{ParentValue, Parent}, {GrandparentValue, Grandparent}, ...]
%% F is not called for nodes without values
%% F is F(Tree, Value, Path)
flatmap_values_with_path_values(F, Tree) when is_function(F, 3) ->
    flatmap_values_with_path_values(F, Tree, []).

flatmap_values_with_path_values(F, Tree, Path) when is_function(F, 3) ->
    try eval_wrangler_tree(Tree) of
        {value, Value, _} ->
            flatmap_values_with_path_values(F, {value, Value}, Tree, Path);
        _ ->
            flatmap_values_with_path_values(F, novalue, Tree, Path)
        catch _:_ ->
            flatmap_values_with_path_values(F, novalue, Tree, Path)
    end.

flatmap_values_with_path_values(F, novalue, Tree, Path) ->
    %% Do not update path
    %% Do not call F for Tree
    case wrangler_syntax:subtrees(Tree) of
      [] -> [Tree];
      Gs ->
      Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
                          [[X || T <- G, X <- flatmap_values_with_path_values(F, T, Path)] || G <- Gs]),
      [wrangler_syntax:copy_attrs(Tree, Tree1)]
    end;
flatmap_values_with_path_values(F, {value, Value}, Tree, Path) when is_function(F, 3) ->
    %% Update path
    %% Call F for Tree
    case wrangler_syntax:subtrees(Tree) of
      [] -> F(Tree, Value, Path);
      Gs ->
      Path1 = [{Value, Tree}|Path],
      Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree),
                          [[X || T <- G, X <- flatmap_values_with_path_values(F, T, Path1)] || G <- Gs]),
      F(wrangler_syntax:copy_attrs(Tree, Tree1), Value, Path)
    end.



%% flatmap_values_with_path_values/2 with acc
%% F is F(Tree, Value, Path, Acc)
flatfoldmap_values_with_path_values(F, Acc, Tree) when is_function(F, 4) ->
    flatfoldmap_values_with_path_values(F, Acc, Tree, []).

flatfoldmap_values_with_path_values(F, Acc, Tree, Path) when is_function(F, 4) ->
    try eval_wrangler_tree(Tree) of
        {value, Value, _} ->
            flatfoldmap_values_with_path_values(F, Acc, {value, Value}, Tree, Path);
        _ ->
            flatfoldmap_values_with_path_values(F, Acc, novalue, Tree, Path)
        catch _:_ ->
            flatfoldmap_values_with_path_values(F, Acc, novalue, Tree, Path)
    end.

flatfoldmap_values_with_path_values(F, Acc, novalue, Tree, Path) ->
    %% Do not update path
    %% Do not call F for Tree
    case wrangler_syntax:subtrees(Tree) of
      [] -> {[Tree], Acc};
      Gs ->
      FF = fun(FF_T, FF_Acc) ->
            flatfoldmap_values_with_path_values(F, FF_Acc, FF_T, Path)
            end,
      {Gs1, Acc1} = api_ast_traverse:mapfoldl_listlist(FF, Acc, Gs),
      Gs2 = map_append(Gs1),
      Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree), Gs2),
      {[wrangler_syntax:copy_attrs(Tree, Tree1)], Acc1}
    end;
flatfoldmap_values_with_path_values(F, Acc, {value, Value}, Tree, Path) when is_function(F, 4) ->
    %% Update path
    %% Call F for Tree
    case wrangler_syntax:subtrees(Tree) of
      [] -> F(Tree, Value, Path, Acc);
      Gs ->
      Path1 = [{Value, Tree}|Path],
      FF = fun(FF_T, FF_Acc) ->
            flatfoldmap_values_with_path_values(F, FF_Acc, FF_T, Path1)
            end,
      {Gs1, Acc1} = api_ast_traverse:mapfoldl_listlist(FF, Acc, Gs),
      Gs2 = map_append(Gs1),
      Tree1 = wrangler_syntax:make_tree(wrangler_syntax:type(Tree), Gs2),
      F(wrangler_syntax:copy_attrs(Tree, Tree1), Value, Path, Acc1)
    end.


map_append(List) when is_list(List) ->
    [lists:append(X) || X <- List].


%% flatfoldmap_values_with_path_values/3 without map
fold_values_with_path_values(F, Acc, Tree) when is_function(F, 4) ->
    FF = fun(FF_T, FF_Value, FF_Path, FF_Acc) ->
        FF_Acc1 = F(FF_T, FF_Value, FF_Path, FF_Acc),
        {[FF_T], FF_Acc1}
        end,
    {_NewTree, Acc2} = flatfoldmap_values_with_path_values(FF, Acc, Tree),
    Acc2.

%% erl_eval MUST not be used with wrangler AST trees
%% It's will eval integers as strings ("1" instead of 1).
%% WranglerAST is expression tree
eval_wrangler_tree(WranglerAST) ->
    Str = tree_to_string(WranglerAST),
    string_eval(Str ++ ".", []).

tree_to_string(WranglerAST) ->
    %% Format does not matter here
    FileFormat = unix,
    TabWidth = 4,
    Io = wrangler_prettypr:pp_a_form(WranglerAST, FileFormat, [], TabWidth),
    Bin = erlang:iolist_to_binary(Io),
    erlang:binary_to_list(Bin).

string_eval(S, Envs) ->
    {ok,Forms,_} = erl_scan:string(S),
    {ok,Exprs} = erl_parse:parse_exprs(Forms),
    eall_node_pathsrl_eval:exprs(Exprs, Envs).
