%% @hidden
%% @private
-module(refac_record_update).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").


%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,
         select_focus/1, 
         check_pre_cond/1,
         selective/0, 
         transform/1]).

%%%===================================================================
%%% gen_refac callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() ->
    ["Record Name : ",
     "Record Definition File Path : ",
     "Auto Record Argument Prefix : "].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec check_pre_cond(Args::#args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(#args{current_file_name=File,
                     user_inputs=[RawRecName, RecDefFilePath, Prefix]}) ->
    try   list_to_atom(RawRecName)
    of RecName -> 
            case read_record_definition([RecDefFilePath, File], RecName) of
                {ok, _} ->
                    case api_refac:is_var_name(Prefix) of
                        true -> ok;
                        false -> {error, "Prefix is not a variable name."}
                    end;
                {error, enoent} -> {error, "File does not exist."};
                {error, Error}  -> {error, Error}
            end
    catch error:badarg -> {error, "Passed record name is not atom."}
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args::#args{}) ->
%%                {ok, syntaxTree()} |
%%                {ok, none}
%% @end
%%--------------------------------------------------------------------
select_focus(_Args) ->
    {ok, none}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selective transformation or not.
%%
%% @spec selective() -> boolean()
%% @end
%%--------------------------------------------------------------------
selective() ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(#args{current_file_name=File,
                user_inputs=[RawRecName, RecDefFilePath, Prefix]}) ->
    RecName = list_to_atom(RawRecName),
    {ok, RecAttrForm} = read_record_definition([RecDefFilePath, File], RecName),
    [_NameForm, FieldDefForms] = erl_syntax:attribute_arguments(RecAttrForm),
    FieldNames = [erl_syntax:atom_value(erl_syntax:record_field_name(FieldForm))
                  || FieldForm <- erl_syntax:tuple_elements(FieldDefForms)],

    %% This code is dirty and skips a lot of cases.
    RecPos2Var = ?STOP_TD_TU([?COLLECT(?T("V1@ = V2@"), 
                  %% Record => Var
                  case is_var(V1@) of
                        true  -> {pos(V2@), V1@};
                        false -> {pos(V1@), V2@}
                  end,
                  (is_record_expr(V1@, RecName) andalso is_var(V2@))
                   orelse 
                  (is_record_expr(V2@, RecName) andalso is_var(V1@))
                )],
                [File]), 

    RecPos2VarDict = dict:from_list(RecPos2Var),

    RecExprs = ?STOP_TD_TU([?COLLECT(?T("Rec@"), 
                  Rec@,
                  is_record_expr(Rec@, RecName)
                )],
                [File]),

    FunClauseRangesAndVarNames = ?STOP_TD_TU([?COLLECT(?T("F@"), 
                  {api_refac:start_end_loc(F@), api_refac:bound_var_names(F@)},
                  type(F@) =:= function_clause
                )],
                [File]),

    RecPos2Form = [{pos(Rec), Rec} || Rec <- RecExprs],
    RecPos2FormDict = dict:from_list(RecPos2Form),

    FieldPos2RecPos =
        [{pos(Field), pos(Rec)}
         || Rec <- RecExprs,
            Field <- wrangler_syntax:record_expr_fields(Rec)],

    FieldPos2RecPosDict = dict:from_list(FieldPos2RecPos),

    VarPos2Refs =
        [{pos(VarForm), refs(VarForm)}
         || Rec <- RecExprs,
            Field <- wrangler_syntax:record_expr_fields(Rec),
            VarForm <- variables(field_value(Field))],
    VarPos2RefsDict = dict:from_list(VarPos2Refs),

    %% Field `f1' is simple: `#rec{f1 = V1}'.
    SimpleFieldPoses =
        [pos(Field)
         || Rec <- RecExprs,
            Field <- wrangler_syntax:record_expr_fields(Rec),
            type(field_value(Field)) =:= variable],
    SimpleFieldPosSet = sets:from_list(SimpleFieldPoses),

    FieldVarPos2RecPos =
        [{pos(VarForm), pos(Rec)}
         || Rec <- RecExprs,
            Field <- wrangler_syntax:record_expr_fields(Rec),
            VarForm <- variables(field_value(Field))],
    FieldVarPos2RecPosDict = dict:from_list(FieldVarPos2RecPos),

    FieldVarPos2FieldPos =
        [{pos(VarForm), pos(Field)}
         || Rec <- RecExprs,
            Field <- wrangler_syntax:record_expr_fields(Rec),
            VarForm <- variables(field_value(Field))],
    FieldVarPos2FieldPosDict = dict:from_list(FieldVarPos2FieldPos),

    %% `Fields' are a tuple of forms.
    RecPos2Fields =
        [{pos(Rec), record_fields(Rec, FieldNames)}
         || Rec <- RecExprs],

    %% Contains variables.
    RecPos2FieldBindings =
        [{RecPos, field_bindings(FieldForms)}
         || {RecPos, FieldForms} <- RecPos2Fields],

    RecPos2FieldParentRecPos =
        [{RecPos, ParentRecPos}
         || {RecPos, FieldBindings} <- RecPos2FieldBindings,
         ParentRecPos <- as_list(field_parent_records(FieldBindings,
                                                      FieldVarPos2RecPosDict))],

    RecPos2ParentRecPos =
        [{RecPos, ParentRecPos}
         || {RecPos, FieldParRecs} <- RecPos2FieldParentRecPos,
            ParentRecPos <- as_list(select_parent_record(RecPos,
                                                         FieldParRecs, 2))],

    RecPos2ParentRecPosDict = dict:from_list(RecPos2ParentRecPos),

    %% Positions of records to add `Arg'.
    RecPosToUpdate = 
        [RecPos
         || {RecPos, _ParentRecPos} <- RecPos2ParentRecPos,
            cat(dict:fetch(RecPos, RecPos2FormDict)) =/= pattern,
            not has_argument(RecPos, RecPos2FormDict)],

    RecPosToUpdateSet = sets:from_list(RecPosToUpdate),

    ParRecPosToAddArg =
        lists:usort([ParentRecPos
         || {RecPos, ParentRecPos} <- RecPos2ParentRecPos,
            not has_argument(RecPos, RecPos2FormDict),
            not has_binding(ParentRecPos, RecPos2VarDict)]),

    ParRecPos2NewBind = new_args(ParRecPosToAddArg, Prefix,
                                 FunClauseRangesAndVarNames),

    ParRecPos2NewBindDict = dict:from_list(ParRecPos2NewBind),

    ParRecPos2OldBind = lists:ukeysort(1,
        [{ParentRecPos, var_name(dict:fetch(ParentRecPos, RecPos2VarDict))}
         || {RecPos, ParentRecPos} <- RecPos2ParentRecPos,
            not has_argument(RecPos, RecPos2FormDict),
            has_binding(ParentRecPos, RecPos2VarDict)]),

    ParRecPos2BindDict = dict:from_list(ParRecPos2OldBind ++ ParRecPos2NewBind),

    ToDelete =
        [{RecPos, ParRecPos, pos(VarForm), def(VarForm), pos(FieldForm)}
         || {{RecPos, FieldBindings},
             {RecPos, FieldParRecs},
             {RecPos, Fields}}
            <- lists:zip3(RecPos2FieldBindings,
                          RecPos2FieldParentRecPos,
                          RecPos2Fields),
            sets:is_element(RecPos, RecPosToUpdateSet),
            
            ParRecPos <- [dict:fetch(RecPos, RecPos2ParentRecPosDict)],
            {FieldParRecPos, VarForm, FieldForm} <- lists:zip3(
                    tuple_to_list(FieldParRecs),
                    tuple_to_list(FieldBindings),
                    tuple_to_list(Fields)),
            ParRecPos =:= FieldParRecPos,
            %% Skip f1 in `#rec{f1 = V1 = 1}'.
            FieldForm =/= undefined,
            type(field_value(FieldForm)) =:= variable],

    FieldPosToDelete = [FieldPos || {_, _, _, _, FieldPos} <- ToDelete],
    ParVarPosToDelete =
        [DefPos
         || {_, _, _, [DefPos|_], _} <- ToDelete,
            length(refs(DefPos, VarPos2RefsDict)) =< 1],

    ParFieldPosToDelete =
        [ParFieldPos
         || VarPos <- ParVarPosToDelete,
            ParFieldPos <- dict_find_all(VarPos, FieldVarPos2FieldPosDict),
            sets:is_element(ParFieldPos, SimpleFieldPosSet),
            dict:is_key(VarPos, FieldVarPos2FieldPosDict)],

    ParRecToDeleteFields =
        [dict:fetch(ParFieldPos, FieldPos2RecPosDict)
         || ParFieldPos <- ParFieldPosToDelete],

%   io:format("RecExprs ~p~n", [RecExprs]),
%   io:format("RecPos2Fields ~p~n", [RecPos2Fields]),
%   io:format("RecPos2ParentRec ~p~n", [RecPos2ParentRec]),
%   io:format("RecPos2FieldBindings ~p~n", [RecPos2FieldBindings]),
%   io:format("RecPos2FieldParentRecPos ~p~n", [RecPos2FieldParentRecPos]),
%   io:format("RecPos2ParentRecPos ~p~n", [RecPos2ParentRecPos]),
%   io:format("RecPosToUpdate ~p~n", [RecPosToUpdate]),
%   io:format("ToDelete ~p~n", [ToDelete]),
%   io:format("FieldPosToDelete ~p~n", [FieldPosToDelete]),
%   io:format("ParVarPosToDelete ~p~n", [ParVarPosToDelete]),
%   io:format("ParFieldPosToDelete ~p~n", [ParFieldPosToDelete]),
%   io:format("ParRecPosToAddArg ~p~n", [ParRecPosToAddArg]),
%   io:format("ParRecPos2NewBind ~p~n", [ParRecPos2NewBind]),
%   io:format("ParRecPos2OldBind ~p~n", [ParRecPos2OldBind]),

    RecPosToEdit = RecPosToUpdate ++ ParRecPosToAddArg ++ ParRecToDeleteFields,
    RecPosToEditSet = sets:from_list(RecPosToEdit),
    FieldPosToDeleteSet = sets:from_list(FieldPosToDelete ++ ParFieldPosToDelete),
    VarPosToDeleteSet = sets:from_list(ParVarPosToDelete),

    Rule1 = ?RULE(?T("Rec@"),
          begin
                RecPos = pos(Rec@),
                Rec1 = update_argument(RecPosToUpdateSet,
                                       RecPos2ParentRecPosDict,
                                       ParRecPos2BindDict,
                                       Rec@),
                Rec2 = delete_fields(FieldPosToDeleteSet, Rec1),
                Rec3 = delete_field_vars(VarPosToDeleteSet, Rec2),
                case bind_record(ParRecPos2NewBindDict, RecPos, Rec3) of
                    Rec@ -> Rec@;
                    Updated -> rec_reset_pos_and_range(Updated)
                end
          end,
          is_record_expr(Rec@, RecName)
          andalso
          sets:is_element(pos(Rec@), RecPosToEditSet)
         ),

    ?FULL_BU_TP([Rule1], [File]).

bind_record(ParRecPos2NewBindDict, RecPos, Rec) ->
    %% pos(Rec) /= RecPos
    case dict:find(RecPos, ParRecPos2NewBindDict) of
        {ok, VarName} ->
            VarForm = wrangler_syntax:variable(VarName),
            wrangler_syntax:match_expr(VarForm, Rec);
        error -> Rec
    end.

update_argument(RecPosToUpdateSet, RecPos2ParentRecPosDict, ParRecPos2BindDict, Rec) ->
    RecPos = pos(Rec),
    case sets:is_element(RecPos, RecPosToUpdateSet) of
        true ->
            ParentRecPos = dict:fetch(RecPos, RecPos2ParentRecPosDict),
            VarName = dict:fetch(ParentRecPos, ParRecPos2BindDict),
            update_record_expr_argument(wrangler_syntax:variable(VarName), Rec);
        false -> Rec
    end.

update_record_expr_argument(ArgForm, RecForm) ->
    TypeForm = wrangler_syntax:record_expr_type(RecForm),
    FieldForms = wrangler_syntax:record_expr_fields(RecForm),
    wrangler_syntax:record_expr(ArgForm, TypeForm, FieldForms).

delete_fields(FieldPosToDeleteSet, RecForm) ->
    FieldForms = wrangler_syntax:record_expr_fields(RecForm),
    FieldForms2 =
        [FieldForm || FieldForm <- FieldForms,
         not sets:is_element(pos(FieldForm), FieldPosToDeleteSet)],
    case FieldForms2 of
        FieldForms -> RecForm;
        _ ->
            ArgForm = wrangler_syntax:record_expr_argument(RecForm),
            TypeForm = wrangler_syntax:record_expr_type(RecForm),
            wrangler_syntax:record_expr(ArgForm, TypeForm, FieldForms2)
    end.

delete_field_vars(VarPosToDeleteSet, RecForm) ->
    Rule1 = ?RULE(?T("V1@ = V2@"),
          case sets:is_element(pos(V1@), VarPosToDeleteSet) of
              true  -> V2@;
              false -> V1@
          end,
          sets:is_element(pos(V1@), VarPosToDeleteSet)
          orelse
          sets:is_element(pos(V2@), VarPosToDeleteSet)
         ),

    {ok, OutForm} = ?FULL_BU_TP([Rule1], RecForm),
    OutForm.

%% recursive
rec_reset_pos_and_range(Node) ->
    api_ast_traverse:map(fun reset_pos_and_range/1, Node).

reset_pos_and_range(Node) ->
    Node1 = wrangler_misc:update_ann(Node, {range, {{0,0},{0,0}}}),
    wrangler_syntax:set_pos(Node1, {0,0}).



is_record_expr(RecForm, RecName) ->
    type(RecForm) =:= record_expr  andalso
    record_name(RecForm) =:= RecName.

is_var(Form) ->
    type(Form) =:= variable.

pos(Form) ->
    wrangler_syntax:get_pos(Form).

cat(Form) ->
    api_refac:syntax_category(Form).

type(Form) ->
    wrangler_syntax:type(Form).

def(Form) ->
    api_refac:variable_define_pos(Form).

refs(Form) ->
    api_refac:var_refs(Form).

var_name(Form) ->
    wrangler_syntax:variable_name(Form).

record_name(RecForm) ->
    wrangler_syntax:atom_value(wrangler_syntax:record_expr_type(RecForm)).

field_name(FieldForm) ->
    wrangler_syntax:atom_value(wrangler_syntax:record_field_name(FieldForm)).

field_value(FieldForm) ->
    wrangler_syntax:record_field_value(FieldForm).
    

%% This function is useful for list comprehensions.
as_list(undefined) -> [];
as_list(X) -> [X].


%% Metainfo helpers
%% -------------------------------------------------------------------------

read_record_definition([""|Files], RecName) ->
    read_record_definition(Files, RecName);
read_record_definition([FileName|_], RecName) ->
    case epp:parse_file(FileName, [], []) of
        {ok, Parsed} ->
            find_record_attribute(Parsed, RecName);
        {error, Reason} -> {error, Reason}
    end.

find_record_attribute([Form|Forms], RecName) ->
    case is_attribute_form(Form, record) andalso is_record_attr(Form, RecName) of
        true  -> {ok, Form};
        false -> find_record_attribute(Forms, RecName)
    end;
find_record_attribute([], _RecName) ->
    {error, "Record attribute not found."}.


is_attribute_form(Form, Name) ->
    try
        erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= Name
    catch error:_ ->
        false
    end.

is_record_attr(Form, RecName) ->
    try
        [NameForm, _FieldForms] = erl_syntax:attribute_arguments(Form),
        erl_syntax:atom_value(NameForm) =:= RecName
    catch error:_ ->
        false
    end.


%% Helpers
%% -------------------------------------------------------------------------

%% Return a tuple `{Field1, Field2, ...}'.
%% A special case is `#rec{f1 = V1, f1 = V2}'.
%% A special case is `#rec{_ = V1}'.
%% Only the last variable will be binded.
record_fields(Rec, FieldNames) when is_list(FieldNames) ->
    FieldCount = length(FieldNames),
    Fields = wrangler_syntax:record_expr_fields(Rec),
    Tuple = list_to_tuple(lists:duplicate(FieldCount, undefined)),
    apply_fields(Fields, FieldNames, Tuple).

apply_fields([Field|Fields], FieldNames, Tuple) ->
    Tuple2 =
        case field_name(Field) of
            '_' -> list_to_tuple(lists:duplicate(tuple_size(Tuple), Field));
            FieldName ->
                case elem_pos(FieldName, FieldNames) of
                    %% Warning, field with this name does not exists!
                    undefined -> Tuple;
                    FieldPos  -> setelement(FieldPos, Tuple, Field)
                end
        end,
    apply_fields(Fields, FieldNames, Tuple2);
apply_fields([], _FieldNames, Tuple) ->
    Tuple.


field_bindings(FieldForms) when is_tuple(FieldForms) ->
    list_to_tuple([field_binding(FieldForm)
                   || FieldForm <- tuple_to_list(FieldForms)]).

field_binding(undefined) -> undefined;
field_binding(FieldForm) ->
    ValueForm = field_value(FieldForm),
    field_value_binding(ValueForm).

field_value_binding(ValueForm) ->
    case type(ValueForm) of
        match_expr ->
            P = wrangler_syntax:match_expr_pattern(ValueForm),
            B = wrangler_syntax:match_expr_body(ValueForm),
            case field_value_binding(P) of
                undefined -> field_value_binding(B);
                Var -> Var
            end;
        variable -> ValueForm;
        _        -> undefined
    end.


variables(ExprForm) ->
    case type(ExprForm) of
        match_expr ->
            P = wrangler_syntax:match_expr_pattern(ExprForm),
            B = wrangler_syntax:match_expr_body(ExprForm),
            variables(P) ++ variables(B);
        variable -> [ExprForm];
        _        -> []
    end.


field_parent_records(FieldBindings, FieldVarPos2RecPosDict) ->
    list_to_tuple([case VarForm of
       undefined -> undefined;
       _ -> field_parent_record(def(VarForm), FieldVarPos2RecPosDict)
       end || VarForm <- tuple_to_list(FieldBindings)]).

field_parent_record([Pos|_], FieldVarPos2RecPosDict) ->
    case dict:find(Pos, FieldVarPos2RecPosDict) of
        {ok, ParentRecPos} -> ParentRecPos;
        error -> undefined
    end.

elem_pos(H, L) ->
    elem_pos(H, L, 1).

elem_pos(H, [H|_], N) ->
    N;
elem_pos(H, [_|T], N) ->
    elem_pos(H, T, N+1);
elem_pos(_, [], _) ->
    undefined.


%% Thresthold means after how many usage to use update.
select_parent_record(RecPos, FieldParRecTuple, Thresthold) ->
    FieldParRecs = tuple_to_list(FieldParRecTuple),
    StatsDict = collect_usage_stats(RecPos, FieldParRecs),
    case dict_entry_with_highest_value(StatsDict) of
        {undefined, undefined}               -> undefined;
        {MaxK, MaxV} when MaxV >= Thresthold -> MaxK;
        _                                    -> undefined
    end.

collect_usage_stats(RecPos, FieldParRecs) ->
    collect_usage_stats(RecPos, FieldParRecs, dict:new()).

%% Stats contains mapping from RecPos to usage count as a field value.
collect_usage_stats(RecPos, [RecPos|FieldParRecs], Stats) ->
    %% Skip self.
    collect_usage_stats(RecPos, FieldParRecs, Stats);
collect_usage_stats(RecPos, [ParRecPos|FieldParRecs], Stats) ->
    Stats2 = dict:update_counter(ParRecPos, 1, Stats),
    collect_usage_stats(RecPos, FieldParRecs, Stats2);
collect_usage_stats(_RecPos, [], Stats) ->
    Stats.

dict_entry_with_highest_value(Dict) ->
    %% Return maximum value of 
    F = fun(K, V, {undefined, undefined})      -> {K, V};
           (K, V, {_MaxK, MaxV}) when V > MaxV -> {K, V};
           (_, _, Acc)                         -> Acc
    end,
    dict:fold(F, {undefined, undefined}, Dict).


%% @doc Is a record with RecPos has an argument?
%% `Arg#rec{}' - Arg is an argument.
has_argument(RecPos, RecPos2FormDict) ->
    case dict:find(RecPos, RecPos2FormDict) of
        {ok, RecForm} -> has_argument(RecForm);
        error -> error({key_not_found, RecPos})
    end.

has_argument(RecForm) ->
    wrangler_syntax:record_expr_argument(RecForm) =/= none.


has_binding(RecPos, RecPos2VarDict) ->
    dict:is_key(RecPos, RecPos2VarDict).


new_arg(Prefix, NameSet) ->
    gen_new_arg_suffix(Prefix, NameSet).

gen_new_arg_suffix(Prefix, NameSet) ->
    Name = list_to_atom(Prefix),
    case sets:is_element(Name, NameSet) of
        false -> Name;
        true -> new_arg_suffix_1(Prefix, NameSet, 1)
    end.

new_arg_suffix_1(Prefix, NameSet, N) ->
    Name = list_to_atom(Prefix ++ integer_to_list(N)),
    case sets:is_element(Name, NameSet) of
        false -> Name;
        true -> new_arg_suffix_1(Prefix, NameSet, N+1)
    end.


new_args(ParRecPosList, Prefix, FunClauseRangesAndVarNames) ->
    Sets = [{FunClauseRange, sets:from_list(VarNames)}
            || {FunClauseRange, VarNames} <- FunClauseRangesAndVarNames],
    Ranges = [FunClauseRange
              || {FunClauseRange, _} <- FunClauseRangesAndVarNames],

    %% `undefined' set is for cases, when records are not used in functions.
    %% Warning: they are not same for macroses.
    Range2VarNamesDict = dict:from_list([{undefined, sets:new()}|Sets]),
    new_args_1(ParRecPosList, Prefix, Ranges, Range2VarNamesDict).

new_args_1([ParRecPos|Tail], Prefix, Ranges, Range2VarNamesDict) ->
    ClauseRange = range_search(ParRecPos, Ranges),
    NameSet = dict:fetch(ClauseRange, Range2VarNamesDict),
    ArgName = new_arg(Prefix, NameSet),
    OutEntry = {ParRecPos, ArgName},
    NameSet2 = sets:add_element(ArgName, NameSet),
    Range2VarNamesDict2 = dict:store(ClauseRange, NameSet2, Range2VarNamesDict),
    OutEntries = new_args_1(Tail, Prefix, Ranges, Range2VarNamesDict2),
    [OutEntry|OutEntries];
new_args_1([], _, _, _) -> [].





range_search(X, [{S,E}=H|_]) when S =< X, X =< E ->
    H;
range_search(X, [_|T]) ->
    range_search(X, T);
range_search(_X, []) ->
    undefined.

refs(DefPos, VarPos2RefsDict) ->
    case dict:find(DefPos, VarPos2RefsDict) of
        {ok, Refs} -> Refs;
        error -> []
    end.

dict_find_all(E, D) ->
    case dict:find(E, D) of
        {ok, V} -> [V];
        error -> []
    end.
