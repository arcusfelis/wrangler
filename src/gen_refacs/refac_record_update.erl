%% @hidden
%% @private
-module(refac_record_update).

-behaviour(gen_refac).

%% Include files
-include_lib("wrangler/include/wrangler.hrl").

-record(rec_field, {
        index,
        var_pos,
        def_pos,
        rec_pos,
        parent_rec_pos
        }).
        

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
    ["Record Name : ", "Record Definition File Path : "].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec check_pre_cond(Args::#args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(#args{current_file_name=File,
                     user_inputs=[RawRecName, RecDefFilePath]}) ->
    try   list_to_atom(RawRecName)
    of RecName -> 
            case read_record_definition([RecDefFilePath, File], RecName) of
                {ok, _} -> ok;
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
transform(#args{current_file_name=File, user_inputs=[RawRecName, RecDefFilePath]}) ->
    RecName = list_to_atom(RawRecName),
    {ok, RecAttrForm} = read_record_definition([RecDefFilePath, File], RecName),
    [_NameForm, FieldDefForms] = erl_syntax:attribute_arguments(RecAttrForm),
    FieldNames = [erl_syntax:atom_value(erl_syntax:record_field_name(FieldForm))
                  || FieldForm <- erl_syntax:tuple_elements(FieldDefForms)],
    FieldDefVals = [maybe_value(to_term(erl_syntax:record_field_value(FieldForm)))
                  || FieldForm <- erl_syntax:tuple_elements(FieldDefForms)],
    io:format("Record #~p{~p}", [RecName, lists:zip(FieldNames, FieldDefVals)]),

    %% This code is dirty and skips a lot of cases.
    Bindings = ?STOP_TD_TU([?COLLECT(?T("V1@ = V2@"), 
                  %% Record => Var
                  case is_var(V1@) of
                    true  -> {V2@, V1@};
                    false -> {V1@, V2@}
                  end,
                  (is_record_expr(V1@, RecName) andalso is_var(V2@))
                   orelse 
                  (is_record_expr(V2@, RecName) andalso is_var(V1@))
                )],
                [File]), 

    RecExprs = ?STOP_TD_TU([?COLLECT(?T("Rec@"), 
                  _This@,
                  is_record_expr(Rec@, RecName)
                )],
                [File]),

    RecPos2Form = [{pos(Rec), Rec} || Rec <- RecExprs],
    RecPos2FormDict = dict:from_list(RecPos2Form),

    %% Convert `FieldValue''s position to `Rec''s position.
    %% `FieldValue' is a variable.
    %%
    %% Pseudocode.
    %% `Rec#rec{field = FieldValue}'
    FieldValuePos2RecPos =
        [{pos(FieldValue), pos(Rec)}
         || Rec <- RecExprs,
            Field <- wrangler_syntax:record_expr_fields(Rec),
            FieldValue <- [field_value(Field)],
            is_var(FieldValue)],
    FieldValuePos2RecPosDict = dict:from_list(FieldValuePos2RecPos),

    FieldVarPos2FieldPos =
        [{pos(VarForm), pos(Rec)}
         || Rec <- RecExprs,
            Field <- wrangler_syntax:record_expr_fields(Rec),
            VarForm <- variables(field_value(Field))],
    FieldVarPos2FieldPosDict = dict:from_list(FieldVarPos2FieldPos),

    %% `Fields' are a tuple of forms.
    RecPos2Fields =
        [{pos(Rec), record_fields(Rec, FieldNames)}
         || Rec <- RecExprs],

    %% Contains variable define positions.
    RecPos2FieldBindings =
        [{RecPos, field_bindings(FieldForms)}
         || {RecPos, FieldForms} <- RecPos2Fields],

    RecPos2FieldBindingsDict = dict:from_list(RecPos2FieldBindings),

    RecPos2FieldParentRecPos =
        [{RecPos, field_parent_records(FieldBindings, FieldVarPos2FieldPosDict)}
         || {RecPos, FieldBindings} <- RecPos2FieldBindings],

    RecPos2ParentRecPos =
        [{RecPos, ParentRecPos}
         || {RecPos, FieldParRecs} <- RecPos2FieldParentRecPos,
            ParentRecPos <- [select_parent_record(RecPos, FieldParRecs, 2)],
            ParentRecPos =/= undefined],

    RecPos2ParentRecPosDict = dict:from_list(RecPos2ParentRecPos),

    %% Positions of records to add `Arg'.
    RecPosToUpdate = 
        [RecPos
         || {RecPos, _ParentRec} <- RecPos2ParentRecPos,
            not has_argument(RecPos, RecPos2FormDict)],

    RecPosToUpdateSet = sets:from_list(RecPosToUpdate),
    ToDelete =
        [{RecPos, FieldN, ParRecPos, VarPos}
         || {RecPos, FieldParRecs} <- RecPos2FieldParentRecPos,
            sets:is_element(RecPos, RecPosToUpdateSet),
            
            ParRecPos <- [dict:fetch(RecPos, RecPos2ParentRecPosDict)],
            FieldBindings <- [dict:fetch(RecPos, RecPos2FieldBindingsDict)],
            {FieldN, FieldParRecPos, [VarPos|_]} <- lists:zip3(
                    lists:seq(1, tuple_size(FieldParRecs)),
                    tuple_to_list(FieldParRecs),
                    tuple_to_list(FieldBindings)),
            ParRecPos =:= FieldParRecPos],



    io:format("FieldValuePos2RecPos ~p~n", [FieldValuePos2RecPos]),
%   io:format("RecExprs ~p~n", [RecExprs]),
%   io:format("RecPos2Fields ~p~n", [RecPos2Fields]),
    io:format("RecPos2FieldBindings ~p~n", [RecPos2FieldBindings]),
    io:format("RecPos2FieldParentRecPos ~p~n", [RecPos2FieldParentRecPos]),
%   io:format("RecPos2ParentRec ~p~n", [RecPos2ParentRec]),
    io:format("RecPos2ParentRecPos ~p~n", [RecPos2ParentRecPos]),
    io:format("RecPosToUpdate ~p~n", [RecPosToUpdate]),
    io:format("ToDelete ~p~n", [ToDelete]),


    ?FULL_TD_TP([rule1(RecName, FieldNames, FieldDefVals)],
                [File]).

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

record_name(RecForm) ->
    wrangler_syntax:atom_value(wrangler_syntax:record_expr_type(RecForm)).

field_name(FieldForm) ->
    wrangler_syntax:atom_value(wrangler_syntax:record_field_name(FieldForm)).

field_value(FieldForm) ->
    wrangler_syntax:record_field_value(FieldForm).


rule1(RecName, FieldNames, FieldDefVals)
    when is_atom(RecName), is_list(FieldNames), is_list(FieldDefVals) ->
    ?RULE(?T("Rec@"), 
          begin
            case cat(Rec@) of
                pattern -> _This@;
                _ -> _This@
            end
          end,
          is_record_expr(Rec@, RecName)
         ).

    

mk_record_field(Name, Val) ->
    wrangler_syntax:record_field(
         wrangler_syntax:atom(Name),
         wrangler_syntax:remove_comments(Val)).

mk_record_fields(RecordFields, Es) ->
    [mk_record_field(Name, Val)
     || {Name, Val} <- lists:zip(RecordFields, Es),
     type(Val) =/= underscore].

%% This fun is the same as `mk_record_field/2', but deletes fields
%% with default values.
mk_record_fields(RecordFields, FieldDefVals, Es) ->
    [mk_record_field(Name, Val)
     || {Name, Val, Def} <- lists:zip3(RecordFields, Es, FieldDefVals),
     wrangler_to_term(Val) =/= {value, Def},
     type(Val) =/= underscore].

%% @doc `Expr' is from `erl_syntax'.
to_term(Expr) ->
    Es = [erl_syntax:revert(Expr)],
    try erl_eval:exprs(Es, [])
    of {value, V, _} -> {value, V}
    catch error:Reason -> {error, Reason}
    end.

maybe_value({value, V})       -> V;
maybe_value({error, _Reason}) -> undefined.


%% @doc `Expr' is a wrangler AST.
wrangler_to_term(Expr) ->
    Es = [wrangler_syntax:revert(Expr)],
    try erl_eval:exprs(Es, [])
    of {value, V, _} -> {value, V}
    catch error:Reason -> {error, Reason}
    end.


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
        variable -> api_refac:variable_define_pos(ValueForm);
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


field_parent_records(FieldBindings, FieldVarPos2FieldPosDict) ->
    list_to_tuple([field_parent_record(FieldBinding, FieldVarPos2FieldPosDict)
                   || FieldBinding <- tuple_to_list(FieldBindings)]).

field_parent_record([Pos|_], FieldVarPos2FieldPosDict) ->
    case dict:find(Pos, FieldVarPos2FieldPosDict) of
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
        {ok, RecForm} ->
            wrangler_syntax:record_expr_argument(RecForm) =/= none;
        error ->
            error({key_not_found, RecPos})
    end.
