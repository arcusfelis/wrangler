%% @hidden
%% @private
-module(refac_tuple_to_record).

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
    [_NameForm, FieldForms] = erl_syntax:attribute_arguments(RecAttrForm),
    FieldNames = [erl_syntax:atom_value(erl_syntax:record_field_name(FieldNameForm))
                  || FieldNameForm <- erl_syntax:tuple_elements(FieldForms)],
    FieldDefVals = [maybe_value(to_term(erl_syntax:record_field_value(FieldForm)))
                  || FieldForm <- erl_syntax:tuple_elements(FieldForms)],
    ?FULL_TD_TP([rule1(RecName, FieldNames, FieldDefVals)],
                [File]).


%% Private functions
%%--------------------------------------------------------------------

rule1(RecName, FieldNames, FieldDefVals)
    when is_atom(RecName), is_list(FieldNames), is_list(FieldDefVals) ->
    ?RULE(?T("{RecName@, Args@@}"), 
          case api_refac:syntax_category(_This@) of
              pattern -> replace_pattern(RecName, FieldNames, Args@@);
              _       -> replace_expr(RecName, FieldNames, FieldDefVals, Args@@)
          end,
          wrangler_syntax:type(RecName@) =:= atom andalso
          wrangler_syntax:atom_value(RecName@) =:= RecName
         ).


replace_pattern(RecName, FieldNames, Args@@) ->
    Fields = mk_record_fields(FieldNames, Args@@),
    wrangler_syntax:record_expr(wrangler_syntax:atom(RecName), Fields).

replace_expr(RecName, FieldNames, FieldDefVals, Args@@) ->
    Fields = mk_record_fields(FieldNames, FieldDefVals, Args@@),
    wrangler_syntax:record_expr(wrangler_syntax:atom(RecName), Fields).


mk_record_field(Name, Val) ->
    wrangler_syntax:record_field(
         wrangler_syntax:atom(Name),
         wrangler_syntax:remove_comments(Val)).

mk_record_fields(RecordFields, Es) ->
    [mk_record_field(Name, Val)
     || {Name, Val} <- lists:zip(RecordFields, Es),
     wrangler_syntax:type(Val) =/= underscore,
     %% non-used _DontWorryVariable
    not (is_leading_underscore_var(Val) andalso
         length(api_refac:var_refs(Val)) =:= 0)].

%% This fun is the same as `mk_record_field/2', but deletes fields
%% with default values.
mk_record_fields(RecordFields, FieldDefVals, Es) ->
    [mk_record_field(Name, Val)
     || {Name, Val, Def} <- lists:zip3(RecordFields, Es, FieldDefVals),
     wrangler_to_term(Val) =/= {value, Def},
     wrangler_syntax:type(Val) =/= underscore].


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


%% General helpers
%% -------------------------------------------------------------------------

%% @doc `Expr' is from `erl_syntax'.
to_term(Expr) ->
    Es = [erl_syntax:revert(Expr)],
    try erl_eval:exprs(Es, [])
    of {value, V, _} -> {value, V}
    catch error:Reason -> {error, Reason}
    end.


%% @doc `Expr' is a wrangler AST.
wrangler_to_term(Expr) ->
    Es = [wrangler_syntax:revert(Expr)],
    try erl_eval:exprs(Es, [])
    of {value, V, _} -> {value, V}
    catch error:Reason -> {error, Reason}
    end.


maybe_value({value, V})       -> V;
maybe_value({error, _Reason}) -> undefined.


is_leading_underscore_var(Var) ->
    wrangler_syntax:type(Var) =:= variable
    andalso $_ =:= hd(atom_to_list(wrangler_syntax:variable_name(Var))).
