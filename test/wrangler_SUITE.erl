-module(wrangler_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([refac_tuple_to_record/1,
         refac_record_update/1]).


suite() ->
    [{timetrap, {minutes, 5}}].

groups() ->
    Tests = [refac_tuple_to_record,
             refac_record_update],
%   [{main_group, [shuffle], Tests}].
    [{main_group, [], Tests}].

all() ->
    [{group, main_group}].

%% Setup/Teardown
%% ----------------------------------------------------------------------
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(_Config) ->
    application:start(wrangler),
    [].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) -> Config.
end_per_testcase(_, Config) -> Config.


%% Tests
%% ----------------------------------------------------------------------

refac_tuple_to_record(Config) ->
    Name    = "refac_tuple_to_record_tests.erl",
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    BeforeFN         = filename:join([DataDir, "before", Name]),
    ExpectedAfterFN  = filename:join([DataDir, "after", Name]),
    ResultAfterFN    = filename:join([PrivDir, Name]),
    file:copy(BeforeFN, ResultAfterFN),
    run_refac(refac_tuple_to_record, ResultAfterFN, ["xmlel", ResultAfterFN]),
    wrangler_preview_server:commit(),
    compare_files(ExpectedAfterFN, ResultAfterFN).


refac_record_update(Config) ->
    Name    = "refac_record_update_tests.erl",
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    BeforeFN         = filename:join([DataDir, "before", Name]),
    ExpectedAfterFN  = filename:join([DataDir, "after", Name]),
    ResultAfterFN    = filename:join([PrivDir, Name]),
    file:copy(BeforeFN, ResultAfterFN),
    run_refac(refac_record_update, ResultAfterFN, ["xmlel", ResultAfterFN, "XL"]),
    wrangler_preview_server:commit(),
    compare_files(ExpectedAfterFN, ResultAfterFN).


%% Helpers
%% ----------------------------------------------------------------------

run_refac(ModuleName, InFileName, Args) ->
    gen_refac:run_refac(ModuleName,
                        [InFileName, [16,50], [[1,1],[1,1]], Args, [], 8]).


compare_files(ExpectedAfterFN, ResultAfterFN) ->
    {ok, ExpectedAfterAST} = api_refac:get_ast(ExpectedAfterFN),
    {ok, ResultAfterAST}   = api_refac:get_ast(ResultAfterFN),
    IsEqual = api_refac:equal(ExpectedAfterAST, ResultAfterAST),
    [begin
        ct:pal("Run to get diff:~ndiff \"~ts\" \"~ts\"~n", [ExpectedAfterFN, ResultAfterFN]),
        error(not_equal)
     end || not IsEqual],
    ok.
