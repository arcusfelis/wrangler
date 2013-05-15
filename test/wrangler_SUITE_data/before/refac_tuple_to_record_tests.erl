-module(refac_tuple_to_record_tests).
-export([ex/1, ex1/1, ex2/1]).

-record(xmlel, {name, children = [], attrs = []}).

ex({xmlel, _N, C, A}) ->
    N1 = 2,
    #xmlel{name = N1, children = C, attrs = A}.

ex1({xmlel, N, C, A}) ->
    #xmlel{name = N, children = C, attrs = A}.

ex2({xmlel, N, _C, _A}) ->
    {xmlel, N, [], []}.
