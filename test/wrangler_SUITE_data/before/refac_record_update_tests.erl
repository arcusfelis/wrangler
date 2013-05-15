-module(refac_record_update_tests).
-export([ex/2, ex1/1, ex2/1, ex3/1, ex4/0, ex5/1, ex6/1, ex7/1, ex8/1]).

-record(xmlel, {name, children = [], attrs = []}).

ex(C, A) ->
    N1 = 2,
    #xmlel{name = N1, children = C, attrs = A}.

ex1(#xmlel{name = N, children = C, attrs = A}) ->
    #xmlel{name = N, children = C, attrs = A}.

ex2(#xmlel{name = N = 1, children = C, attrs = A}) ->
    #xmlel{name = N, children = C, attrs = A}.

ex3(El) ->
    #xmlel{name = N, children = C, attrs = A} = El,
    #xmlel{name = N, children = C, attrs = A}.

ex4() ->
    Child = #xmlel{name = x, children = [a,b], attrs = [{a,b}]},
    #xmlel{name = x, children = [Child]}.

ex5(El) ->
    #xmlel{} = El,
    El#xmlel{}.

ex6(El) ->
    #xmlel{name = N, children = C, attrs = A} = El,
    tuple_to_list(#xmlel{name = N, children = C, attrs = [A]}).

ex7(El) ->
    #xmlel{name = N,
           children = C,
           attrs = A} = El,
    tuple_to_list(#xmlel{name = N,
                         children = C,
                         attrs = [#xmlel{name = <<"looooooooooooooooooooooongName">>,
                                         children = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]}]}).


ex8(#xmlel{name = _N, children = C, attrs = A}) ->
    #xmlel{name = <<"mess">>, children = C, attrs = A}.
