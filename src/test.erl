-module(test).
-compile(export_all).

a() ->
    Key0 = <<"K">>,
    Val0 = <<"V">>,

    Key1 = <<"=----key----=">>,
    Val1 = <<"=----val----=">>,
    
    Key2 = <<"=----KEY----=">>,
    Val2 = <<"=----VAL----=">>,

    Key3 = <<"K1">>,
    Val3 = <<"=----V1----=">>,

    Key4 = <<"1">>,
    Val4 = <<"1">>,

    Key5 = <<"1111">>,
    Val5 = <<"1111">>,
    
    Key6 = <<"=----K1----=">>,
    Val6 = <<"V1">>,

    Key7 = <<"=---------------------K----------------------------=">>,
    Val7 = <<"=---------------------V----------------------------=">>,

    R0 = evedis:command(<<"SET ", Key0/binary, " ", Val0/binary>>),
    R1 = evedis:command(<<"SET ", Key1/binary, " ", Val1/binary>>),
    R2 = evedis:command(<<"SET ", Key2/binary, " ", Val2/binary>>),
    R3 = evedis:command(<<"SET ", Key3/binary, " ", Val3/binary>>),
    R4 = evedis:command(<<"SET ", Key4/binary, " ", Val4/binary>>),
    R5 = evedis:command(<<"SET ", Key5/binary, " ", Val5/binary>>),
    R6 = evedis:command(<<"SET ", Key6/binary, " ", Val6/binary>>),
    R7 = evedis:command(<<"SET ", Key7/binary, " ", Val7/binary>>),

    {{R0, evedis:command(<<"GET ", Key0/binary>>)},
     {R1, evedis:command(<<"GET ", Key1/binary>>)},
     {R2, evedis:command(<<"GET ", Key2/binary>>)},
     {R3, evedis:command(<<"GET ", Key3/binary>>)},
     {R4, evedis:command(<<"GET ", Key4/binary>>)},
     {R5, evedis:command(<<"GET ", Key5/binary>>)},
     {R6, evedis:command(<<"GET ", Key6/binary>>)},
     {R7, evedis:command(<<"GET ", Key7/binary>>)}}.

b(MaxRec, MaxVal) ->
    Val = list_to_binary([$a || _ <- lists:seq(1, MaxVal)]),
    timer:tc(
      fun() ->
	      lists:map(
		fun(I) ->
			Key = integer_to_binary(I),
			Cmd = <<"SET ", Key/binary, 
				" ", Val/binary>>,

			{Cmd, <<"true">> = evedis:command(Cmd)}
		end,
		lists:seq(1, MaxRec)
	       )
      end).
