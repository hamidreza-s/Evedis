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

c() ->
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

    R0 = evedis:set(Key0, Val0),
    R1 = evedis:set(Key1, Val1),
    R2 = evedis:set(Key2, Val2),
    R3 = evedis:set(Key3, Val3),
    R4 = evedis:set(Key4, Val4),
    R5 = evedis:set(Key5, Val5),
    R6 = evedis:set(Key6, Val6),
    R7 = evedis:set(Key7, Val7),

    {{R0, evedis:get(Key0)},
     {R1, evedis:get(Key1)},
     {R2, evedis:get(Key2)},
     {R3, evedis:get(Key3)},
     {R4, evedis:get(Key4)},
     {R5, evedis:get(Key5)},
     {R6, evedis:get(Key6)},
     {R7, evedis:get(Key7)}}.

d(MaxRec, MaxVal) ->
    Val = list_to_binary([$a || _ <- lists:seq(1, MaxVal)]),
    timer:tc(fun() ->
		     [ok = evedis:set(integer_to_binary(I), 
				      <<"\"", Val/binary, "\"">>) ||
			 I <- lists:seq(1, MaxRec)]
	     end).
