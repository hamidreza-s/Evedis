-module(test).
-compile(export_all).

test_1() ->
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

    Key7 = <<"=-----------------------------------K----------------------------------------=">>,
    Val7 = <<"=-----------------------------------V----------------------------------------=">>,

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

test_2() ->
    Val = <<"v">>,
    evedis:reflect(Val).
