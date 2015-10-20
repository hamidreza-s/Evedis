-module(evedis_kv_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%%====================================================================
%% CT Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% suite | groups | all
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

groups() -> [].

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

%%--------------------------------------------------------------------
%% init_per_suite | end_per_suite
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    evedis:start(),
    Config.

end_per_suite(_Config) ->
    evedis:stop(),
    ok.

%%--------------------------------------------------------------------
%% init_per_group | end_per_group
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
    Config.

end_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% init_per_testcase | end_per_testcase
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% test_evedis_kv
%%--------------------------------------------------------------------
test_evedis_kv(_Config) ->

    %%----------------------------------------------------------------
    %% set and get various key values
    %%----------------------------------------------------------------

    Key1 = <<"K">>,
    Val1 = <<"V">>,

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

    ?assertEqual(<<"true">>, evedis_kv:set(Key1, Val1)),
    ?assertEqual(Val1, evedis_kv:get(Key1)),

    ?assertEqual(<<"true">>, evedis_kv:set(Key2, Val2)),
    ?assertEqual(Val2, evedis_kv:get(Key2)),

    ?assertEqual(<<"true">>, evedis_kv:set(Key3, Val3)),
    ?assertEqual(Val3, evedis_kv:get(Key3)),

    ?assertEqual(<<"true">>, evedis_kv:set(Key4, Val4)),
    ?assertEqual(Val4, evedis_kv:get(Key4)),

    ?assertEqual(<<"true">>, evedis_kv:set(Key5, Val5)),
    ?assertEqual(Val5, evedis_kv:get(Key5)),

    ?assertEqual(<<"true">>, evedis_kv:set(Key6, Val6)),
    ?assertEqual(Val6, evedis_kv:get(Key6)),

    ?assertEqual(<<"true">>, evedis_kv:set(Key7, Val7)),
    ?assertEqual(Val7, evedis_kv:get(Key7)),

    %%----------------------------------------------------------------
    %% set, setnx, get and del commands
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_kv:set(<<"-k1-">>, <<"-v1-">>)),
    ?assertEqual(<<"false">>, evedis_kv:setnx(<<"-k1-">>, <<"-v1-">>)),
    ?assertEqual(<<"-v1-">>, evedis_kv:get(<<"-k1-">>)),

    ?assertEqual(<<"1">>, evedis_kv:del(<<"-k1-">>)),
    ?assertEqual(not_found, evedis_kv:get(<<"-k1-">>)),

    %%----------------------------------------------------------------
    %% set, get and append
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_kv:set(<<"-k2-">>, <<"-v2-">>)),
    ?assertEqual(<<"-v2-">>, evedis_kv:get(<<"-k2-">>)),

    ?assertEqual(<<"5">>, evedis_kv:append(<<"-k2-">>, <<"+">>)),
    ?assertEqual(<<"-v2-+">>, evedis_kv:get(<<"-k2-">>)),

    %%----------------------------------------------------------------
    %% set and exists
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_kv:set(<<"-k3-">>, <<"-v3-">>)),
    ?assertEqual(<<"true">>, evedis_kv:exists(<<"-k3-">>)),
    ?assertEqual(<<"false">>, evedis_kv:exists(<<"=k3=">>)),
    
    %%----------------------------------------------------------------
    %% set and strlen
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_kv:set(<<"-k4-">>, <<"-v4-">>)),
    ?assertEqual(<<"4">>, evedis_kv:strlen(<<"-k4-">>)),

    %%----------------------------------------------------------------
    %% set, copy and move
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_kv:set(<<"-k5-">>, <<"-v5-">>)),
    ?assertEqual(<<"true">>, evedis_kv:copy(<<"-k5-">>, <<"-k6-">>)),
    ?assertEqual(evedis_kv:get(<<"-k5-">>), evedis_kv:get(<<"-k6-">>)),

    ?assertEqual(<<"true">>, evedis_kv:move(<<"-k6-">>, <<"-k7-">>)),
    ?assertEqual(<<"false">>, evedis_kv:exists(<<"-k6-">>)),
    ?assertEqual(<<"-v5-">>, evedis_kv:get(<<"-k7-">>)),

    %%----------------------------------------------------------------
    %% mset, msetnx, mget
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_kv:mset([{<<"-k8-">>, <<"-v8-">>},
					     {<<"-k9-">>, <<"-v9-">>}])),

    ?assertEqual(<<"true">>, evedis_kv:msetnx([{<<"-k8-">>, <<"=v8=">>}])),

    ?assertEqual([<<"-v8-">>, <<"-v9-">>, not_found],
		 evedis_kv:mget([<<"-k8-">>, <<"-k9-">>, <<"=k9=">>])),

    %%----------------------------------------------------------------
    %% getset
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_kv:set(<<"-k10-">>, <<"-v10-">>)),
    ?assertEqual(<<"-v10-">>, evedis_kv:getset(<<"-k10-">>, 
					       <<"-v10.1-">>)),
    ?assertEqual(<<"-v10.1-">>, evedis_kv:getset(<<"-k10-">>,
						 <<"-v10.2-">>)),
    ?assertEqual(<<"-v10.2-">>, evedis_kv:get(<<"-k10-">>)),

    %%----------------------------------------------------------------
    %% incr, decr, incrby and decrby
    %%----------------------------------------------------------------

    ?assertEqual(<<"1">>, evedis_kv:incr(<<"-k11-">>)),
    ?assertEqual(<<"2">>, evedis_kv:incr(<<"-k11-">>)),
    ?assertEqual(<<"3">>, evedis_kv:incr(<<"-k11-">>)),

    ?assertEqual(<<"2">>, evedis_kv:decr(<<"-k11-">>)),
    ?assertEqual(<<"1">>, evedis_kv:decr(<<"-k11-">>)),
    ?assertEqual(<<"0">>, evedis_kv:decr(<<"-k11-">>)),

    ?assertEqual(<<"10">>, evedis_kv:incrby(<<"-k11-">>, <<"10">>)),
    ?assertEqual(<<"8">>, evedis_kv:decrby(<<"-k11-">>, <<"2">>)),

    ok.
