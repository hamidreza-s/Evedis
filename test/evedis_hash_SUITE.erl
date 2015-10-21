-module(evedis_hash_SUITE).

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
%% test_evedis_hash
%%--------------------------------------------------------------------
test_evedis_hash(_Config) ->
    
    %%----------------------------------------------------------------
    %% create database
    %%----------------------------------------------------------------

    ?assertEqual({ok, hash_test}, evedis:create(hash_test, [])),

    %%----------------------------------------------------------------
    %% set and setx
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_hash:set(hash_test,
					     <<"id-181">>,
					     <<"name">>,
					     <<"Hamidreza">>)),


    ?assertEqual(<<"false">>, evedis_hash:setnx(hash_test,
						<<"id-181">>,
						<<"name">>,
						<<"Hamidreza">>)),

    ?assertEqual(<<"true">>, evedis_hash:set(hash_test,
					     <<"id-181">>,
					     <<"age">>,
					     <<"28">>)),

    ?assertEqual(<<"true">>, evedis_hash:set(hash_test,
					     <<"id-181">>,
					     <<"language">>,
					     <<"Persian">>)), 

    %%----------------------------------------------------------------
    %% get
    %%----------------------------------------------------------------

    ?assertEqual(<<"Hamidreza">>, evedis_hash:get(hash_test,
						  <<"id-181">>,
						  <<"name">>)),

    ?assertEqual(<<"28">>, evedis_hash:get(hash_test,
					   <<"id-181">>,
					   <<"age">>)),
    
    ?assertEqual(not_found, evedis_hash:get(hash_test,
					    <<"id-181">>,
					    <<"job">>)),

    %%----------------------------------------------------------------
    %% del
    %%----------------------------------------------------------------

    ?assertEqual(<<"1">>, evedis_hash:del(hash_test, 
					  <<"id-181">>, 
					  [<<"age">>])),

    ?assertEqual(not_found, evedis_hash:get(hash_test, 
					    <<"id-181">>, 
					    <<"age">>)),

    %%----------------------------------------------------------------
    %% len
    %%----------------------------------------------------------------

    ?assertEqual(<<"2">>, evedis_hash:len(hash_test, <<"id-181">>)),

    %%----------------------------------------------------------------
    %% exists
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_hash:exists(hash_test, 
						<<"id-181">>, 
						<<"name">>)),

    ?assertEqual(<<"false">>, evedis_hash:exists(hash_test,
						 <<"id-181">>, 
						 <<"job">>)),

    %%----------------------------------------------------------------
    %% keys
    %%----------------------------------------------------------------

    ?assertEqual([<<"name">>, <<"language">>], 
		 evedis_hash:keys(hash_test, <<"id-181">>)),

    %%----------------------------------------------------------------
    %% vals
    %%----------------------------------------------------------------
    ?assertEqual([<<"Hamidreza">>, <<"Persian">>],
		 evedis_hash:vals(hash_test, <<"id-181">>)),

    %%----------------------------------------------------------------
    %% getall
    %%----------------------------------------------------------------

    ?assertEqual([<<"name">>, <<"Hamidreza">>, 
		  <<"language">>, <<"Persian">>],
		 evedis_hash:getall(hash_test, <<"id-181">>)),

    %%----------------------------------------------------------------
    %% mset
    %%----------------------------------------------------------------

    ?assertEqual(<<"2">>, evedis_hash:mset(hash_test,
					   <<"id-191">>,
					   [{<<"-k1-">>, <<"-v1-">>},
					    {<<"-k2-">>, <<"-v2-">>}])),

    %%----------------------------------------------------------------
    %% mget
    %%----------------------------------------------------------------

    ?assertEqual([<<"-v1-">>, <<"-v2-">>],
		 evedis_hash:mget(hash_test,
				  <<"id-191">>, 
				  [<<"-k1-">>, <<"-k2-">>])),

    ok.
