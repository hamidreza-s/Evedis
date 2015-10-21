-module(evedis_SUITE).

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
%% test_evedis
%%--------------------------------------------------------------------
test_evedis(_Config) ->

    %%----------------------------------------------------------------
    %% create and use disk storage database
    %%----------------------------------------------------------------

    ?assertEqual({ok, db1},
		 evedis:create(db1,
			       [{storage_type, disk}, 
				{storage_path, "/tmp/"}])),

    ?assertEqual(<<"true">>, evedis:command(db1, <<"SET foo bar">>)),

    ?assertEqual(<<"bar">>, evedis:command(db1, <<"GET foo">>)),
    
    ?assertEqual({ok, db2}, 
		 evedis:create(db2,
			       [{storage_type, disk}, 
				{storage_path, "/tmp/"}])),
		 
    ?assertEqual(<<"true">>, evedis:command(db2, <<"SET foo ban">>)),
    
    ?assertEqual(<<"ban">>, evedis:command(db2, <<"GET foo">>)),
    

    %%----------------------------------------------------------------
    %% create and use memory storage database
    %%----------------------------------------------------------------

    ?assertEqual({ok, db3},
		 evedis:create(db3, [{storage_type, memory}])),

    ?assertEqual(<<"true">>, evedis:command(db3, <<"SET foo bar">>)),

    ?assertEqual(<<"bar">>, evedis:command(db3, <<"GET foo">>)),
    
    ?assertEqual({ok, db4}, 
		 evedis:create(db4,
			       [{storage_type, memory}])),
		 
    ?assertEqual(<<"true">>, evedis:command(db4, <<"SET foo ban">>)),
    
    ?assertEqual(<<"ban">>, evedis:command(db4, <<"GET foo">>)),
    
    %%----------------------------------------------------------------
    %% create dublicated database
    %%----------------------------------------------------------------

    ?assertEqual(db_already_exists,
		 evedis:create(db1,
			       [{storage_type, disk}, 
				{storage_path, "/tmp/"}])),

    ?assertEqual(db_already_exists,
		 evedis:create(db3, [{storage_type, memory}])),

    %%----------------------------------------------------------------
    %% use a database which does not exist
    %%----------------------------------------------------------------

    ?assertEqual(db_not_found, evedis:command(wrong_db, <<"SET foo ban">>)),

    ok.
