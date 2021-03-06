-module(evedis_set_SUITE).

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
%% test_evedis_set
%%--------------------------------------------------------------------
test_evedis_set(_Config) ->
    
    %%----------------------------------------------------------------
    %% create database
    %%----------------------------------------------------------------

    ?assertEqual({ok, set_test}, evedis:create(set_test, [])),

    %%----------------------------------------------------------------
    %% add
    %%----------------------------------------------------------------

    ?assertEqual(<<"3">>, evedis_set:add(set_test, 
					 <<"foo">>, [<<"bar">>,
						     <<"bat">>,
						     <<"ban">>])),

    %%----------------------------------------------------------------
    %% len
    %%----------------------------------------------------------------

    ?assertEqual(<<"3">>, evedis_set:len(set_test, <<"foo">>)),

    %%----------------------------------------------------------------
    %% ismember
    %%----------------------------------------------------------------

    ?assertEqual(<<"true">>, evedis_set:ismember(set_test, 
						 <<"foo">>, <<"bat">>)),

    ?assertEqual(<<"false">>, evedis_set:ismember(set_test, 
						  <<"foo">>, <<"wrong">>)),

    %%----------------------------------------------------------------
    %% peek and top
    %%----------------------------------------------------------------
    
    ?assertEqual(<<"ban">>, evedis_set:peek(set_test, <<"foo">>)),

    ?assertEqual(<<"bar">>, evedis_set:top(set_test, <<"foo">>)),

    %%----------------------------------------------------------------
    %% pop and rem
    %%----------------------------------------------------------------

    ?assertEqual(<<"ban">>, evedis_set:pop(set_test, <<"foo">>)),

    ?assertEqual(<<"1">>, evedis_set:'rem'(set_test,
					   <<"foo">>, [<<"bar">>])),

    %%----------------------------------------------------------------
    %% members
    %%----------------------------------------------------------------

    ?assertEqual([<<"bat">>], evedis_set:members(set_test, <<"foo">>)),

    %%----------------------------------------------------------------
    %% add, inter and diff
    %%----------------------------------------------------------------

    ?assertEqual(<<"3">>, evedis_set:add(set_test, 
					 <<"FOO">>, [<<"bar">>,
						     <<"bat">>,
						     <<"ban">>])),

    ?assertEqual([<<"bat">>], 
		 evedis_set:inter(set_test, [<<"FOO">>, <<"foo">>])),

    ?assertEqual([<<"bar">>, <<"ban">>],
		 evedis_set:diff(set_test, [<<"FOO">>, <<"foo">>])),

    ok.
