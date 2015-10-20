-module(evedis_list_SUITE).

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
%% test_evedis_list
%%--------------------------------------------------------------------
test_evedis_list(_Config) ->
    
    %%----------------------------------------------------------------
    %% push
    %%----------------------------------------------------------------

    ?assertEqual(<<"5">>, evedis_list:push(<<"foo">>, [<<"bar">>,
						       <<"bat">>,
						       <<"ban">>,
						       <<"ban">>,
						       <<"ban">>])),

    %%----------------------------------------------------------------
    %% index
    %%----------------------------------------------------------------

    ?assertEqual(<<"bar">>, evedis_list:index(<<"foo">>, <<"0">>)),
    ?assertEqual(<<"bat">>, evedis_list:index(<<"foo">>, <<"1">>)),
    ?assertEqual(<<"ban">>, evedis_list:index(<<"foo">>, <<"2">>)),
    ?assertEqual(<<"ban">>, evedis_list:index(<<"foo">>, <<"3">>)),
    ?assertEqual(<<"ban">>, evedis_list:index(<<"foo">>, <<"4">>)),

    %%----------------------------------------------------------------
    %% pop
    %%----------------------------------------------------------------
    
    ?assertEqual(<<"bar">>, evedis_list:pop(<<"foo">>)),

    %%----------------------------------------------------------------
    %% len
    %%----------------------------------------------------------------

    ?assertEqual(<<"4">>, evedis_list:len(<<"foo">>)),

    ok.
