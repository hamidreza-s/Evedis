-module(evedis_string_SUITE).

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
%% test_evedis_string
%%--------------------------------------------------------------------
test_evedis_string(_Config) ->
    
    %%----------------------------------------------------------------
    %% create database
    %%----------------------------------------------------------------

    ?assertEqual({ok, string_test}, evedis:create(string_test, [])),

    %%----------------------------------------------------------------
    %% getcsv
    %%----------------------------------------------------------------

    ?assertEqual([<<"foo">>, <<"bar">>, <<"bat">>],
		 evedis_string:getcsv(string_test, <<"foo,bar,bat">>)),

    %%----------------------------------------------------------------
    %% strip_tag
    %%----------------------------------------------------------------

    ?assertEqual(<<"foo">>, 
		 evedis_string:strip_tag(
		   string_test, 
		   <<"<html><body>foo</body></html>">>)),
    
    %%----------------------------------------------------------------
    %% str_split
    %%----------------------------------------------------------------

    ?assertEqual([<<"f">>, <<"o">>, <<"o">>],
		 evedis_string:str_split(string_test, <<"foo">>)),

    %%----------------------------------------------------------------
    %% size_fmt
    %%----------------------------------------------------------------

    ?assertEqual(<<"0.1 KB">>, evedis_string:size_fmt(string_test, 
						      <<"99">>)),

    ?assertEqual(<<"976.5 KB">>, evedis_string:size_fmt(string_test, 
							<<"999999">>)),

    ?assertEqual(<<"93.1 GB">>, evedis_string:size_fmt(string_test, 
						       <<"99999999999">>)),

    %%----------------------------------------------------------------
    %% soundex
    %%----------------------------------------------------------------
    
    ?assertEqual(<<"F000">>, evedis_string:soundex(string_test, 
						   <<"foo">>)),

    ?assertEqual(<<"B600">>, evedis_string:soundex(string_test, 
						   <<"bar">>)),
    
    %%----------------------------------------------------------------
    %% base64 and base64_dec
    %%----------------------------------------------------------------

    ?assertEqual(<<"Zm9v">>, evedis_string:base64(string_test, 
						  <<"foo">>)),

    ?assertEqual(<<"foo">>, evedis_string:base64_dec(string_test, 
						     <<"Zm9v">>)),

    ok.
