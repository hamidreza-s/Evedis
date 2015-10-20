-module(evedis_misc).

-export([rand/2, getrandmax/0, randstr/1, time/0,
	date/0, os/0, echo/1, abort/0, cmd_list/0,
	table_list/0, vedis/0, commit/0, rollback/0,
	'begin'/0]).

%%====================================================================
%% Misc. Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: RAND [min, max]
%% Generate a random (unsigned 32-bit) integer. 
%% @end
%%--------------------------------------------------------------------
rand(Min, Max) ->
    evedis:command(<<"RAND ", Min/binary, " ", Max/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: GETRANDMAX
%% Show largest possible random value.
%% @end
%%--------------------------------------------------------------------
getrandmax() ->
    evedis:command(<<"GETRANDMAX">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: RANDSTR [len]
%% Generate a random string (English alphabet).
%% @end
%%--------------------------------------------------------------------
randstr(Len) ->
    evedis:command(<<"RANDSTR ", Len/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: TIME
%% Return the current time (GMT).
%% @end
%%--------------------------------------------------------------------
time() ->
    evedis:command(<<"TIME">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DATE
%% Return the current date.
%% @end
%%--------------------------------------------------------------------
date() ->
    evedis:command(<<"DATE">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: OS
%% Return a brief description of the host operating system. 
%% @end
%%--------------------------------------------------------------------
os() ->
    evedis:command(<<"OS">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: ECHO message
%% Return the given argument.
%% @end
%%--------------------------------------------------------------------
echo(Msg) ->
    evedis:command(<<"ECHO ", Msg/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: ABORT
%% Throw an error message and abort execution. 
%% @end
%%--------------------------------------------------------------------
abort() ->
    evedis:command(<<"ABORT">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: CMD_LIST
%% Return an indexed array holding the list of installed vedis commands. 
%% @end
%%--------------------------------------------------------------------
cmd_list() ->
    evedis:command(<<"CMD_LIST">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: TABLE_LIST
%% Return an array holding the list of loaded vedis 
%% tables (i.e. Hashes, Sets, List) in memory.
%% @end
%%--------------------------------------------------------------------
table_list() ->
    evedis:command(<<"TABLE_LIST">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: VEDIS
%% Expand the vedis signature and copyright notice.
%% @end
%%--------------------------------------------------------------------
vedis() ->
    evedis:command(<<"VEDIS">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: COMMIT
%% Commit an active write transaction.
%% @end
%%--------------------------------------------------------------------
commit() ->
    evedis:command(<<"COMMIT">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: ROLLBACK
%% Rollback a write transaction. 
%% @end
%%--------------------------------------------------------------------
rollback() ->
    evedis:command(<<"ROLLBACK">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: BEGIN
%% Start a write transaction. 
%% @end
%%--------------------------------------------------------------------
'begin'() ->
    evedis:command(<<"BEGIN">>).
