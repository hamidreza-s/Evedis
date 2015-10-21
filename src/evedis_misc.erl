-module(evedis_misc).

-export([rand/3, getrandmax/1, randstr/2, time/1,
	date/1, os/1, echo/2, abort/1, cmd_list/1,
	table_list/1, vedis/1, commit/1, rollback/1,
	'begin'/1]).

%%====================================================================
%% Misc. Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: RAND [min, max]
%% Generate a random (unsigned 32-bit) integer. 
%% @end
%%--------------------------------------------------------------------
rand(DBName, Min, Max) ->
    evedis:command(DBName, <<"RAND ", Min/binary, " ", Max/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: GETRANDMAX
%% Show largest possible random value.
%% @end
%%--------------------------------------------------------------------
getrandmax(DBName) ->
    evedis:command(DBName, <<"GETRANDMAX">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: RANDSTR [len]
%% Generate a random string (English alphabet).
%% @end
%%--------------------------------------------------------------------
randstr(DBName, Len) ->
    evedis:command(DBName, <<"RANDSTR ", Len/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: TIME
%% Return the current time (GMT).
%% @end
%%--------------------------------------------------------------------
time(DBName) ->
    evedis:command(DBName, <<"TIME">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DATE
%% Return the current date.
%% @end
%%--------------------------------------------------------------------
date(DBName) ->
    evedis:command(DBName, <<"DATE">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: OS
%% Return a brief description of the host operating system. 
%% @end
%%--------------------------------------------------------------------
os(DBName) ->
    evedis:command(DBName, <<"OS">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: ECHO message
%% Return the given argument.
%% @end
%%--------------------------------------------------------------------
echo(DBName, Msg) ->
    evedis:command(DBName, <<"ECHO ", Msg/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: ABORT
%% Throw an error message and abort execution. 
%% @end
%%--------------------------------------------------------------------
abort(DBName) ->
    evedis:command(DBName, <<"ABORT">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: CMD_LIST
%% Return an indexed array holding the list of installed vedis commands. 
%% @end
%%--------------------------------------------------------------------
cmd_list(DBName) ->
    evedis:command(DBName, <<"CMD_LIST">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: TABLE_LIST
%% Return an array holding the list of loaded vedis 
%% tables (i.e. Hashes, Sets, List) in memory.
%% @end
%%--------------------------------------------------------------------
table_list(DBName) ->
    evedis:command(DBName, <<"TABLE_LIST">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: VEDIS
%% Expand the vedis signature and copyright notice.
%% @end
%%--------------------------------------------------------------------
vedis(DBName) ->
    evedis:command(DBName, <<"VEDIS">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: COMMIT
%% Commit an active write transaction.
%% @end
%%--------------------------------------------------------------------
commit(DBName) ->
    evedis:command(DBName, <<"COMMIT">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: ROLLBACK
%% Rollback a write transaction. 
%% @end
%%--------------------------------------------------------------------
rollback(DBName) ->
    evedis:command(DBName, <<"ROLLBACK">>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: BEGIN
%% Start a write transaction. 
%% @end
%%--------------------------------------------------------------------
'begin'(DBName) ->
    evedis:command(DBName, <<"BEGIN">>).
