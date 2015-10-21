-module(evedis_kv).

-export([get/2, set/3, setnx/3, del/2, 
	 append/3, exists/2, strlen/2, copy/3, 
	 move/3, mget/2, mset/2, msetnx/2,
	 getset/3, incr/2, decr/2, incrby/3,
	 decrby/3]).
 
%%====================================================================
%% Key/Value Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: GET key
%% Get the value of key. If the key does not exist the special 
%% value null is returned.
%% @end
%%--------------------------------------------------------------------
get(DBName, Key) -> 
    evedis:command(DBName, <<"GET ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SET key value
%% Set key to hold the string value.
%% @end
%%--------------------------------------------------------------------
set(DBName, Key, Val) -> 
    evedis:command(DBName, <<"SET ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SETNX key value
%% Set key to hold string value if key does not exist.
%% @end
%%--------------------------------------------------------------------
setnx(DBName, Key, Val) ->
    evedis:command(DBName, <<"SETNX ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DEL key
%% Removes the specified keys. A key is ignored if it does not exist. 
%% @end
%%--------------------------------------------------------------------
del(DBName, Key) -> 
    evedis:command(DBName, <<"DEL ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: APPEND key value
%% If key already exists and is a string, this command appends 
%% the value at the end of the string.
%% @end
%%--------------------------------------------------------------------
append(DBName, Key, Val) ->
     evedis:command(DBName, <<"APPEND ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: EXISTS key
%% Check if a key already exists in the datastore. 
%% @end
%%--------------------------------------------------------------------
exists(DBName, Key) ->
    evedis:command(DBName, <<"EXISTS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STRLEN key
%% Returns the length of the string value stored at key.
%% @end
%%--------------------------------------------------------------------
strlen(DBName, Key) ->
    evedis:command(DBName, <<"STRLEN ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: COPY old_key new_key
%% Copy key values. 
%% @end
%%--------------------------------------------------------------------
copy(DBName, OldKey, NewVal) ->
    evedis:command(DBName, 
		   <<"COPY ", OldKey/binary, " ", NewVal/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MOVE old_key new_key
%% Move key values (remove old key).
%% @end
%%--------------------------------------------------------------------
move(DBName, OldKey, NewVal) ->
    evedis:command(DBName, 
		   <<"MOVE ", OldKey/binary, " ", NewVal/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MGET key [key1, key2, ...]
%% Returns the values of all specified keys.
%% @end
%%--------------------------------------------------------------------
mget(DBName, KeyList) ->
    do_mget(DBName, KeyList, <<"MGET">>).
do_mget(DBName, [Key|Tail], Cmd) ->
    do_mget(DBName, Tail, <<Cmd/binary, " ", Key/binary>>);
do_mget(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MSET key value [key value]
%% Sets the given keys to their respective values.
%% @end
%%--------------------------------------------------------------------
mset(DBName, KeyValList) ->
    do_mset(DBName, KeyValList, <<"MSET">>).
do_mset(DBName, [{Key, Val}|Tail], Cmd) ->
    do_mset(DBName, Tail, <<Cmd/binary, " ", Key/binary, " ", Val/binary>>);
do_mset(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MSETNX key value [key value]
%% Sets the given keys to their respective values.
%% @end
%%--------------------------------------------------------------------
msetnx(DBName, KeyValList) ->
    do_msetnx(DBName, KeyValList, <<"MSETNX">>).
do_msetnx(DBName, [{Key, Val}|Tail], Cmd) ->
    do_msetnx(DBName, Tail, <<Cmd/binary, " ", Key/binary, " ", Val/binary>>);
do_msetnx(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: GETSET key value
%% Atomically sets key to value and returns the old value stored at key.
%% @end
%%--------------------------------------------------------------------
getset(DBName, Key, Val) ->
    evedis:command(DBName, <<"GETSET ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: INCR key
%% Increments the number stored at key by one.
%% @end
%%--------------------------------------------------------------------
incr(DBName, Key) ->
    evedis:command(DBName, <<"INCR ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DECR key
%% Decrements the number stored at key by one.
%% @end
%%--------------------------------------------------------------------
decr(DBName, Key) ->
    evedis:command(DBName, <<"DECR ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: INCRBY key increment
%% Increments the number stored at key by increment.
%% @end
%%--------------------------------------------------------------------
incrby(DBName, Key, Incr) ->
    evedis:command(DBName, <<"INCRBY ", Key/binary, " ", Incr/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DECRBY key decrement
%% Decrements the number stored at key by decrement.
%% @end
%%--------------------------------------------------------------------
decrby(DBName, Key, Decr) ->
    evedis:command(DBName, <<"DECRBY ", Key/binary, " ", Decr/binary>>).
