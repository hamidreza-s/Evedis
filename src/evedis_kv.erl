-module(evedis_kv).

-export([get/1, set/2, setnx/2, del/1, 
	 append/2, exists/1, strlen/1, copy/2, 
	 move/2, mget/1, mset/1, msetnx/1,
	 getset/2, incr/1, decr/1, incrby/2,
	 decrby/2]).
 
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
get(Key) -> 
    evedis:command(<<"GET ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SET key value
%% Set key to hold the string value.
%% @end
%%--------------------------------------------------------------------
set(Key, Val) -> 
    evedis:command(<<"SET ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SETNX key value
%% Set key to hold string value if key does not exist.
%% @end
%%--------------------------------------------------------------------
setnx(Key, Val) ->
    evedis:command(<<"SETNX ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DEL key
%% Removes the specified keys. A key is ignored if it does not exist. 
%% @end
%%--------------------------------------------------------------------
del(Key) -> 
    evedis:command(<<"DEL ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: APPEND key value
%% If key already exists and is a string, this command appends 
%% the value at the end of the string.
%% @end
%%--------------------------------------------------------------------
append(Key, Val) ->
     evedis:command(<<"APPEND ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: EXISTS key
%% Check if a key already exists in the datastore. 
%% @end
%%--------------------------------------------------------------------
exists(Key) ->
    evedis:command(<<"EXISTS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STRLEN key
%% Returns the length of the string value stored at key.
%% @end
%%--------------------------------------------------------------------
strlen(Key) ->
    evedis:command(<<"STRLEN ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: COPY old_key new_key
%% Copy key values. 
%% @end
%%--------------------------------------------------------------------
copy(OldKey, NewVal) ->
    evedis:command(<<"COPY ", OldKey/binary, " ", NewVal/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MOVE old_key new_key
%% Move key values (remove old key).
%% @end
%%--------------------------------------------------------------------
move(OldKey, NewVal) ->
    evedis:command(<<"MOVE ", OldKey/binary, " ", NewVal/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MGET key [key1, key2, ...]
%% Returns the values of all specified keys.
%% @end
%%--------------------------------------------------------------------
mget(KeyList) ->
    do_mget(KeyList, <<"MGET">>).
do_mget([Key|Tail], Cmd) ->
    do_mget(Tail, <<Cmd/binary, " ", Key/binary>>);
do_mget([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MSET key value [key value]
%% Sets the given keys to their respective values.
%% @end
%%--------------------------------------------------------------------
mset(KeyValList) ->
    do_mset(KeyValList, <<"MSET">>).
do_mset([{Key, Val}|Tail], Cmd) ->
    do_mset(Tail, <<Cmd/binary, " ", Key/binary, " ", Val/binary>>);
do_mset([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: MSETNX key value [key value]
%% Sets the given keys to their respective values.
%% @end
%%--------------------------------------------------------------------
msetnx(KeyValList) ->
    do_msetnx(KeyValList, <<"MSETNX">>).
do_msetnx([{Key, Val}|Tail], Cmd) ->
    do_msetnx(Tail, <<Cmd/binary, " ", Key/binary, " ", Val/binary>>);
do_msetnx([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: GETSET key value
%% Atomically sets key to value and returns the old value stored at key.
%% @end
%%--------------------------------------------------------------------
getset(Key, Val) ->
    evedis:command(<<"GETSET ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: INCR key
%% Increments the number stored at key by one.
%% @end
%%--------------------------------------------------------------------
incr(Key) ->
    evedis:command(<<"INCR ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DECR key
%% Decrements the number stored at key by one.
%% @end
%%--------------------------------------------------------------------
decr(Key) ->
    evedis:command(<<"DECR ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: INCRBY key increment
%% Increments the number stored at key by increment.
%% @end
%%--------------------------------------------------------------------
incrby(Key, Incr) ->
    evedis:command(<<"INCRBY ", Key/binary, " ", Incr/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: DECRBY key decrement
%% Decrements the number stored at key by decrement.
%% @end
%%--------------------------------------------------------------------
decrby(Key, Decr) ->
    evedis:command(<<"DECRBY ", Key/binary, " ", Decr/binary>>).



