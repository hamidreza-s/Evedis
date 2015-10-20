-module(evedis_hash).

-export([get/2, set/3, del/2, len/1,
	 exists/2, mget/2, keys/1, vals/1,
	 getall/1, mset/2, setnx/3]).

%%====================================================================
%% Hash Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HGET key field
%% Returns the value associated with field in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
get(Key, Field) ->
    evedis:command(<<"HGET ", Key/binary, " ", Field/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HSET key field value
%% Sets field in the hash stored at key to value. 
%% @end
%%--------------------------------------------------------------------
set(Key, Field, Val) ->
    evedis:command(<<"HSET ", Key/binary, " ", 
		     Field/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HDEL key field [field2,...]
%% Removes the specified fields from the hash stored at key. 
%% @end
%%--------------------------------------------------------------------
del(Key, FieldList) ->
    do_del(FieldList, <<"HDEL ", Key/binary>>).
do_del([Field|Tail], Cmd) ->
    do_del(Tail, <<Cmd/binary, " ", Field/binary>>);
do_del([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HLEN key
%% Returns the number of fields contained in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
len(Key) ->
    evedis:command(<<"HLEN ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HEXISTS key field
%% Returns if field is an existing field in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
exists(Key, Field) ->
    evedis:command(<<"HEXISTS ", Key/binary, " ", Field/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HMGET key field [field2,...]
%% Returns the values associated with the specified fields in the 
%% hash stored at key.
%% @end
%%--------------------------------------------------------------------
mget(Key, FieldList) ->
    do_mget(FieldList, <<"HMGET ", Key/binary>>).
do_mget([Field|Tail], Cmd) ->
    do_mget(Tail, <<Cmd/binary, " ", Field/binary>>);
do_mget([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HKEYS key
%% Returns all field names in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
keys(Key) ->
    evedis:command(<<"HKEYS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HVALS key
%% Returns all field values in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
vals(Key) ->
    evedis:command(<<"HVALS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HGETALL key
%% Returns all fields and values of the hash stored at key.
%% @end
%%--------------------------------------------------------------------
getall(Key) ->
    evedis:command(<<"HGETALL ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HMSET key field value [field1 value1,...]
%% Sets the specified fields to their respective values 
%% in the hash stored at key. 
%% @end
%%--------------------------------------------------------------------
mset(Key, FieldValList) ->
    do_mset(FieldValList, <<"HMSET ", Key/binary>>).
do_mset([{Field, Val}|Tail], Cmd) ->
    do_mset(Tail, <<Cmd/binary, " ", Field/binary, " ", Val/binary>>);
do_mset([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HSETNX key field value
%% Sets field in the hash stored at key to value, 
%% only if field does not yet exist. 
%% @end
%%--------------------------------------------------------------------
setnx(Key, Field, Val) ->
    evedis:command(<<"HSETNX ", Key/binary, " ",
		     Field/binary, " ", Val/binary>>).
