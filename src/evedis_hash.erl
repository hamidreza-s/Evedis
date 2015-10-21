-module(evedis_hash).

-export([get/3, set/4, del/3, len/2,
	 exists/3, mget/3, keys/2, vals/2,
	 getall/2, mset/3, setnx/4]).

%%====================================================================
%% Hash Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HGET key field
%% Returns the value associated with field in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
get(DBName, Key, Field) ->
    evedis:command(DBName, 
		   <<"HGET ", Key/binary, " ", Field/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HSET key field value
%% Sets field in the hash stored at key to value. 
%% @end
%%--------------------------------------------------------------------
set(DBName, Key, Field, Val) ->
    evedis:command(DBName, 
		   <<"HSET ", Key/binary, " ", 
		     Field/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HDEL key field [field2,...]
%% Removes the specified fields from the hash stored at key. 
%% @end
%%--------------------------------------------------------------------
del(DBName, Key, FieldList) ->
    do_del(DBName, FieldList, <<"HDEL ", Key/binary>>).
do_del(DBName, [Field|Tail], Cmd) ->
    do_del(DBName, Tail, <<Cmd/binary, " ", Field/binary>>);
do_del(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HLEN key
%% Returns the number of fields contained in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
len(DBName, Key) ->
    evedis:command(DBName, <<"HLEN ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Vedis command: HEXISTS key field
%% Returns if field is an existing field in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
exists(DBName, Key, Field) ->
    evedis:command(DBName, 
		   <<"HEXISTS ", Key/binary, " ", Field/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HMGET key field [field2,...]
%% Returns the values associated with the specified fields in the 
%% hash stored at key.
%% @end
%%--------------------------------------------------------------------
mget(DBName, Key, FieldList) ->
    do_mget(DBName, FieldList, <<"HMGET ", Key/binary>>).
do_mget(DBName, [Field|Tail], Cmd) ->
    do_mget(DBName, Tail, <<Cmd/binary, " ", Field/binary>>);
do_mget(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HKEYS key
%% Returns all field names in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
keys(DBName, Key) ->
    evedis:command(DBName, <<"HKEYS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HVALS key
%% Returns all field values in the hash stored at key.
%% @end
%%--------------------------------------------------------------------
vals(DBName, Key) ->
    evedis:command(DBName, <<"HVALS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HGETALL key
%% Returns all fields and values of the hash stored at key.
%% @end
%%--------------------------------------------------------------------
getall(DBName, Key) ->
    evedis:command(DBName, <<"HGETALL ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HMSET key field value [field1 value1,...]
%% Sets the specified fields to their respective values 
%% in the hash stored at key. 
%% @end
%%--------------------------------------------------------------------
mset(DBName, Key, FieldValList) ->
    do_mset(DBName, FieldValList, <<"HMSET ", Key/binary>>).
do_mset(DBName, [{Field, Val}|Tail], Cmd) ->
    do_mset(DBName, Tail, 
	    <<Cmd/binary, " ", Field/binary, " ", Val/binary>>);
do_mset(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: HSETNX key field value
%% Sets field in the hash stored at key to value, 
%% only if field does not yet exist. 
%% @end
%%--------------------------------------------------------------------
setnx(DBName, Key, Field, Val) ->
    evedis:command(DBName, 
		   <<"HSETNX ", Key/binary, " ",
		     Field/binary, " ", Val/binary>>).
