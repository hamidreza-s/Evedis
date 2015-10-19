-module(evedis_kv).

-export([
	 get/1,
	 set/2,
	 del/1,
	 append/2,
	 exists/1,
	 strlen/1,
	 copy/2,
	 move/2,
	 mget/1,
	 setnx/2,
	 mset/1,
	 msetnx/1,
	 getset/2,
	 incr/1,
	 decr/1,
	 incrby/2,
	 decrby/2
	]).
 
%%====================================================================
%% Key/Value Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% get
%%--------------------------------------------------------------------
get(Key) -> 
    evedis:command(<<"GET ", Key/binary>>).

%%--------------------------------------------------------------------
%% set
%%--------------------------------------------------------------------
set(Key, Val) -> 
    evedis:command(<<"SET ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% del
%%--------------------------------------------------------------------
del(Key) -> 
    evedis:command(<<"DEL ", Key/binary>>).

%%--------------------------------------------------------------------
%% append
%%--------------------------------------------------------------------
append(Key, Val) ->
     evedis:command(<<"APPEND ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% exists
%%--------------------------------------------------------------------
exists(Key) ->
    evedis:command(<<"EXISTS ", Key/binary>>).

%%--------------------------------------------------------------------
%% strlen
%%--------------------------------------------------------------------
strlen(Key) ->
    evedis:command(<<"STRLEN ", Key/binary>>).

%%--------------------------------------------------------------------
%% copy
%%--------------------------------------------------------------------
copy(OldKey, NewVal) ->
    evedis:command(<<"COPY ", OldKey/binary, " ", NewVal/binary>>).

%%--------------------------------------------------------------------
%% move
%%--------------------------------------------------------------------
move(OldKey, NewVal) ->
    evedis:command(<<"MOVE ", OldKey/binary, " ", NewVal/binary>>).

%%--------------------------------------------------------------------
%% mget
%%--------------------------------------------------------------------
mget(KeyList) ->
    mget(KeyList, <<"MGET">>).
mget([Key|Tail], Cmd) ->
    mget(Tail, <<Cmd/binary, " ", Key/binary>>);
mget([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% setnx
%%--------------------------------------------------------------------
setnx(Key, Val) ->
    evedis:command(<<"SETNX ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% mset
%%--------------------------------------------------------------------
mset(KeyValList) ->
    mset(KeyValList, <<"MSET">>).
mset([{Key, Val}|Tail], Cmd) ->
    mset(Tail, <<Cmd/binary, " ", Key/binary, " ", Val/binary>>);
mset([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% msetnx
%%--------------------------------------------------------------------
msetnx(KeyValList) ->
    msetnx(KeyValList, <<"MSETNX">>).
msetnx([{Key, Val}|Tail], Cmd) ->
    msetnx(Tail, <<Cmd/binary, " ", Key/binary, " ", Val/binary>>);
msetnx([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% getset
%%--------------------------------------------------------------------
getset(Key, Val) ->
    evedis:command(<<"GETSET ", Key/binary, " ", Val/binary>>).

%%--------------------------------------------------------------------
%% incr
%%--------------------------------------------------------------------
incr(Key) ->
    evedis:command(<<"INCR ", Key/binary>>).

%%--------------------------------------------------------------------
%% decr
%%--------------------------------------------------------------------
decr(Key) ->
    evedis:command(<<"DECR ", Key/binary>>).

%%--------------------------------------------------------------------
%% incrby
%%--------------------------------------------------------------------
incrby(Key, Incr) ->
    evedis:command(<<"INCRBY ", Key/binary, " ", Incr/binary>>).

%%--------------------------------------------------------------------
%% decrby
%%--------------------------------------------------------------------
decrby(Key, Decr) ->
    evedis:command(<<"DECRBY ", Key/binary, " ", Decr/binary>>).



