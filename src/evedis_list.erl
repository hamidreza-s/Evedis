-module(evedis_list).

-export([index/3, len/2, pop/2, push/3]).

%%====================================================================
%% List Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LINDEX key index
%% Returns the element at index index in the list stored at key.
%% @end
%%--------------------------------------------------------------------
index(DBName, Key, Index) ->
    evedis:command(DBName, <<"LINDEX ", Key/binary, " ", Index/binary>>).
    
%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LLEN key 
%% Returns the number of fields contained in the list stored at key.
%% @end
%%--------------------------------------------------------------------
len(DBName, Key) ->
    evedis:command(DBName, <<"LLEN ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LPOP key
%% Removes and returns the first element of the list stored at key.
%% @end
%%--------------------------------------------------------------------
pop(DBName, Key) ->
    evedis:command(DBName, <<"LPOP ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LPUSH key value [valuen,...]
%% Insert all the specified values at the head of the list stored at key. 
%% @end
%%--------------------------------------------------------------------
push(DBName, Key, ValList) ->
    do_push(DBName, ValList, <<"LPUSH ", Key/binary>>).
do_push(DBName, [Val|Tail], Cmd) ->
    do_push(DBName, Tail, <<Cmd/binary, " ", Val/binary>>);
do_push(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).
