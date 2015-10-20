-module(evedis_list).

-export([index/2, len/1, pop/1, push/2]).

%%====================================================================
%% List Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LINDEX key index
%% Returns the element at index index in the list stored at key.
%% @end
%%--------------------------------------------------------------------
index(Key, Index) ->
    evedis:command(<<"LINDEX ", Key/binary, " ", Index/binary>>).
    
%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LLEN key 
%% Returns the number of fields contained in the list stored at key.
%% @end
%%--------------------------------------------------------------------
len(Key) ->
    evedis:command(<<"LLEN ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LPOP key
%% Removes and returns the first element of the list stored at key.
%% @end
%%--------------------------------------------------------------------
pop(Key) ->
    evedis:command(<<"LPOP ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: LPUSH key value [valuen,...]
%% Insert all the specified values at the head of the list stored at key. 
%% @end
%%--------------------------------------------------------------------
push(Key, ValList) ->
    do_push(ValList, <<"LPUSH ", Key/binary>>).
do_push([Val|Tail], Cmd) ->
    do_push(Tail, <<Cmd/binary, " ", Val/binary>>);
do_push([], Cmd) ->
    evedis:command(Cmd).
