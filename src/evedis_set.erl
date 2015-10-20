-module(evedis_set).

-export([add/2, ismember/2, pop/1, peek/1,
	 top/1, 'rem'/2, members/1, diff/1, 
	 inter/1, len/1]).

%%====================================================================
%% Set Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SADD key member [member2,...]
%% Add the specified members to the set stored at key.
%% @end
%%--------------------------------------------------------------------
add(Key, MemberList) ->
    do_add(MemberList, <<"SADD ", Key/binary>>).
do_add([Member|Tail], Cmd) ->
    do_add(Tail, <<Cmd/binary, " ", Member/binary>>);
do_add([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SISMEMBER key member
%% Returns if member is a member of the set stored at key.
%% @end
%%--------------------------------------------------------------------
ismember(Key, Member) ->
    evedis:command(<<"SISMEMBER ", Key/binary, " ", Member/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SPOP key
%% Removes and returns the last record from the set value stored at key.
%% @end
%%--------------------------------------------------------------------
pop(Key) ->
    evedis:command(<<"SPOP ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SPEEK key
%% Returns the last record from the set value stored at key.
%% @end
%%--------------------------------------------------------------------
peek(Key) ->
    evedis:command(<<"SPEEK ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STOP key
%% Returns the first record from the set value stored at key.
%% @end
%%--------------------------------------------------------------------
top(Key) ->
    evedis:command(<<"STOP ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SREM key member [member1,...]
%% Remove the specified members from the set stored at key.
%% @end
%%--------------------------------------------------------------------
'rem'(Key, MemberList) ->
    do_rem(MemberList, <<"SREM ", Key/binary>>).
do_rem([Member|Tail], Cmd) ->
    do_rem(Tail, <<Cmd/binary, " ", Member/binary>>);
do_rem([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SMEMBERS key
%% Returns all the members of the set value stored at key.
%% @end
%%--------------------------------------------------------------------
members(Key) ->
    evedis:command(<<"SMEMBERS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SIDFF key [key2,...]
%% Returns the members of the set resulting from the difference 
%% between the first set and all the successive sets.
%% @end
%%--------------------------------------------------------------------
diff(Keys) ->
    do_diff(Keys, <<"SDIFF">>).
do_diff([Key|Tail], Cmd) ->
    do_diff(Tail, <<Cmd/binary, " ", Key/binary>>);
do_diff([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SINTER key [key2,...]
%% Returns the members of the set resulting from the intersection 
%% of all the given sets.
%% @end
%%--------------------------------------------------------------------
inter(Keys) ->
    do_inter(Keys, <<"SINTER">>).
do_inter([Key|Tail], Cmd) ->
    do_inter(Tail, <<Cmd/binary, " ", Key/binary>>);
do_inter([], Cmd) ->
    evedis:command(Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SLEN key 
%% Returns the set cardinality (number of elements) 
%% of the set stored at key.
%% @end
%%--------------------------------------------------------------------
len(Key) ->
    evedis:command(<<"SLEN ", Key/binary>>).
