-module(evedis_set).

-export([add/3, ismember/3, pop/2, peek/2,
	 top/2, 'rem'/3, members/2, diff/2, 
	 inter/2, len/2]).

%%====================================================================
%% Set Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SADD key member [member2,...]
%% Add the specified members to the set stored at key.
%% @end
%%--------------------------------------------------------------------
add(DBName, Key, MemberList) ->
    do_add(DBName, MemberList, <<"SADD ", Key/binary>>).
do_add(DBName, [Member|Tail], Cmd) ->
    do_add(DBName, Tail, <<Cmd/binary, " ", Member/binary>>);
do_add(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SISMEMBER key member
%% Returns if member is a member of the set stored at key.
%% @end
%%--------------------------------------------------------------------
ismember(DBName, Key, Member) ->
    evedis:command(DBName, 
		   <<"SISMEMBER ", Key/binary, " ", Member/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SPOP key
%% Removes and returns the last record from the set value stored at key.
%% @end
%%--------------------------------------------------------------------
pop(DBName, Key) ->
    evedis:command(DBName, <<"SPOP ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SPEEK key
%% Returns the last record from the set value stored at key.
%% @end
%%--------------------------------------------------------------------
peek(DBName, Key) ->
    evedis:command(DBName, <<"SPEEK ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STOP key
%% Returns the first record from the set value stored at key.
%% @end
%%--------------------------------------------------------------------
top(DBName, Key) ->
    evedis:command(DBName, <<"STOP ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SREM key member [member1,...]
%% Remove the specified members from the set stored at key.
%% @end
%%--------------------------------------------------------------------
'rem'(DBName, Key, MemberList) ->
    do_rem(DBName, MemberList, <<"SREM ", Key/binary>>).
do_rem(DBName, [Member|Tail], Cmd) ->
    do_rem(DBName, Tail, <<Cmd/binary, " ", Member/binary>>);
do_rem(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SMEMBERS key
%% Returns all the members of the set value stored at key.
%% @end
%%--------------------------------------------------------------------
members(DBName, Key) ->
    evedis:command(DBName, <<"SMEMBERS ", Key/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SIDFF key [key2,...]
%% Returns the members of the set resulting from the difference 
%% between the first set and all the successive sets.
%% @end
%%--------------------------------------------------------------------
diff(DBName, Keys) ->
    do_diff(DBName, Keys, <<"SDIFF">>).
do_diff(DBName, [Key|Tail], Cmd) ->
    do_diff(DBName, Tail, <<Cmd/binary, " ", Key/binary>>);
do_diff(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SINTER key [key2,...]
%% Returns the members of the set resulting from the intersection 
%% of all the given sets.
%% @end
%%--------------------------------------------------------------------
inter(DBName, Keys) ->
    do_inter(DBName, Keys, <<"SINTER">>).
do_inter(DBName, [Key|Tail], Cmd) ->
    do_inter(DBName, Tail, <<Cmd/binary, " ", Key/binary>>);
do_inter(DBName, [], Cmd) ->
    evedis:command(DBName, Cmd).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SLEN key 
%% Returns the set cardinality (number of elements) 
%% of the set stored at key.
%% @end
%%--------------------------------------------------------------------
len(DBName, Key) ->
    evedis:command(DBName, <<"SLEN ", Key/binary>>).
