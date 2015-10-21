-module(evedis_string).

-export([getcsv/2, strip_tag/2, str_split/2, size_fmt/2,
	 soundex/2, base64/2, base64_dec/2]).

%%====================================================================
%% String Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: GETCSV input [delim, enclosure, escape]
%% Parse a CSV string into a array.
%% @end
%%--------------------------------------------------------------------
getcsv(DBName, Input) ->
    evedis:command(DBName, <<"GETCSV ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STRIP_TAG input
%% Strip HTML tags from a string. 
%% @end
%%--------------------------------------------------------------------
strip_tag(DBName, Input) ->
    evedis:command(DBName, <<"STRIP_TAG ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STR_SPLIT input [split_length = 1]
%% Split a string into an indexed array. 
%% @end
%%--------------------------------------------------------------------
str_split(DBName, Input) ->
    evedis:command(DBName, <<"STR_SPLIT ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SIZE_FMT int_size 
%% Return a smart string represenation of 
%% the given size [i.e: 64-bit integer]. 
%% @end
%%--------------------------------------------------------------------
size_fmt(DBName, IntSize) ->
    evedis:command(DBName, <<"SIZE_FMT ", IntSize/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SOUNDEX string
%% Calculate the soundex key of a string.
%% @end
%%--------------------------------------------------------------------
soundex(DBName, String) ->
    evedis:command(DBName, <<"SOUNDEX ", String/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: BASE64 input
%% Encode input with MIME base64. 
%% @end
%%--------------------------------------------------------------------
base64(DBName, Input) ->
    evedis:command(DBName, <<"BASE64 ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: BASE64_DEC input
%% decode MIME base64 based input. 
%% @end
%%--------------------------------------------------------------------
base64_dec(DBName, Input) ->
    evedis:command(DBName, <<"BASE64_DEC ", Input/binary>>).
