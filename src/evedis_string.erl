-module(evedis_string).

-export([getcsv/1, strip_tag/1, str_split/1, size_fmt/1,
	 soundex/1, base64/1, base64_dec/1]).

%%====================================================================
%% String Built-in Functions (BIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: GETCSV input [delim, enclosure, escape]
%% Parse a CSV string into a array.
%% @end
%%--------------------------------------------------------------------
getcsv(Input) ->
    evedis:command(<<"GETCSV ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STRIP_TAG input
%% Strip HTML tags from a string. 
%% @end
%%--------------------------------------------------------------------
strip_tag(Input) ->
    evedis:command(<<"STRIP_TAG ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: STR_SPLIT input [split_length = 1]
%% Split a string into an indexed array. 
%% @end
%%--------------------------------------------------------------------
str_split(Input) ->
    evedis:command(<<"STR_SPLIT ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SIZE_FMT int_size 
%% Return a smart string represenation of 
%% the given size [i.e: 64-bit integer]. 
%% @end
%%--------------------------------------------------------------------
size_fmt(IntSize) ->
    evedis:command(<<"SIZE_FMT ", IntSize/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: SOUNDEX string
%% Calculate the soundex key of a string.
%% @end
%%--------------------------------------------------------------------
soundex(String) ->
    evedis:command(<<"SOUNDEX ", String/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: BASE64 input
%% Encode input with MIME base64. 
%% @end
%%--------------------------------------------------------------------
base64(Input) ->
    evedis:command(<<"BASE64 ", Input/binary>>).

%%--------------------------------------------------------------------
%% @doc
%% Evedis command: BASE64_DEC input
%% decode MIME base64 based input. 
%% @end
%%--------------------------------------------------------------------
base64_dec(Input) ->
    evedis:command(<<"BASE64_DEC ", Input/binary>>).
