-module(evedis).

-export([
	 start/0,
	 ping/0,
	 reflect/1,
	 set/2,
	 get/1
	]).

-export([
	 disk_commit/0
	]).

-define(ERR_NIF_NOT_LOADED, "NIF library not loaded!~n").

start() ->
    ok = load(),
    ok = init(),
    ok.

load() ->
    PrivDir = filename:join("..", "priv"),
    SoName = ?MODULE,
    SoPath = case code:priv_dir(?MODULE) of
		 {error, bad_name} ->
		     case code:which(?MODULE) of
			 Filename when is_list(Filename) ->
			     filename:join([filename:dirname(Filename),
					   PrivDir, SoName]);
			 _ ->
			     filename:join(PrivDir, SoName)
		     end;
		 Dir ->
		     filename:join(Dir, SoName)
	     end,
    erlang:load_nif(SoPath, application:get_all_env(?MODULE)).

init() ->
    ?ERR_NIF_NOT_LOADED.

ping() ->
    ?ERR_NIF_NOT_LOADED.    

reflect(_BinObj) ->
    ?ERR_NIF_NOT_LOADED.

set(_BinKey, _BinValue) ->
    ?ERR_NIF_NOT_LOADED.

get(_BinKey) ->
    ?ERR_NIF_NOT_LOADED.

disk_commit() ->
    ?ERR_NIF_NOT_LOADED.




