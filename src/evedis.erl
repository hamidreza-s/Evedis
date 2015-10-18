-module(evedis).

-export([
	 start/0,
	 ping/0,
	 reflect/1,
	 command/1,
	 set/2,
	 get/1
	]).

-export([
	 disk_commit/0
	]).

-define(ERR_NIF_NOT_LOADED, "NIF library not loaded!~n").

start() ->
    ok = application:start(?MODULE),
    ok = load().

load() ->
    PrivRelPath = filename:join("..", "priv"),
    SoName = ?MODULE,
    PrivAbsPath = 
	case code:priv_dir(?MODULE) of
	    {error, bad_name} ->
		Filename = code:which(?MODULE),
		filename:join([filename:dirname(Filename), PrivRelPath]);
	    Path ->
		Path
	end,
    
    SoPath = filename:join(PrivAbsPath, SoName),
    erlang:load_nif(SoPath, application:get_all_env(?MODULE)),
    
    {ok, Storage} = application:get_env(?MODULE, storage),
    case Storage of
	memory ->
	    init();
	disk ->
	    StoragePath = filename:join(PrivAbsPath, "db"),
	    init(list_to_binary(StoragePath))
    end.

init() ->
    ?ERR_NIF_NOT_LOADED.

init(_Storage) ->
    ?ERR_NIF_NOT_LOADED.

ping() ->
    ?ERR_NIF_NOT_LOADED.    

reflect(_BinObj) ->
    ?ERR_NIF_NOT_LOADED.

command(_BinCmd) ->
    ?ERR_NIF_NOT_LOADED.

set(_BinKey, _BinValue) ->
    ?ERR_NIF_NOT_LOADED.

get(_BinKey) ->
    ?ERR_NIF_NOT_LOADED.

disk_commit() ->
    ?ERR_NIF_NOT_LOADED.
