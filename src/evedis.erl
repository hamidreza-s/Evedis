-module(evedis).

-export([
	 start/0,
	 ping/0,
	 command/1
	]).

-define(ERR_NIF_NOT_LOADED, "NIF library not loaded!~n").

%%====================================================================
%% Module Management
%%====================================================================

%%--------------------------------------------------------------------
%% start
%%--------------------------------------------------------------------
start() ->
    ok = application:start(?MODULE),
    ok = load().

%%--------------------------------------------------------------------
%% load
%%--------------------------------------------------------------------
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

%%====================================================================
%% Native Implemented Functions (NIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% init
%%--------------------------------------------------------------------
init() ->
    ?ERR_NIF_NOT_LOADED.

init(_Storage) ->
    ?ERR_NIF_NOT_LOADED.

%%--------------------------------------------------------------------
%% ping
%%--------------------------------------------------------------------
ping() ->
    ?ERR_NIF_NOT_LOADED.    

%%--------------------------------------------------------------------
%% command
%%--------------------------------------------------------------------
command(_BinCmd) ->
    ?ERR_NIF_NOT_LOADED.
