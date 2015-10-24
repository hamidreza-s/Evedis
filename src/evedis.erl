-module(evedis).

-export([start/0, stop/0, load_shared_object/0]).
-export([create/2, command/2, close/1]).

-define(PTR_TAB, vedis_pointer_table).
-define(PTR_TAB_OPTS, [named_table, set, public]).
-define(ERR_NIF_NOT_LOADED, "NIF library not loaded!~n").

%%====================================================================
%% Module Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start evedis wrapper and creates pointer table
%% @end
%%--------------------------------------------------------------------
start() ->
    case load_shared_object() of
	ok ->
	    ok;
	{error, {reload, _}} ->
	    ok
    end,
    ets:new(?PTR_TAB, ?PTR_TAB_OPTS),
    ets:setopts(?PTR_TAB, {heir, whereis(application_controller), []}),
    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% Stop evedis wrapper and clear pointer table
%% @end
%%--------------------------------------------------------------------
stop() ->
    ets:delete(?PTR_TAB),

    %% @TODO: close each vedis database

    ok.

%%====================================================================
%% Vedis shared-object (SO) loader
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Load evedis shared object (evedis.so)
%% @end
%%--------------------------------------------------------------------
load_shared_object() ->
    SoName = ?MODULE,
    SoPath = filename:join(priv_abs_path(), SoName),
    erlang:load_nif(SoPath, application:get_all_env(?MODULE)).

%%====================================================================
%% Vedis DB API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create (or load) database by its name
%% @end
%%--------------------------------------------------------------------
create(DBName, Opts) ->    
    case ets:lookup(?PTR_TAB, DBName) of
	[_] ->
	    db_already_exists;
	_ ->
	    do_create(DBName, Opts)
    end.

do_create(DBName, Opts) ->
    DBNameStr = atom_to_list(DBName),
    DefaultStorageType = memory,
    DefaultStoragePath = filename:join(priv_abs_path(), DBNameStr),

    StorageType = proplists:get_value(storage_type, Opts, 
				      DefaultStorageType),    
    {ok, Pointer} =
	case StorageType of
	    memory ->
		nif_init();
	    disk ->
		StoragePath = proplists:get_value(storage_path, Opts, 
						  DefaultStoragePath),
		StoragePath1 = filename:join(StoragePath, DBNameStr),
		StoragePath2 = list_to_binary(StoragePath1),
		
		nif_init(StoragePath2)
	end,
    
    ets:insert(?PTR_TAB, {DBName, Pointer}),

    {ok, DBName}.

%%--------------------------------------------------------------------
%% @doc
%% Issue vedis command by database name
%% @end
%%--------------------------------------------------------------------
command(DBName, Cmd) ->
    case ets:lookup(?PTR_TAB, DBName) of
	[{_, Pointer}] ->
	    nif_command(Pointer, Cmd);
	_ ->
	    db_not_found
    end.

%%--------------------------------------------------------------------
%% @doc
%% close vedis database
%% @end
%%--------------------------------------------------------------------
close(DBName) ->
    [{DBName, Pointer}] = ets:lookup(?PTR_TAB, DBName),
    ets:delete(?PTR_TAB, DBName),
    nif_close(Pointer).

%%====================================================================
%% Native Implemented Functions (NIFs)
%%====================================================================

%%--------------------------------------------------------------------
%% init
%%--------------------------------------------------------------------
nif_init() ->
    ?ERR_NIF_NOT_LOADED.

nif_init(_Storage) ->
    ?ERR_NIF_NOT_LOADED.    

%%--------------------------------------------------------------------
%% command
%%--------------------------------------------------------------------
nif_command(_Pointer, _BinCmd) ->
    ?ERR_NIF_NOT_LOADED.

%%--------------------------------------------------------------------
%% close
%%--------------------------------------------------------------------
nif_close(_Pointer) ->
    ?ERR_NIF_NOT_LOADED.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% priv_abs_path
%%--------------------------------------------------------------------
priv_abs_path() ->
    PrivRelPath = filename:join("..", "priv"),
    case code:priv_dir(?MODULE) of
	{error, bad_name} ->
	    Filename = code:which(?MODULE),
	    filename:join([filename:dirname(Filename), PrivRelPath]);
	Path ->
	    Path
    end.
