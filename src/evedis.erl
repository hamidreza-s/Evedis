-module(evedis).

-export([init/0, ping/0]).

ping() ->
    "NIF library not loaded".

init() ->
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
