#include "erl_nif.h"
#include "vedis.h"

static ERL_NIF_TERM ping(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "pong", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"ping", 0, ping}
};

ERL_NIF_INIT(evedis, nif_funcs, NULL, NULL, NULL, NULL)
