#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_nif.h"
#include "vedis.h"

static vedis *p_store;
static vedis_value *p_result;

static ERL_NIF_TERM
evedis_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  int rc;
  rc = vedis_open(&p_store, ":mem:");
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }  
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
evedis_ping(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, "pong");
}

static ERL_NIF_TERM
evedis_reflect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary obj_bin;
  enif_inspect_binary(env, argv[0], &obj_bin);
  return enif_make_binary(env, &obj_bin);
}

static ERL_NIF_TERM
evedis_set(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  
  ErlNifBinary key_bin;
  ErlNifBinary val_bin;
  
  if(!enif_inspect_binary(env, argv[0], &key_bin) ||
     !enif_inspect_binary(env, argv[1], &val_bin)) {
    return enif_make_badarg(env);
  }
  
  int rc;
  char *cmd = (char*) enif_alloc(100);
  strcpy(cmd, "SET ");
  strncat(cmd, (const char*) key_bin.data, key_bin.size);
  strcat(cmd, " ");
  strncat(cmd, (const char*) val_bin.data, val_bin.size); 
  rc = vedis_exec(p_store, cmd, -1);
  enif_free(cmd);
  
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }
  
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
evedis_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  
  ErlNifBinary key_bin;
  
  if(!enif_inspect_binary(env, argv[0], &key_bin)) {
    return enif_make_badarg(env);
  }
  
  char *cmd = (char*) enif_alloc(100);
  int rc;
  
  strcpy(cmd, "GET ");
  strncat(cmd, (const char*) key_bin.data, key_bin.size);
  
  vedis_exec(p_store, cmd, -1);
  rc = vedis_exec_result(p_store, &p_result);
  enif_free(cmd);
  
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }
  
  const char *res;
  res = vedis_value_to_string(p_result, 0);
  
  ErlNifBinary res_bin;
  int res_size = strlen(res);
  
  enif_alloc_binary(res_size, &res_bin);
  res_bin.size = res_size;
  strcpy((char*) res_bin.data, res);
  return enif_make_binary(env, &res_bin);
}

static ERL_NIF_TERM
evedis_disk_commit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int rc;
  rc = vedis_commit(p_store);
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }
  return enif_make_atom(env, "ok");
}

static ErlNifFunc
nif_funcs[] = {
  {"init", 0, evedis_init},
  {"ping", 0, evedis_ping},
  {"reflect", 1, evedis_reflect},
  {"set", 2, evedis_set},
  {"get", 1, evedis_get},
  {"disk_commit", 0, evedis_disk_commit}
};

ERL_NIF_INIT(evedis, nif_funcs, NULL, NULL, NULL, NULL)
