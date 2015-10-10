#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_nif.h"
#include "vedis.h"

static vedis *p_store;

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
  enif_inspect_binary(env, argv[0], &key_bin);
  enif_inspect_binary(env, argv[1], &val_bin);
  
  char *cmd = (char*) malloc(sizeof(char));
  int rc;

  strcpy(cmd, "SET ");
  strcat(cmd, (const char*) key_bin.data);
  strcat(cmd, " ");
  strcat(cmd, (const char*) val_bin.data);
  
  rc = vedis_exec(p_store, cmd, -1);
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
evedis_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  ErlNifBinary key_bin;
  enif_inspect_binary(env, argv[0], &key_bin);

  char *cmd = (char*) malloc(sizeof(char));
  int rc;
  vedis_value *p_result;

  strcpy(cmd, "GET ");
  strcat(cmd, (const char*) key_bin.data);
  
  vedis_exec(p_store, cmd, -1);
  rc = vedis_exec_result(p_store, &p_result);
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }

  const char *res;
  res = vedis_value_to_string(p_result, 0);

  ErlNifBinary res_bin;
  int res_size = strlen(res);
  
  enif_alloc_binary(res_size, &res_bin);
  res_bin.size = res_size;
  strcpy(res_bin.data, res);
  return enif_make_binary(env, &res_bin);
}

static ErlNifFunc
nif_funcs[] = {
  {"init", 0, evedis_init},
  {"ping", 0, evedis_ping},
  {"reflect", 1, evedis_reflect},
  {"set", 2, evedis_set},
  {"get", 1, evedis_get}
};

ERL_NIF_INIT(evedis, nif_funcs, NULL, NULL, NULL, NULL)
