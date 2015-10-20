#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_nif.h"
#include "vedis.h"

static vedis *p_store;
static vedis_value *p_result;

static ERL_NIF_TERM
evedis_priv_exec(ErlNifEnv *env, char *cmd);

static ERL_NIF_TERM
evedis_nif_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
evedis_nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
evedis_nif_ping(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
evedis_nif_command(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/*================================================================*/
/* Erlang NIF Initialing */
/*================================================================*/

/*----------------------------------------------------------------*/
/* nif_funcs */
/*----------------------------------------------------------------*/
static ErlNifFunc
nif_funcs[] = {
  {"init", 0, evedis_nif_init},
  {"init", 1, evedis_nif_init},
  {"close", 0, evedis_nif_close},
  {"ping", 0, evedis_nif_ping},
  {"command", 1, evedis_nif_command}
};

/*----------------------------------------------------------------*/
/* ERL_NIF_INIT */
/*----------------------------------------------------------------*/
ERL_NIF_INIT(evedis, nif_funcs, NULL, NULL, NULL, NULL)

/*================================================================*/
/* Erlang NIF API */
/*================================================================*/
 
/*----------------------------------------------------------------*/
/* evedis_nif_init */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_nif_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  char *storage_type = (char*) enif_alloc(1000);
  ErlNifBinary storage_path;
  
  if(argc == 0) {
    strcpy(storage_type, ":mem:");
  } else {
    if(!enif_inspect_binary(env, argv[0], &storage_path)) {
      return enif_make_badarg(env);
    }
    strncpy(storage_type,
  	    (const char*) storage_path.data,
  	    storage_path.size);
  }

  int rc;
  rc = vedis_open(&p_store, (const char*) storage_type);
  enif_free(storage_type);
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }  
  return enif_make_atom(env, "ok");
}

/*----------------------------------------------------------------*/
/* evedis_nif_close */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  vedis_close(p_store);
  return enif_make_atom(env, "ok");
}

/*----------------------------------------------------------------*/
/* evedis_nif_ping */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_nif_ping(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, "pong");
}

/*----------------------------------------------------------------*/
/* evedis_nif_command */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_nif_command(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  ErlNifBinary cmd_bin;
  
  if(!enif_inspect_binary(env, argv[0], &cmd_bin)) {
    return enif_make_badarg(env);
  }

  char *cmd = (char*) enif_alloc(cmd_bin.size);
  strncat(cmd, (const char*) cmd_bin.data, cmd_bin.size);

  return evedis_priv_exec(env, cmd);
}

/*================================================================*/
/* Private Functions */
/*================================================================*/

/*----------------------------------------------------------------*/
/* evedis_priv_exec */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_priv_exec(ErlNifEnv *env, char *cmd) {

  int rc;
  
  rc = vedis_exec(p_store, cmd, -1);
  enif_free(cmd);
  
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }

  rc = vedis_exec_result(p_store, &p_result);  

  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }
  
  if(vedis_value_is_null(p_result)){

    return enif_make_atom(env, "not_found");

  }else if(vedis_value_is_array(p_result)) {

    vedis_value *p_entry;
    unsigned cnt = 0;
    unsigned res_arr_size = vedis_array_count(p_result);
    ERL_NIF_TERM res_arr[res_arr_size];

    while((p_entry = vedis_array_next_elem(p_result)) != 0) {

      const char *res;
      ErlNifBinary res_bin;
      int res_size;

      if(vedis_value_is_null(p_entry)) {
	
	res_arr[cnt++] = enif_make_atom(env, "not_found");
	
      }else{
      
	res = vedis_value_to_string(p_entry, 0);
	res_size = strlen(res);
	
	enif_alloc_binary(res_size, &res_bin);
	res_bin.size = res_size;
	strcpy((char*) res_bin.data, res);
	res_arr[cnt++] = enif_make_binary(env, &res_bin);
      }
    }
    
    return enif_make_list_from_array(env, res_arr, cnt);
    
  }else{
    
    const char *res;
    res = vedis_value_to_string(p_result, 0);
    ErlNifBinary res_bin;
    int res_size = strlen(res);
    
    enif_alloc_binary(res_size, &res_bin);
    res_bin.size = res_size;
    strcpy((char*) res_bin.data, res);
    return enif_make_binary(env, &res_bin);
    
  }
}


