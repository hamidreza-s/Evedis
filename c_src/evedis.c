#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_nif.h"
#include "vedis.h"

#define MAX_PATH 1024

static ErlNifResourceType *rsc_type;

/*================================================================*/
/* Function Declarations */
/*================================================================*/

static void
free_resource(ErlNifEnv *env, void *obj);

static int
load(ErlNifEnv* env, void **priv, ERL_NIF_TERM load_info);

static ERL_NIF_TERM
evedis_priv_exec(ErlNifEnv *env, vedis **p_store_rsc, char *cmd);

static ERL_NIF_TERM
evedis_nif_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
evedis_nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

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
  {"nif_init", 0, evedis_nif_init},
  {"nif_init", 1, evedis_nif_init},
  {"nif_close", 1, evedis_nif_close},
  {"nif_command", 2, evedis_nif_command}
};

/*----------------------------------------------------------------*/
/* ERL_NIF_INIT */
/*----------------------------------------------------------------*/
ERL_NIF_INIT(evedis, nif_funcs, &load, NULL, NULL, NULL)

/*================================================================*/
/* Erlang NIF Hooks and Resource Handler */
/*================================================================*/

/*----------------------------------------------------------------*/
/* free_resource */
/*----------------------------------------------------------------*/
void
free_resource(ErlNifEnv *env, void *obj)
{
  struct vedis **p_store_rsc = obj;
  vedis_close(*p_store_rsc);
}

/*----------------------------------------------------------------*/
/* load */
/*----------------------------------------------------------------*/
static int
load(ErlNifEnv* env, void **priv, ERL_NIF_TERM load_info)
{
  rsc_type = enif_open_resource_type(env,
				     NULL,
				     "evedis",
				     free_resource,
				     ERL_NIF_RT_CREATE,
				     NULL);

  if(rsc_type == NULL) return -1;
  return 0;
}

/*================================================================*/
/* Erlang NIF API */
/*================================================================*/
 
/*----------------------------------------------------------------*/
/* evedis_nif_init */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_nif_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  vedis **p_store_rsc;
  ERL_NIF_TERM p_store_term;
  
  p_store_rsc =
    (struct vedis**) enif_alloc_resource(rsc_type, sizeof(struct vedis*));

  if(p_store_rsc == NULL) return enif_make_badarg(env);

  p_store_term = enif_make_resource(env, p_store_rsc);

  char *storage_type = (char*) enif_alloc(MAX_PATH);
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
  rc = vedis_open(p_store_rsc, (const char*) storage_type);
  enif_free(storage_type);
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }  

  return enif_make_tuple2(env,
			  enif_make_atom(env, "ok"),
			  p_store_term);
}

/*----------------------------------------------------------------*/
/* evedis_nif_close */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_nif_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  vedis **p_store_rsc;

  if(argc != 1) {
    return enif_make_badarg(env);
  }

  if(!enif_get_resource(env, argv[0], rsc_type, (void**) &p_store_rsc)) {
    return enif_make_badarg(env);
  }

  vedis_close(*p_store_rsc);
  return enif_make_atom(env, "ok");
}

/*----------------------------------------------------------------*/
/* evedis_nif_command */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_nif_command(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  vedis **p_store_rsc;
  ErlNifBinary cmd_bin;

  if(argc != 2) {
    return enif_make_badarg(env);
  }

  if(!enif_get_resource(env, argv[0], rsc_type, (void**) &p_store_rsc)) {
    return enif_make_atom(env, "error");
  }
  
  if(!enif_inspect_binary(env, argv[1], &cmd_bin)) {
    return enif_make_atom(env, "error");
  }

  char *cmd = (char*) enif_alloc(cmd_bin.size);
  strncat(cmd, (const char*) cmd_bin.data, cmd_bin.size);

  return evedis_priv_exec(env, p_store_rsc, cmd);
}

/*================================================================*/
/* Private Functions */
/*================================================================*/

/*----------------------------------------------------------------*/
/* evedis_priv_exec */
/*----------------------------------------------------------------*/
static ERL_NIF_TERM
evedis_priv_exec(ErlNifEnv *env, vedis **p_store_rsc, char *cmd)
{
  int rc;
  vedis_value *p_result;
  
  rc = vedis_exec(*p_store_rsc, cmd, -1);
  enif_free(cmd);
  
  if(rc != VEDIS_OK) {
    return enif_make_atom(env, "error");
  }

  rc = vedis_exec_result(*p_store_rsc, &p_result);  

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


