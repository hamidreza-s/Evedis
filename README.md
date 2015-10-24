Evedis
======

Evedis is an Erlang binding for [Vedis](http://vedis.symisc.net) which is an embedded C library for storing data in memory or disk. Its API is similar in concept to [Redis](http://redis.io) but without the networking layer since Vedis run in the same process of the host application.

Evedis uses Erlang Native Implemented Functions (NIF) for wrapping Vedis library to be used inside Erlang application.

Features
-----

**Vedis Features**
- Serverless, datastore engine.
- Transactional (ACID) datastore.
- Built with over 70 commands similar to the standard Redis commands.
- Zero configuration.
- Single database file, does not use temporary files.
- Cross-platform file format.
- Standard Key/Value store.
- Support for on-disk as well in-memory datastore.
- Thread safe and full reentrant.
- Simple, Clean and easy to use API.
- Support Terabyte sized databases. 

**Evedis Features**
- Support for creating multiple database
- Support for concurrent read and write on multiple database
- Implemented all Vedis commands
- Well test and fully documented code

Quick Start
-----

**Installation**

In a machine that has an installed Erlang, just clone this repository and run *make live* command. For functionality testing you can run *make test* command and *make doc* for generating its documents.

```bash
$ git clone https://github.com/hamidreza-s/Evedis.git
$ cd Evedis
$ make test # optional
$ make doc # optional
$ make live
```

After last command you will be attached to Erlang console which has just loaded **evedis** NIF shared object file. Then lets write some commands to get into it quickly.

```erlang

%% create an in-memory database
{ok, foo} = evedis:create(foo, [{storage_type, memory}]).

%% create a disk based database
{ok, bar} = evedis:create(bar, [{storage_type, disk}, {storage_path, "/tmp"}]).

%% set some values by key
<<"true">> = evedis_kv:set(foo, <<"db_name">>, <<"FooDB">>).
<<"true">> = evedis_kv:set(bar, <<"db_name">>, <<"BarDB">>).

%% get some values by key
<<"FooDB">> = evedis_kv:get(foo, <<"db_name">>).
<<"BarDB">> = evedis_kv:get(bar, <<"db_name">>).

%% append to a value by its key
<<"true">> = evedis_kv:append(foo, <<"db_name">>, <<"_V1">>).
<<"FooDB_V1">> = evedis_kv:get(foo, <<"db_name">>).

%% delete a key value
<<"true">> = evedis_kv:del(bar, <<"db_name">>).
not_found = evedis_kv:get(bar, <<"db_name">>).

%% multiple set and get commands
<<"true">> = evedis_kv:mset(foo, [{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}]).
[<<"v1">>, <<"v2">>] = evedis_kv:mget(foo, [<<"k1">>, <<"k2">>]).

%% increment and decrement a value
<<"1">> = evedis_kv:incr(foo, <<"counter">>).
<<"5">> = evedis_kv:incrby(foo, <<"conter">>, 4).
<<"4">> = evedis_kv:decr(foo, <<"counter">>).
<<"2">> = evedis_kv:decrby(foo, <<"counter">>, 2).
```

As well as *evedis_kv* module which is responsible for **Key/Value** API, *evedis_hash*, *evedis_list* and *evedis_set* modules are responsible for **Hash**, **List** and **Set** API respectively.

API
-----

The full API list as well as its *types* and *specs* are listed as follows:

**Evedis Types**

```erlang
-type db() :: atom().
-type param() :: binary().
-type result() :: binary() | not_found.
-type return() :: result() | [result()].
```

**Key/Value Specifications**

```erlang
-spec append(DBName::db(), Key::param(), Val::param()) -> return(). 
-spec copy(DBName::db(), OldKey::param(), NewVal::param()) -> return().
-spec decr(DBName:db(), Key::param()) -> return().
-spec decrby(DBName:db(), Key::param(), Decr::param()) -> return().
-spec del(DBName::db(), Key::param()) -> return().
-spec exists(DBName::db(), Key::param()) -> return().
-spec get(DBName::db(), Key::param()) -> return().
-spec getset(DBName::db(), Key::param(), Val::param) -> return().
-spec incr(DBName::db(), Key::param()) -> return().
-spec incrby(DBName::db(), Key::param(), Incr::param()) -> return().
-spec mget(DBName::db(), KeyList::[param()]) -> return().
-spec move(DBName::db(), OldKey::param(), NewVal::param()) -> return().
-spec mset(DBName::db(), KeyValList::[{param(), param()}]) -> return().
-spec msetnx(DBName::db(), KeyValList::[{param(), param()}]) -> return().
-spec set(DBName::db(), Key::param(), Val::param()) -> return().
-spec setnx(DBName::db(), Key::param(), Val::param()) -> return().
-spec strlen(DBName::db(), Key::param()) -> return().
```
