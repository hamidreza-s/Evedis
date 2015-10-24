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
- Support for creating multiple databases.
- Support for concurrent read and write on multiple databases.
- Implemented all Vedis commands.
- Well tested and fully documented code.

Quick Start
-----

**Installation**

In a machine that has an installed Erlang, just clone this repository and run `make live` command. For functionality testing you can run `make test` command and `make doc` for generating its documents.

```sh
$ git clone https://github.com/hamidreza-s/Evedis.git
$ cd Evedis
$ make test # optional
$ make doc # optional
$ make live
```

After last command you will be attached to Erlang console which has just loaded `evedis.so` NIF shared object file. Then lets write some commands to get into it quickly.

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
<<"5">> = evedis_kv:incrby(foo, <<"conter">>, <<"4">>).
<<"4">> = evedis_kv:decr(foo, <<"counter">>).
<<"2">> = evedis_kv:decrby(foo, <<"counter">>, <<"2">>).
```

To see more examples you can check `test` directory. It contains lots of examples for all the commands.

API
-----

As well as `evedis_kv` module which is responsible for **Key/Value** API, `evedis_hash`, `evedis_list` and `evedis_set` modules are responsible for **Hash**, **List** and **Set** API respectively.

**Key/Value**

```erlang
-module(evedis_kv).
-export([get/2, set/3, setnx/3, del/2, 
	 append/3, exists/2, strlen/2, copy/3, 
	 move/3, mget/2, mset/2, msetnx/2,
	 getset/3, incr/2, decr/2, incrby/3,
	 decrby/3]).
```

**List**

```erlang
-module(evedis_list).
-export([index/3, len/2, pop/2, push/3]).
```

**Set**

```erlang
-module(evedis_set).
-export([add/3, ismember/3, pop/2, peek/2,
	 top/2, 'rem'/3, members/2, diff/2, 
	 inter/2, len/2]).
```

**Hash**

```erlang
-module(evedis_hash).
-export([get/3, set/4, del/3, len/2,
	 exists/3, mget/3, keys/2, vals/2,
	 getall/2, mset/3, setnx/4]).
```

**String**

```erlang
-module(evedis_string).
-export([getcsv/2, strip_tag/2, str_split/2, size_fmt/2,
	 soundex/2, base64/2, base64_dec/2]).
```

**Misc**

```erlang
-module(evedis_misc).
-export([rand/3, getrandmax/1, randstr/2, time/1,
	date/1, os/1, echo/2, abort/1, cmd_list/1,
	table_list/1, vedis/1, commit/1, rollback/1,
	'begin'/1]).
```

For generating a detailed API list with descriptions run `make doc` command.


Contribution
-----

Comments, contributions and patches are greatly appreciated.