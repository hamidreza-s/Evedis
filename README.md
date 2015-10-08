Evedis
======

Evedis is an Erlang binding for Vedis which is an embedded C library for storing data in memory or disk. Its API is similar in concept to Redis but without the networking layer since Vedis run in the same process of the host application.

Evedis uses Erlang Native Implemented Functions (NIF) for wrapping Vedis library to be used inside Erlang application.