-module(gleam_epgsql).
-compile(no_auto_import).

-export([start_link/1, string/1, bool/1, int/1, float/1, null/0, array/1, query/3]).

start_link(A) ->
    gleam_epgsql_native:start_link(A).

string(A) ->
    gleam_epgsql_native:param(A).

bool(A) ->
    gleam_epgsql_native:param(A).

int(A) ->
    gleam_epgsql_native:param(A).

float(A) ->
    gleam_epgsql_native:param(A).

null() ->
    gleam_epgsql_native:null().

array(A) ->
    gleam_epgsql_native:param(A).

run_query(A, B, C) ->
    gleam_epgsql_native:run_query(A, B, C).

query(Conn, Sql, Params) ->
    result:map(run_query(Conn, Sql, Params), fun tuple:second/1).
