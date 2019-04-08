-module(gleam_epgsql).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1, bool/1, int/1, float/1, null/0, array/1, query/3]).

err(E) ->
    {error, E}.

start_link(A) ->
    gleam_epgsql_native:start_link(A).

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

-ifdef(TEST).
library_test() ->
    {ok,
     Conn} = start_link([{host, <<"localhost">>},
                         {username, <<"postgres">>},
                         {password, <<"postgres">>},
                         {database, <<"gleam_epgsql_test">>}]),
    expect:equal(query(Conn, <<"select 1;">>, []), {ok, [any:from({1})]}),
    expect:equal(query(Conn, <<"select 1, 2, 3, 4;">>, []),
                 {ok, [any:from({1, 2, 3, 4})]}),
    expect:equal(query(Conn, <<"SELECT $1::REAL">>, [float(2.5)]),
                 {ok, [any:from({2.5})]}),
    Error = #{}#{extra => [{file, <<"scan.l">>},
                           {line, <<"1128">>},
                           {position, <<"1">>},
                           {routine, <<"scanner_yyerror">>},
                           {severity,
                            <<"ERROR">>}]}#{severity => error}#{message => <<"syntax error at or near \"syntax\"">>}#{codename => atom:create_from_string(<<"syntax_error">>)}#{code => <<"42601">>},
    expect:equal(query(Conn, <<"syntax error">>, []), err(Error)),
    expect:equal(query(Conn, <<"SELECT * FROM cats;">>, []), {ok, []}),
    expect:equal(query(Conn,
                       <<"INSERT INTO cats (id, name, is_cute) VALUES
  (1, 'Nubi', true),
  (2, 'Ginny', true);">>,
                       []),
                 {ok, []}),
    expect:equal(query(Conn, <<"SELECT * FROM cats WHERE id = 1;">>, []),
                 {ok, [any:from({1, <<"Nubi">>, true})]}),
    expect:equal(query(Conn, <<"SELECT name FROM cats ORDER BY id;">>, []),
                 {ok, [any:from({<<"Nubi">>}), any:from({<<"Ginny">>})]}),
    expect:equal(query(Conn,
                       <<"INSERT INTO cats VALUES (3, 'Mister Bigglesworth', $1)">>,
                       [bool(false)]),
                 {ok, []}),
    expect:equal(query(Conn,
                       <<"SELECT name FROM cats WHERE is_cute = $1;">>,
                       [bool(false)]),
                 {ok, [any:from({<<"Mister Bigglesworth">>})]}),
    expect:equal(query(Conn,
                       <<"SELECT name FROM cats WHERE is_cute = $1 AND id > $2;">>,
                       [bool(true), int(2)]),
                 {ok, []}),
    expect:equal(query(Conn,
                       <<"SELECT name FROM cats WHERE is_cute = $1 AND id > $2;">>,
                       [bool(true), int(1)]),
                 {ok, [any:from({<<"Ginny">>})]}),
    expect:equal(query(Conn, <<"UPDATE cats SET is_cute = $1">>, [bool(true)]),
                 {ok, []}),
    expect:equal(query(Conn, <<"SELECT is_cute FROM cats WHERE id = 3">>, []),
                 {ok, [any:from({true})]}),
    expect:equal(query(Conn,
                       <<"UPDATE cats SET is_cute = NOT is_cute WHERE id = 3 RETURNING is_cute">>,
                       []),
                 {ok, [any:from({false})]}),
    expect:equal(query(Conn, <<"SELECT is_cute FROM cats WHERE id = 3">>, []),
                 {ok, [any:from({false})]}).
-endif.
