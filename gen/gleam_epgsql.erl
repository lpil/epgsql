-module(gleam_epgsql).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1, query/3]).

err(E) ->
    {error, E}.

start_link(A) ->
    gleam_epgsql_native:start_link(A).

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
    expect:equal(query(Conn, <<"select 1;">>, []), {ok, any:from([{1}])}),
    expect:equal(query(Conn, <<"select 1, 2, 3, 4;">>, []),
                 {ok, any:from([{1, 2, 3, 4}])}),
    Error = #{}#{extra => [{file, <<"scan.l">>},
                           {line, <<"1128">>},
                           {position, <<"1">>},
                           {routine, <<"scanner_yyerror">>},
                           {severity,
                            <<"ERROR">>}]}#{severity => error}#{message => <<"syntax error at or near \"syntax\"">>}#{codename => atom:create_from_string(<<"syntax_error">>)}#{code => <<"42601">>},
    expect:equal(query(Conn, <<"syntax error">>, []), err(Error)).
-endif.
