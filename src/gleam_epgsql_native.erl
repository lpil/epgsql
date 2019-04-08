-module(gleam_epgsql_native).

-include_lib("epgsql/include/epgsql.hrl").

-export([start_link/1, run_query/3, param/1, null/0]).

start_link(Opts) ->
  NewOpts =
    lists:map(fun
      ({K, V}) when K =:= username orelse K =:= password orelse K =:= host -> {K, binary_to_list(V)};
      (X) -> X
    end, Opts),
  case epgsql:connect(NewOpts) of
    {error, #error{} = E} ->
      {connect_query_error, query_error(E)};

    Other ->
      Other
  end.

run_query(Conn, Sql, Params) ->
  case epgsql:equery(Conn, Sql, Params) of
    {ok, _Count} -> {ok, {[], []}};
    {ok, Columns, Rows} -> {ok, {Columns, Rows}};
    {ok, _Count, Columns, Rows} -> {ok, {Columns, Rows}};
    {error, Error} -> {error, query_error(Error)}
  end.

query_error({error, Severity, Code, Codename, Message, Extra}) ->
  #{severity => Severity,
    code => Code,
    codename => Codename,
    message => Message,
    extra => Extra}.

param(X) ->
  X.

null() ->
  null.
