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
  try epgsql:equery(Conn, Sql, Params) of
    {ok, _Count} -> {ok, {[], []}};
    {ok, Columns, Rows} -> {ok, {Columns, Rows}};
    {ok, _Count, Columns, Rows} -> {ok, {Columns, Rows}};
    {error, Error} -> {error, query_error(Error)};
    X -> erlang:display(X)
  catch
    error:function_clause:Stacktrace ->
      case Stacktrace of
        [{lists, zip, [ExtraExpected, ExtraGiven], _} | _] ->
          NParams = length(Params),
          Expected = NParams + length(ExtraExpected) - length(ExtraGiven),
          {error, {incorrect_number_of_params, #{given => NParams, expected => Expected}}};

        _ ->
          erlang:raise(error, function_clause, Stacktrace)
      end
  end.

query_error({error, Severity, Code, Codename, Message, Extra}) ->
  {pg_error, #{severity => Severity,
               code => Code,
               codename => Codename,
               message => Message,
               extra => Extra}}.

param(X) ->
  X.

null() ->
  null.
