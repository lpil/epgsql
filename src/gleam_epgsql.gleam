import any
import atom
import tuple
import expect
import result
import erlang_process

pub enum Severity =
  | Debug
  | Log
  | Info
  | Notice
  | Warning
  | Error
  | Fatal
  | Panic

pub enum QueryErrorExtra =
  | Severity(String)
  | Detail(String)
  | Hint(String)
  | Position(String)
  | InternalPosition(String)
  | InternalQuery(String)
  | Where(String)
  | SchemaName(String)
  | TableName(String)
  | ColumnName(String)
  | DataTypeName(String)
  | ConstraintName(String)
  | File(String)
  | Line(String)
  | Routine(String)

// TODO: inline when the compiler supports aliases
// pub type QueryError =
//   {
//     severity = Severity,
//     code = String,
//     codename = atom:Atom,
//     message = String,
//     extra = List(QueryErrorExtra),
//   }

pub enum QueryError =
  | PgError({
      severity = Severity,
      code = String,
      codename = atom:Atom,
      message = String,
      extra = List(QueryErrorExtra),
    })
  | IncorrectNumberOfParams({
      expected = Int,
      given = Int,
    })

pub enum ConnectError =
  | InvalidAuthorizationSpecification
  | InvalidPassword
  | UnsupportedAuthMethod(any:Any) // TODO: Refine this Any
  | SaslServerFinal(any:Any)
  | ConnectQueryError(QueryError)

pub external type Parameter;

// TODO: Include other possible options
pub enum ConnectionOption =
  | Database(String)
  | Username(String)
  | Password(String)
  | Host(String)
  | Port(Int)
  | Timeout(Int)

pub external fn start_link(List(ConnectionOption))
  -> Result(erlang_process:Pid, ConnectError)
  = "gleam_epgsql_native" "start_link"

enum Column =
  | Column(String, atom:Atom, Int, Int, Int)

pub external fn string(String) -> Parameter = "gleam_epgsql_native" "param"

pub external fn bool(Bool) -> Parameter = "gleam_epgsql_native" "param"

pub external fn int(Int) -> Parameter = "gleam_epgsql_native" "param"

pub external fn float(Float) -> Parameter = "gleam_epgsql_native" "param"

pub external fn null() -> Parameter = "gleam_epgsql_native" "null"

pub external fn array(List(Parameter)) -> Parameter = "gleam_epgsql_native" "param"

external fn run_query(erlang_process:Pid, String, List(Parameter))
  -> Result({List(Column), List(any:Any)}, QueryError)
  = "gleam_epgsql_native" "run_query"

pub fn query(conn, sql, params) {
  conn
  |> run_query(_, sql, params)
  |> result:map(_, tuple:second)
}
