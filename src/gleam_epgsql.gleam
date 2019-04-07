import any
import atom
import tuple
import expect
import result

fn err(e) {
  Error(e)
}

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

pub enum ConnectError =
  | InvalidAuthorizationSpecification
  | InvalidPassword
  | UnsupportedAuthMethod(any:Any) // TODO: Refine this Any
  | SaslServerFinal(any:Any)
  | ConnectQueryError({
     severity = Severity,
     code = String,
     codename = atom:Atom,
     message = String,
     extra = List(QueryErrorExtra),
   })

pub external type Connection;

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
  -> Result(Connection, ConnectError)
  = "gleam_epgsql_native" "start_link"

enum Column =
  | Column(String, atom:Atom, Int, Int, Int)

external fn run_query(Connection, String, List(Parameter))
  -> Result(
      {List(Column), any:Any},
      {
        severity = Severity,
        code = String,
        codename = atom:Atom,
        message = String,
        extra = List(QueryErrorExtra),
      }
    )
  = "gleam_epgsql_native" "run_query"

pub fn query(conn, sql, params) {
  conn
  |> run_query(_, sql, params)
  |> result:map(_, tuple:second)
}

test library {
  let Ok(conn) = start_link([
    Host("localhost"),
    Username("postgres"),
    Password("postgres"),
    Database("gleam_epgsql_test"),
  ])

  "select 1;"
  |> query(conn, _, [])
  |> expect:equal(_, Ok(any:from([{1}])))

  "select 1, 2, 3, 4;"
  |> query(conn, _, [])
  |> expect:equal(_, Ok(any:from([{1, 2, 3, 4}])))

  let error = {
    code = "42601",
    codename = atom:create_from_string("syntax_error"),
    message = "syntax error at or near \"syntax\"",
    severity = Error,
    extra = [
      File("scan.l"),
      Line("1128"),
      Position("1"),
      Routine("scanner_yyerror"),
      Severity("ERROR"),
    ],
  }

  "syntax error"
  |> query(conn, _, [])
  |> expect:equal(_, err(error))
}
