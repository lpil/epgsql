import gleam/expect
import gleam/atom
import gleam/epgsql
import gleam/any

pub fn library_test() {
  let Ok(conn) = epgsql:start_link([
    epgsql:Host("localhost"),
    epgsql:Username("postgres"),
    epgsql:Password("postgres"),
    epgsql:Database("gleam_epgsql_test"),
  ])

  "select 1;"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({1})]))

  "select 1, 2, 3, 4;"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({1, 2, 3, 4})]))

  "SELECT $1::REAL"
  |> epgsql:query(conn, _, [epgsql:float(2.5)])
  |> expect:equal(_, Ok([any:from({2.5})]))

  let error = epgsql:PgError({
    code = "42601",
    codename = atom:create_from_string("syntax_error"),
    message = "syntax error at or near \"syntax\"",
    severity = epgsql:Error,
    extra = [
      epgsql:File("scan.l"),
      epgsql:Line("1128"),
      epgsql:Position("1"),
      epgsql:Routine("scanner_yyerror"),
      epgsql:Severity("ERROR"),
    ],
  })

  "syntax error"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Error(error))

  "DELETE FROM cats;"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([]))

  "SELECT * FROM cats;"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([]))

  "INSERT INTO cats (id, name, is_cute) VALUES
  (1, 'Nubi', true),
  (2, 'Ginny', true);"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([]))

  "SELECT * FROM cats WHERE id = 1;"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({1, "Nubi", True})]))

  "SELECT name FROM cats ORDER BY id;"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({"Nubi"}), any:from({"Ginny"})]))

  "INSERT INTO cats VALUES (3, 'Mister Bigglesworth', $1)"
  |> epgsql:query(conn, _, [epgsql:bool(False)])
  |> expect:equal(_, Ok([]))

  "SELECT name FROM cats WHERE is_cute = $1;"
  |> epgsql:query(conn, _, [epgsql:bool(False)])
  |> expect:equal(_, Ok([any:from({"Mister Bigglesworth"})]))

  "SELECT name FROM cats WHERE is_cute = $1 AND id > $2;"
  |> epgsql:query(conn, _, [epgsql:bool(True), epgsql:int(2)])
  |> expect:equal(_, Ok([]))

  "SELECT name FROM cats WHERE is_cute = $1 AND id > $2;"
  |> epgsql:query(conn, _, [epgsql:bool(True), epgsql:int(1)])
  |> expect:equal(_, Ok([any:from({"Ginny"})]))

  "UPDATE cats SET is_cute = $1"
  |> epgsql:query(conn, _, [epgsql:bool(True)])
  |> expect:equal(_, Ok([]))

  "SELECT is_cute FROM cats WHERE id = 3"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({True})]))

  "UPDATE cats SET is_cute = NOT is_cute WHERE id = 3 RETURNING is_cute"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({False})]))

  "SELECT is_cute FROM cats WHERE id = 3"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({False})]))

  "UPDATE cats SET is_cute = NOT is_cute WHERE id = 3"
  |> epgsql:query(conn, _, [epgsql:string("hi there")])
  |> expect:equal(_, Error(epgsql:IncorrectNumberOfParams({expected = 0, given = 1})))

  "UPDATE cats SET is_cute = $1 WHERE id = $2"
  |> epgsql:query(conn, _, [epgsql:string("hi there")])
  |> expect:equal(_, Error(epgsql:IncorrectNumberOfParams({expected = 2, given = 1})))

  "UPDATE cats SET is_cute = $1 WHERE id = $2 OR id = $3"
  |> epgsql:query(conn, _, [epgsql:string("hi there")])
  |> expect:equal(_, Error(epgsql:IncorrectNumberOfParams({expected = 3, given = 1})))

  "UPDATE cats SET is_cute = $1 WHERE id = $2 OR id = $3"
  |> epgsql:query(conn, _, [])
  |> expect:equal(_, Error(epgsql:IncorrectNumberOfParams({expected = 3, given = 0})))

  "UPDATE cats SET is_cute = NOT is_cute WHERE id = $1"
  |> epgsql:query(conn, _, [epgsql:string("hi there"), epgsql:bool(True), epgsql:int(4)])
  |> expect:equal(_, Error(epgsql:IncorrectNumberOfParams({expected = 1, given = 3})))
}
