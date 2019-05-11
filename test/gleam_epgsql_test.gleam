import expect
import atom
import gleam_epgsql
import any

pub fn library_test() {
  let Ok(conn) = gleam_epgsql:start_link([
    gleam_epgsql:Host("localhost"),
    gleam_epgsql:Username("postgres"),
    gleam_epgsql:Password("postgres"),
    gleam_epgsql:Database("gleam_epgsql_test"),
  ])

  "select 1;"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({1})]))

  "select 1, 2, 3, 4;"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({1, 2, 3, 4})]))

  "SELECT $1::REAL"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:float(2.5)])
  |> expect:equal(_, Ok([any:from({2.5})]))

  let error = gleam_epgsql:PgError({
    code = "42601",
    codename = atom:create_from_string("syntax_error"),
    message = "syntax error at or near \"syntax\"",
    severity = gleam_epgsql:Error,
    extra = [
      gleam_epgsql:File("scan.l"),
      gleam_epgsql:Line("1128"),
      gleam_epgsql:Position("1"),
      gleam_epgsql:Routine("scanner_yyerror"),
      gleam_epgsql:Severity("ERROR"),
    ],
  })

  "syntax error"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Error(error))

  "DELETE FROM cats;"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([]))

  "SELECT * FROM cats;"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([]))

  "INSERT INTO cats (id, name, is_cute) VALUES
  (1, 'Nubi', true),
  (2, 'Ginny', true);"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([]))

  "SELECT * FROM cats WHERE id = 1;"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({1, "Nubi", True})]))

  "SELECT name FROM cats ORDER BY id;"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({"Nubi"}), any:from({"Ginny"})]))

  "INSERT INTO cats VALUES (3, 'Mister Bigglesworth', $1)"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:bool(False)])
  |> expect:equal(_, Ok([]))

  "SELECT name FROM cats WHERE is_cute = $1;"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:bool(False)])
  |> expect:equal(_, Ok([any:from({"Mister Bigglesworth"})]))

  "SELECT name FROM cats WHERE is_cute = $1 AND id > $2;"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:bool(True), gleam_epgsql:int(2)])
  |> expect:equal(_, Ok([]))

  "SELECT name FROM cats WHERE is_cute = $1 AND id > $2;"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:bool(True), gleam_epgsql:int(1)])
  |> expect:equal(_, Ok([any:from({"Ginny"})]))

  "UPDATE cats SET is_cute = $1"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:bool(True)])
  |> expect:equal(_, Ok([]))

  "SELECT is_cute FROM cats WHERE id = 3"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({True})]))

  "UPDATE cats SET is_cute = NOT is_cute WHERE id = 3 RETURNING is_cute"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({False})]))

  "SELECT is_cute FROM cats WHERE id = 3"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Ok([any:from({False})]))

  "UPDATE cats SET is_cute = NOT is_cute WHERE id = 3"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:string("hi there")])
  |> expect:equal(_, Error(gleam_epgsql:IncorrectNumberOfParams({expected = 0, given = 1})))

  "UPDATE cats SET is_cute = $1 WHERE id = $2"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:string("hi there")])
  |> expect:equal(_, Error(gleam_epgsql:IncorrectNumberOfParams({expected = 2, given = 1})))

  "UPDATE cats SET is_cute = $1 WHERE id = $2 OR id = $3"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:string("hi there")])
  |> expect:equal(_, Error(gleam_epgsql:IncorrectNumberOfParams({expected = 3, given = 1})))

  "UPDATE cats SET is_cute = $1 WHERE id = $2 OR id = $3"
  |> gleam_epgsql:query(conn, _, [])
  |> expect:equal(_, Error(gleam_epgsql:IncorrectNumberOfParams({expected = 3, given = 0})))

  "UPDATE cats SET is_cute = NOT is_cute WHERE id = $1"
  |> gleam_epgsql:query(conn, _, [gleam_epgsql:string("hi there"), gleam_epgsql:bool(True), gleam_epgsql:int(4)])
  |> expect:equal(_, Error(gleam_epgsql:IncorrectNumberOfParams({expected = 1, given = 3})))
}
