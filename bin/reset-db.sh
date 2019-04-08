#!/bin/sh

set -eu

echo
echo Resetting database

psql <<SQL
CREATE DATABASE gleam_epgsql_test;
SQL

psql -v "ON_ERROR_STOP=1" -d gleam_epgsql_test <<SQL
DROP TABLE IF EXISTS cats;

CREATE TABLE cats (
  id INTEGER PRIMARY KEY,
  name VARCHAR(50) NOT NULL,
  is_cute boolean NOT NULL DEFAULT true
);
SQL

echo Done
echo
