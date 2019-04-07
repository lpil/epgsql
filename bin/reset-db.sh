#!/bin/sh

set -eu

echo
echo Resetting database

psql <<SQL
DROP DATABASE IF EXISTS gleam_epgsql_test;
CREATE DATABASE gleam_epgsql_test;

CREATE TABLE IF NOT EXISTS cats (
  id INTEGER PRIMARY KEY,
  name VARCHAR(50) NOT NULL,
  is_cute boolean NOT NULL DEFAULT true
);
SQL

echo Done
echo
