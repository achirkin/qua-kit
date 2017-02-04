DROP DATABASE IF EXISTS sirendb;
DO
$body$
BEGIN
   IF NOT EXISTS (
      SELECT *
      FROM   pg_catalog.pg_user
      WHERE  usename = 'siren') THEN
      CREATE USER siren LOGIN PASSWORD 'sirenpass';
   END IF;
END
$body$;
CREATE DATABASE sirendb;
GRANT ALL PRIVILEGES ON DATABASE sirendb to siren;


