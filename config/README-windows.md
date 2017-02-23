### I. Installation instructions on Windows:

Install PostgreSQL 9.5 + PostGIS 2.2.
I used installed by the following link:
https://get.enterprisedb.com/postgresql/postgresql-9.5.5-1-windows-x64.exe
or
https://www.enterprisedb.com/downloads/postgres-postgresql-downloads

After first steps of installation, continue to Stack Builder tool, and select:
"Spatial Extensions" -> "PostGIS 2.2 Bundle for Postgres 9.5 (64 bit) v2.2.2 (installed)"

Make sure the path to Postgres bin is in environment
(I had to add C:\Program Files\PostgreSQL\9.5\bin to PATH)


### II. Configuration instructions on Windows:

siren.exe service requires postgis database to be set up before running.
To configure a default database, run following .bat file:

setup-sirendb.bat


### III. Run qua-kit on Windows:

Just run all four applications in a given order by clicking on them.
1) helen.exe + siren.exe + hs-example-service.exe can by using .bat file: runLuci.bat
2) qua-kit.exe 


Example .geojson scenario is available at static\data\run1\mooctask.geojson