### qua-kit pack

These binaries allow to run qua-kit under linux and access it
at `http://localhost:3000`.
The binaries were build under Ubuntu 16.04.

#### Requirements

The siren service is based on PostgreSQL database and its extension PostGIS.
This means that to build and run the service, you need to install those.
On Ubuntu 16.04 I am using following command for this purpose:
```
  sudo apt-get install postgresql postgis
```
Currently, `postgresql` package refers to `postgresql-9.5`,
`postgis` package refers to `postgresql-9.5-postgis-2.2`.


### Setting up a database

Run a script `setup-sirendb.sh` as a user with postgres `rights`, e.g.:
```
sudo -u postgres ./setup-sirendb.sh
```

### Running
The following command run everything in separate unix threads
```
./qua-server &
./helen &
sleep 1
./siren &
./hs-example-service &
sleep 1
```
To stop everithing in the same manner, just type:
```
killall helen
killall qua-server
```
