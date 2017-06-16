### qua-kit pack

These binaries allow to run qua-kit under mac and access it
at `http://localhost:3000`.

#### Requirements

The siren service is based on PostgreSQL database and its extension PostGIS.
This means that to build and run the service, you need to install those.
On Ubuntu 16.04 I am using following command for this purpose:
```
brew install libgd
brew install postgresql
brew install postgis
```


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
