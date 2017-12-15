### qua-kit pack

These binaries allow to run qua-kit under mac and access it
at `http://localhost:3000`.

#### Requirements

The siren service is based on PostgreSQL database and its extension PostGIS.
This means that to build and run the service, you need to install those.
In addition, qua-server need `gd` graphics library.
On mac OS I am using following command for this purpose:
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

Current bundle consists of several programs that need to run at the same time.
Luckily, helen can start programs in separate threads specified in file `helen-config.yaml`.
Thus, you only need to start helen manually. 
```
./helen
```
Make sure postgres database is set up and running before this.
