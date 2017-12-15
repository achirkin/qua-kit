### qua-kit pack

These binaries allow to run qua-kit under linux and access it
at `http://localhost:3000`.
The binaries were build under Ubuntu 16.04.

#### Requirements

The siren service is based on PostgreSQL database and its extension PostGIS.
This means that to build and run the service, you need to install those.
In addition, qua-server need `gd` graphics library.
On Ubuntu 16.04 I am using following command for this purpose:
```
  sudo apt-get install postgresql postgis libpq-dev libgd-dev
```
Currently, `postgresql` package refers to `postgresql-9.5`,
`postgis` package refers to `postgresql-9.5-postgis-2.2`.


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

### Using

  1. Go to qua-kit site at `http://localhost:3000` and click on "use geometry editor".
      * (hint) Open browser console to see debug output if you have any troubles.
  2. To be able to connect to luci, you need to login. The default admin credentials are:
      * username: "admin@qua-kit.hs"
      * password: "make it some random thing"
  3. Open toolbox -> connect to luci.
  4. Run scenario:
      * (a) Load some scenario via luci (if uploaded something before).
      * (b) Upload some scenario using `FILES` button.
            There are some scenarios available in `geojson-examples` archive.
  5. Make sure that `luci` and some service is running, then go to `SERVICES` tab.
     It should show a list of available services.
     You can select one to run it.
     Click on `refresh` button if you do not see your service in a list.
     Selecting an active service invokes parameter refreshing and display.
     Check if all optional parameters of your service are displayed as intended.
  6. Press green `play` button.
