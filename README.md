## qua-kit
# Quick Urban Analysis Kit
=================================================

Qua-Kit is a client-server system that aims at assisting urban designers in their design process.
Try it on our web server at [qua-kit.ethz.ch](http://qua-kit.ethz.ch).

Qua-View (`apps/hs/qua-view`) is a WebGL-base browser viewer and editor for building geometry. It is based on Haskell GHCJS.

Luci (`apps/java/luci2`) is a lightweight middleware that allows to connect different urban computing services together
and present their results in Qua-View.

Projects that are already in some git repositories can be added as git modules
using `git submodule add <repository> <path>`.

### Installation prerequisites

##### Java
First, install
[`JDK`](http://www.oracle.com/technetwork/java/javase/downloads/index.html);
we prefer java 8.
Second, install
[`maven`](https://maven.apache.org/).

##### Haskell
To build and install (and run later) haskell applications use
[`stack`](http://docs.haskellstack.org/en/stable/README.html).
`Stack` is a tool that installs for you haskell compilers (GHC), manages all package dependencies,
and builds the projects.

### Components

### qua-view

Path: `apps/hs/qua-view`.

Client side of qua-kit. Browse the submodule for details.

#### qua-server

Path: `apps/hs/qua-server`.

This app requires `gd` library at least (`libgd-dev` in Ubuntu).
Follow error messages when installing to check if there are any other requirements.
To build and run a particular app, use `build` and `exec` commands provided by `stack`.
For example, to run `qua-server` you shoud:
```
stack build qua-server --flag qua-server:dev
stack exec qua-server
```
Flag `qua-server:dev` is needed to use sqlite database instead of postgresql
Alternatively, you can use `yesod-bin` package to run it:
```
stack install yesod-bin cabal-install
stack exec yesod devel
```

#### Luci

Path: `apps/java/luci2`.
Given Java and maven are set up correctly, run Luci as follows:
```
cd apps/java/luci2
mvn clean install
mvn exec:java -pl scenario
```

#### Helen

Path: `apps/hs/helen`.
Given Java and maven are set up correctly, run Luci as follows:
```
cd apps/hs/helen
stack install
```


#### luci-connect

Path: `libs/hs/luci-connect`.
Luci-connect is a haskell library for clients and services of Luci.
Refer to `libs/hs/luci-connect/README.md` for further documentation.


### Notes

Some SQL queries become really slow when database grows (I have 1331 votes now).
Having added couple indices speeds up "compare designs" query about 5-10x.
```
CREATE INDEX ON vote (better_id);
CREATE INDEX ON vote (worse_id);
```
Maybe a better solution is to make the request itself faster later,
but for now it solved the problem.


# Running luci service together with qua-kit and helen.

If you develop a luci (qua-view-compliant) service, at some point you need to test the whole system altogether.
The framework consist of foure parties: `helen`, `siren`, `qua-kit`, and your service.
So you need to run the three things, and then use the running website to execute your service.
Note, all haskell apps (`helen`, `siren`, `qua-kit`) can be compiled using 
haskell stack tool by running `stack install --install-ghc` from the projects folders.
Note also, `siren` requires `postgis` database to be set up and running;
refer to `siren` docs for details.

  1. Compile and run `helen` (`apps/hs/helen`).
     Helen is a small app that replicates Luci core. 
  3. Compile and run `siren` (`services/siren`).
     Siren provides scenario support for helen and services.
  2. Compile and run `qua-server` (`apps/hs/qua-server`).
  4. Compile and run your service connected to localhost `helen`.
     Alternatively, you can try `dist-walls-service` executable - it has been tested to work with current version of luci and helen.
     It is available at `libs/hs/luci-connect` folder.
     To run it use following command:
     
        stack setup # you only need this once to set up GHC
        stack install
        dist-walls-service
     
  5. Go to page `http://localhost:3000/viewer`
      * (hint) Open browser console to see debug output if you have any troubles.
  6. Open toolbox -> connect to luci.
  7. Run scenario:
      * (a) Load some scenario via luci (if uploaded something before).
      * (b) Upload some scenario using `FILES` button.
            There is one available at `apps/hs/qua-server/static/data/mooctask.geojson`.
            Save it to luci.
  8. Make sure that `luci` and some service is running, then go to `SERVICES` tab.
     It should show a list of available services.
     You can select one to run it.
     Click on `refresh` button if you do not see your service in a list.
     Selecting an active service invokes parameter refreshing and display.
     Check if all optional parameters of your service are displayed as intended.
  9. Press green `play` button.
     
