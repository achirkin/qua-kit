## Attention ZuriHac 2017 haskellers!

I have prepared a set of [things](https://github.com/achirkin/qua-kit/issues) I want to do for the project. You are welcome to:
 * choose any issue you like
 * discuss with me its implementation
 * solve it during the event
 * ask any questions
 * propose new tasks

### "I want to code, not to spend too much time trying to undestand qua-kit internals!"

Ok! I have a nice option to write standalone programs that can be easily integrated into qua-kit. I call them "computational services". Computational service is a small console program that consumes urban geometry and produces some analysis results.
We support the following execution modes:
 * `points`: Take a grid of points and a geometry, return grid of results (qua-kit visualizes it using heatmap).
 * `object`: Compute a single value for each geometry object (floating-point value is added into object properties).
 * `scenario`: Do whatever you want with geometry and return a single line of text or an image (e.g. some plot).
 * `new`: Create new geometry (or modify existing).

Example of such a service is available in `services/examples-hs/` - it computes distance to a closest object for each point on a grid. You can see how it works on our live page:
 1. Go to https://qua-kit.ethz.ch/viewer
 2. To be able to connect to luci, you need to login. The default admin credentials are:
    * username: "admin@qua-kit.hs"
    * password: "make it some random thing"
 3. Open control panel (Red "Tools" button -> gear button)
 4. Press button "CONNECT" (connect to luci)
 5. Press "SCENARIOS" and select "Empower Shack" on the bottom of the list
 6. Go to "SERVICES" tab
 7. There will be one service available "hs-example-service". Make sure "points" mode is selected.
 8. Press green button "Play" to run service
 9. You will see heatmap-like visualization for mode "points"


# Quick Urban Analysis Kit


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
Helen can also run dependend executables, i.e. services.
Here is an example of helen config to run qua-kit and all services.
```yaml
host:             127.0.0.1
port:             7654
loglevel:         info
restart-attempts: 3

trusted-clients:
  - 127.0.0.1

bundled-services:
  - name: siren service
    executable: services/siren
  - name: dist to walls service
    executable: services/hs-example-service
  - name: qua-kit runtime
    executable: services/qua-server
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
     Alternatively, you can try `hs-example-service` executable - it has been tested to work with current version of luci and helen.
     It is available at `services/examples-hs` folder.
     To run it use following command:
     
        stack setup # you only need this once to set up GHC
        stack install
        hs-example-service
     
  5. Go to page `http://localhost:3000/viewer`
      * (hint) Open browser console to see debug output if you have any troubles.
  6. To be able to connect to luci, you need to login. The default admin credentials are:
      * username: "admin@qua-kit.hs"
      * password: "make it some random thing"
  7. Open toolbox -> connect to luci.
  8. Run scenario:
      * (a) Load some scenario via luci (if uploaded something before).
      * (b) Upload some scenario using `FILES` button.
            There is one available at `apps/hs/qua-server/static/data/mooctask.geojson`.
            Save it to luci.
  9. Make sure that `luci` and some service is running, then go to `SERVICES` tab.
     It should show a list of available services.
     You can select one to run it.
     Click on `refresh` button if you do not see your service in a list.
     Selecting an active service invokes parameter refreshing and display.
     Check if all optional parameters of your service are displayed as intended.
  10. Press green `play` button.
     
