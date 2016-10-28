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

To build and run a particular app, use `build` and `exec` commands provided by `stack`.
For examplre, to run `qua-server` you shoud:
```
stack build qua-server --flag qua-server:dev
stack exec qua-server
```
Alternatively, you can use `yesod-bin` package to run it:
```
stack install yesod-bin
yesod devel
```

#### Luci

Path: `apps/java/luci2`.
Given Java and maven are set up correctly, run Luci as follows:
```
cd apps/java/luci2
mvn clean install
mvn exec:java -pl scenario
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


# Running luci service together with qua-kit and luci.

If you develop a luci (qua-view-compliant) service, at some point you need to test a whole system altogether.
The framework consist of three parties: `luci`, `qua-kit`, and your service.
So you need to run three things, and then locate the site.

  1. Compile and run `luci`.
  2. Compile and run `qua-server`.
  3. Compile and run your service connected to localhost `luci`.
     Alternatively, you can try `dist-wall-service` executable.
     It is available at `libs/hs/luci-connect` folder.
     Use `stack install && dist-wall-service` to run it, or refer to corresponding readme to solve any problems.
     Note, that you have to use linux and have `llvm-3.5` dev libraries to run it.
  4. Go to page `http://localhost:3000/viewer`
  4. (hint) Open browser console to see debug output.
  6. Open toolbox -> connect to luci.
  7. (a) Load some scenario via luci (if uploaded something before).
  8. (b) Upload some scenario using `FILES` button.
     There is one available at `apps/hs/qua-server/static/data/mooctask.geojson`.
     Save it to luci.
  9. Make sure that `luci` and some service is running, then go to `SERVICES` tab and click on `refresh` button.
     You need it to update service manager inside JS app.
     At this moment service controls are broken, so do not look at them!
     When you press refresh you should see a list of available services in console output.
     The first service in the list is the one that runs.
  10. Press green `play` button.
     
