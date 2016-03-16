## qua-kit
# Quick Urban Analysis Kit
=================================================

Qua-Kit is a client-server system that aims at assisting urban designers in their design process.

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

#### qua-view
To build and run a particular app, use `build` and `exec` commands provided by `stack`.
For examplre, to run `qua-server` you shoud:
```
stack build qua-server
stack exec qua-server
```

#### Luci
Given Java and maven are set up correctly, run Luci as follows:
```
cd apps/java/luci2
mvn clean install
mvn exec:exec
```
