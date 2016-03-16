## qua-kit
# Quick Urban Analysis Kit
=================================================

Qua-Kit is a client-server system that aims at assisting urban designers in their design process.

Qua-View (`apps/hs/qua-view`) is a WebGL-base browser viewer and editor for building geometry. It is based on Haskell GHCJS.

Luci (`apps/java/luci2`) is a lightweight middleware that allows to connect different urban computing services together
and present their results in Qua-View.


Projects that are already in some git repositories can be added as git modules
using `git submodule add <repository> <path>`.

### Java

First, install
[`JDK`](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

Second, using
[`maven`](https://maven.apache.org/)
for all java projects is highly recommended.

#### Luci

Given Java and maven are set up correctly, running luci as easy as running these four commands:
```
cd apps/java/luci2
mvn clean install
mvn exec:exec
```

### Haskell

To build and install (and run later) haskell applications use
[`stack`](http://docs.haskellstack.org/en/stable/README.html) as follows:
```
stack install
```
Otherwise, to build and run a particular app, use `build` and `exec` commands.
For examplre, to run `get-edx-mooc-images` you shoud:
```
stack build get-edx-mooc-images
stack exec get-edx-mooc-images
```
