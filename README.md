## qua-kit
# Quick Urban Analysis Kit
=================================================

Qua-Kit is a client-server system that aims at assisting urban designers in their design process.

Qua-View is a WebGL-base browser viewer and editor for building geometry. It is based on Haskell GHCJS.

Luci (luci2) is a lightweight middleware that allows to connect different urban computing services together
and present their results in Qua-View.

The repository structure is taken from Elevence Digital Finance AG
according to
[Simon Meier](https://raw.githubusercontent.com/meiersi/HaskellerZ/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/).

Our code-layout's intended use is the following:
```
▾ apps/          -- code for all our applications
  ▾ hs/          --   the ones written in Haskell
    ▾ qua-view/  --     an example Haskell application
        ...
  ▾ java/        --   the ones written in Java
    ▾ luci2/     --     an example Java application
        ...
▾ docs/          -- all documentation
    ...
▾ dotfiles/      -- shared configuration files
    .some_config -- example of such
    ...
▾ libs/          -- code for all our libraries
  ▾ hs/          --   the ones written in Haskell
      ...
  ▾ java/        --   the ones written in Java
      ...
▾ scratch/       -- scratch directories to host experiments by
  ▾ achirkin/    -- individual employees
      ...
```

Projects that are already in some git repositories can be added as git modules
using `git submodule add <repository> <path>`.

### Java

First, install
[`JDK`](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

Second, using
[`maven`](https://maven.apache.org/)
for all java projects is highly recommended.
Download and install the tool and use `mvn` commands to compile and deploy apps.
For example, compiling `cdm-processor` looks like this: 
```
cd apps/java/cdm-processor
mvn clean jfx:jar
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
