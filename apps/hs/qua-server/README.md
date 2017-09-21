## qua-server
# Quick Urban Analysis Kit - Server
=================================================

This is a server side of qua-kit projet.


### Compile qua-server as a standalone executable

The fastest and recommended method to build and run qua-server is to use stack:
```
stack install --install-ghc --flag qua-server:postgresql
```
This command installs in an isolated environment qua-server executable and all its dependencies (including a compiler if needed).
Hence, it may take a while.

Note Windows users: sometimes stack install/build commands fail with strange errors on Windows;
in most cases just running the same command one or two more times solves the problem
(this error occurs in different packages every time).

To run qua-server, copy its executable from the build folder (you can find it in an output from stack install command)
to any folder you want; copy folder `static` to the same place (it contains all static web resources).
Then just run the executable.
The site will be available on `http://localhost:3000/`.


### Setup a development/deployment environment

Read this section if you want to develop `qua-server` itself.

This requires `yesod-bin` haskell package that is available in hackage and stackage.
I also use atom IDE, so I install packages `happy` and `ghc-mod`.
Altogether, run following to prepare an environment:
```
stack setup
stack build happy
stack build ghc-mod
stack build yesod-bin cabal-install
stack build
```

Run qua-server in development mode:
```
stack exec yesod devel
```

Deploy qua-server to a configured keter server:
```
stack exec yesod keter
```

### Acknowledgements

Thanks to [Daemonite's Material UI](https://github.com/Daemonite/material) team for frontend templates.

For the web server engine: [yesodweb](http://www.yesodweb.com/).
