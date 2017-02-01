## qua-server
# Quick Urban Analysis Kit - Server
=================================================

This is a server side of qua-kit projet.


#### Using yesod toolset for development and deployment

This requires `yesod-bin` haskell package that is available in hackage and stackage.

Deploy qua-server to a configured keter server:
```
yesod keter
```

Run qua-server in development mode:
```
yesod devel
```

Run test suite:
```
yesod test
```
#### Usging stack

Make sure local ghc version is there and build yesod build tools:
```
stack setup
stack build yesod-bin cabal-install
```
And then all commands should work via stack, e.g.:
```
stack exec yesod devel
```

#### Acknowledgements

Thanks to [Daemonite's Material UI](https://github.com/Daemonite/material) team for frontend templates.

For the web server engine: [yesodweb](http://www.yesodweb.com/).
