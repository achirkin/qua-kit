### siren

Siren is a geometry scenario management service.
It is meant to be used with Luci/helen communication servers.

#### Requirements

The service is base on PostgreSQL database and its extension PostGIS.
This means that to build and run the service, you need to install those.
On Ubuntu 16.04 I am using following command for this purpose:
```
  sudo apt-get install postgresql postgis libpq-dev
```
Currently, `postgresql` package refers to `postgresql-9.5`,
`postgis` package refers to `postgresql-9.5-postgis-2.2`,
and `libpq-dev` is a library required to build PostgreSQL client applications.


#### NB on using Atom

Just as a reminder:
if using atom editor with `haskell-ghc-mod`, you need to build a `ghc-mod` executable
in the project folder.
Therefore, to install ghc-mod, type following:
```
  stack setup
  stack build happy
  stack build ghc-mod
```
Also, do not forget to remove folder `dist` if cabal created it,
and make sure atom plugin uses stack sandboxing.
