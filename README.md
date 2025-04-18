# Tunebank-node

An experimental rewrite of the [musicrest](https://github.com/newlandsvalley/musicrest) web service in Purescript on node. The backend is `Postgres` and the web server framework is `HTTPurple`.


## URL Scheme

The RESTful [URL Scheme](https://github.com/newlandsvalley/Tunebank-node/blob/master/URL-SCHEME.md) is somewhat simplified from that of `musicrest`. This section summarisess the main [differences](https://github.com/newlandsvalley/Tunebank-node/blob/master/DIFFERENCES.md) between the two.

## Populating the Test Database 

The postgres SQL scripts that initialise the test database include the DDL statements and scripts that populate the genres, rhythms, roles and user tables with essential static data.  These are to be found in the `installation/SQL` directory.

In addition, the `installation/abc-samples` directory contains a set of Scandi tunes in ABC format.  These are reloaded automatically by the test framework at each invocation.

## Building 

spago build

## Testing

  * Run the SQL scripts
  * Run up the `tunebank-node` server using `spago run` (required for the integration tests).
  * Run the tests - `spago test`