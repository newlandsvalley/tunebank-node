# Tunebank-node

An experimental rewrite of the [musicrest](https://github.com/newlandsvalley/musicrest) web service in Purescript on node. The backend is `Postgres` and the web server framework is `HTTPurple`.

## _Usage_

### _Genres and Rhythms_

Tunebank-node is quite opinionated about the ABC that it accepts.  It has the concept of a `genre` of music and the current release is restricted to five - _English, Irish, Scandi, Scottish_ and _Klezmer_. Within a genre, it recognizes a set of `rhythms` appropriate to that genre and the `R:` (rhythm) header in the ABC of any submitted tune must match one of these.

It also rejects tunes that use chord symbols.

Finally, each tune within a genre is recognized by its title which must be unique.

### _Authorization_

Users need not be registered to be allowed to issue `GET` requests for any tune or comment.  However, they must be registered in order to submit a tune or comment or to delete or update one.  If amending or deleteting a resource, they must have been the original submitter.  However, certain users are allowed admin privileges and they have full access to the database.

## URL Scheme

The RESTful [URL Scheme](https://github.com/newlandsvalley/Tunebank-node/blob/master/URL-SCHEME.md) is somewhat simplified from that of `musicrest`. The number of [differences](https://github.com/newlandsvalley/Tunebank-node/blob/master/DIFFERENCES.md) between the two is fairly small.

## Populating the Test Database 

The postgres SQL scripts that initialise the test database include the DDL statements and scripts that populate the genres, rhythms, roles and user tables with essential static data.  These are to be found in the `installation/SQL` directory.

In addition, the `installation/abc-samples` directory contains a set of Scandi tunes in ABC format.  These are reloaded automatically by the test framework at each invocation.

## Building 

spago build

## Testing

  * Run the SQL scripts
  * Run up the `tunebank-node` server using `spago run` (required for the integration tests).
  * Run the tests - `spago test`