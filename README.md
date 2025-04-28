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

The postgres SQL scripts that initialise the test database include the DDL statements and scripts that populate the genres, rhythms, roles and user tables with essential static data.  These are to be found in the `installation/SQL` directory. These scripts include a file named `dbroles-test.sql` which defines a user name of `test_database_user` with a password of `changeit`. This username is used by the test framework when accessing the database.  

The `installation/abc-samples` directory contains a set of Scandi tunes in ABC format.  These are reloaded automatically by the test framework at each invocation.

## Configuration

Configuration is by means of `tunebank.conf` in the `conf` directory.  This is provided as `prototype-tunebank.conf` and uses the `test_database_user` as described above. The `mail` section allows connection only to [ethereal](https://ethereal.email/) which is a fake SMTP service suitable for testing purposes only. The mail authorization section uses a randonly generated user and password which has been pre-registered with ethereal. (SMTP is used only to complete new user registration). This prototype file should be renamed to `tunebank.conf` before testing.

## Building 

spago build

## Testing

  * Install Postgres using your own superuser name and create a database named `tunedbtest`.
  * Copy `prototype-tunebank.conf` to `tunebank.conf`.
  * Run the SQL scripts in alphabetical order.
  * Run up the `tunebank-node` server using `spago run` (required for the integration tests).
  * Run the tests - `spago test`.
  * Check the server log for mail confirmation.  This should establish that a user registration message has been sent to an ethereal url which you can then inspect.

### Security - Testing

To do

### Security - Production

Once you have successfully run the tests, you are in a position to install a production server.  You should create a Postgres database with a production database name, provide a production version of `dbroles-test.sql` with a production user name and password and provide a production version of `tunebank.conf` which uses these production names and passwords. In addition, you will need to use your own SMTP service.

Once you have these, make your own copy of `tunebank.conf` and edit this to provide the appropriate connections to yourt database and email service.

