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

## Project Structure

The project is structured as a polyrepo.  The main library module is in the `tunebank` subdirectory with three apps that depend on it - `server`, `migration` and `importation`. Tests are contained in the server module. There is also a small library module for parsing command-line arguments - `arg-parser` - shared by the migration and importation modules. Export is handled simply by using postgres psql scripts.

## Populating the Test Database 

The postgres SQL scripts that initialise the test database include the DDL statements and scripts that populate the genres, rhythms, roles and user tables with essential static data.  These are to be found in the `installation/SQL` directory. These scripts include a file named `dbroles-test.sql` which defines a user name of `test_database_user` with a password of `changeit`. This username is used by the test framework when accessing the database.  

## Configuration

Configuration is by means of `tunebank.conf` in the `server/conf` directory  (and also needed in the `migration/conf` and `importation/conf` directories).  This is provided as `prototype-tunebank.conf` and uses the `test_database_user` as described above. 

The `mail` section allows connection to [ethereal](https://ethereal.email/) which is a fake SMTP service suitable for testing purposes only. The mail authorization section describes a randonly generated user and password but this is not used. If the mail host is set to `smtp.ethereal.email` then a testing environment is assumed and mail is issued to a standard ethereal testing address. (SMTP is used only in creation of or updates to user registration). 

The prototype file should be renamed to `tunebank.conf` before testing.

## Reliance on a Reverse Proxy

It is assumed that a reverse-proxy server will be configured between the frontend and the tunebank-node server such that tunebank-node URLs will appear to belong to the set of URLs supported by the frontend - specifically the URL `https://frontend/tunebank` should proxy requests to `http://${server.host}:${server.port}`.  This is particularly important where tunebank-node issues an email to a potential user wishing to register which contains a link to complete the registration. This URL is to the frontend server proxy address and uses the server name defined in the configuration under `${mail.frontend}`.

## Logging

Logging is via `Winston` and the `Winston daily rotate file` module and writes to the `logs` directory as defined in the configuration file. At the moment, logging is fairly minimal - when the server starts or shuts down, when users attempt to register or log in, when unexpected errors occur.

## Building and Running

  * Install libsystemd and all the npm dependencies

  * to build the server - `npm run build-server`
  * to bundle the server - `npm run bundle-server`
  * to run the server - `npm run server`

  * to build the migration utility - `npm run build-migration`
  * to bundle the migration utility - `npm run bundle-migration`

  * to build the importation utility - `npm run build-importation`
  * to bundle the importation utility - `npm run bundle-importation`

### Setup

  * Install Postgres using your own superuser name and create a database named `tunedbtest`.
  * Copy `prototype-tunebank.conf` to `tunebank.conf`.
  * Run the SQL scripts in alphabetical order.
  * Run up the `tunebank-node` server (required for the integration tests) - `npm run server`.

## Testing

The `server/testdata/abc-samples` directory contains a set of Scandi tunes in ABC format.  These are reloaded automatically by the test framework at each invocation.

### Running the Tests

  * Run up a server in a different window (required by the integration tests).
  * Run the tests - `npm run test`.
  * Check the server log for mail confirmation.  This should establish that a user registration message has been sent to an ethereal url which you can then inspect.

## Production

### Security

Once you have successfully run the tests, you are in a position to install a production server.  You should create a Postgres database with a production database name, provide a version of `dbroles-test.sql` perhaps named `dbtoles-production.sql` with your own database user name and password and provide a version of `tunebank.conf` which uses this names and password. In addition, you will need to use your own SMTP service and configure it appropriately.

### Running the Production Server

  * Ensure node is installed on your production server
  * Bundle the app into a single file `tunebank.js` using `npm run bundle-server`.
  * Create a home directory to house your server and copy tunebank.js to it.
  * Make it executable - `chmod 777 tunebank.js`
  * Create a subdirectory `conf` and copy into it your production tunebank.conf
  * Run using `./tunebank.js`.
  * (You can then set up appropriate scripting for running as a background service)

## Migration from Musicrest

The migration package handles migration from Musicrest. Musicrest exports users, comments and tunes as rows of JSON in a proprietary format.  The migration package then loads this data into a staging or production server. To use this, first bundle the migration package - `npm run bundle-migration` which produces a runnable javascript file - `migrate.js` and copy this to the home directory of your production server and make it an executable.

Migration assumes the same subdirectory - `conf` that the server relies on.  It also requires a directory named `migration` where the files exported from Musicrest live. 

Migration uses a naturalistic command line.  For example:

  * .\migrate.js users 
  * .\migrate.js english tunes 
  * .\migrate.js scandi comments

## Export as JSON

Excellent facilities exist within postgres to export data as rows of JSON records.  The `export` directory contains example scripts that can be run via `psql` and which respectively export users, Scandi tunes and Scandi comments.  These can be used to export all tunebank data in a format suitable for later import.

The only downside is that postgres prefaces escape sequences such as `"\n"` with a further backslash.  This particularly affects tune ABC and comments and has to be cleaned up by the importer.

## Import of exported JSON files

Import (of previously exported JSON files) is similar to migration. First bundle the importation package - `npm run bundle-importation` which produces a runnable javascript file - `import.js` and copy this to the home directory of your production server and make it an executable.

Again, exportation assumes the same subdirectory - `conf` that the server relies on.  It also requires a directory named `export` where the files previously exported from Tunebank live. 

As for migration, it uses a naturalistic command line.  For example:

  * .\import.js users 
  * .\import.js english tunes 
  * .\import.js scandi comments




