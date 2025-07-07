## URL Scheme


URL path segments in italics represent fixed text; those in bold type are variable.  For example, the text 'genre' is fixed whilst **agenre** can be any of _english, irish, scandi, scottish or klezmer_.

The following URLs are supported:

#### Tunes

*  GET / _genre_ - get a list of genres.
*  GET / _genre_ / **agenre** / _rhythm_ - get a list of rhythms appropriate for the genre.
*  GET / _genre_ / **agenre** / _tune_ - get a paged list of tunes.
*  POST / _genre_ / **agenre** / _tune_ - submit a tune in ABC format.
*  GET / _genre_ / **agenre** / _exists_ - return true if the genre exists
*  GET / _genre_ / **agenre** / _tune_ / **title** - get the tune metadata (many of the ABC header values plus the ABC in Json format)
*  GET / _genre_ / **agenre** / _tune_ / **title** / _abc_ - get the tune in ABC format.
*  DELETE / _genre_  / **agenre** / _tune_ / **title** - delete the tune from the database.
*  GET / _genre_ / **agenre** / _tune_ / **title** / _exists_ - return true if the tune exists
*  GET / _genre_ / **agenre** / _tune_ / **title** / **format** - get a tune the requested format (which can be abc, pdf or midi)
*  GET / _genre_ / **agenre** / _search_ - get a paged list of tunes that correspond to the search parameters.

A user must be logged in before she can submit a tune. Only the original submitter or a user wth `Administrator` privileges is allowed to update or delete tunes, otherwise a `Forbidden (403)` response is returned.

#### Comments

*  GET / _genre_ / **agenre** / _tune_ / **title** / _comments_ - get the comments attached to a tune
*  POST / _genre_ / **agenre** / _tune_ / **title** / _comments_ - add a comment and attach it to a tune
*  GET / _comment_ / **acommentid** - get a particular comment
*  POST / _comment_ / **acommentid** - update a particular comment
*  DELETE / _comment_ / **acommentid** - delete this comment

Only the original submitter of the tune or a user wth `Administrator` privileges is allowed to update or delete comments, otherwise a `Forbidden (403)` response is returned.


#### Users

*  GET / _user_  - get a paged list of users 
*  POST / _user_  - add a new (as yet not fully validated) user
*  GET / _user_  / _validate_ / **uuid** - validate a user by checking the uuid returned to us
*  GET / _user_ / **username** - get the details of a given user
*  DELETE / _user_ / **username** - delete the given user
*  POST / _user_ / _getName_ - get the name of the user from the email address
*  POST / _user_ / _newPassword_ - update the password for the logged-in user
*  POST / _user_ / _newPassword_ - update the password for the logged-in user
*  POST / _user_ / _newPasswordOTP_ - send the user a one-time-password to prepare for a password change
*  GET / _user_ / check - check that the user in the Auth header is known to us
*  GET / _user_ / search - get a paged list of users 

Only an user with Administrator` privileges is allowed to access user records


#### _URL Parameters_

URLs that return lists take optional paging parameters indicating the number of entries on a page and the identity of the page to display. The default is to display the first page with no more than 15 entries to the page.

#### CORS Headers

In order to support requests which emanate from a JavaScript `HttpRequest`, the URL scheme provides appropriate CORS headers.  All responses include an `Access-Control-Allow-Origin` response header and all `POST` or `DELETE` requests induce a set of pre-flight response headers which respond to an `OPTIONS` request that the browser will issue in these cases.

#### Error Responses

The set of possible HTTP errors that may be returned from the server is restricted to:

  * `BadRequest` (400) plus message
  * `Unauthorized` (401)
  * `Forbidden` (403) plus message
  * `InternalServerError` (500)

Error responses messages are returned in JSON format with a single tag - `message`. 