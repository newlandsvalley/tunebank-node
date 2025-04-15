# URL Scheme


URL path segments in italics represent fixed text; those in bold type are variable.  For example, the text 'genre' is fixed whilst **agenre** can be any of _english, irish, scandi, scottish or klezmer_. The following URLs are supported:

##### Tunes

*  GET / _musicrest_ / _genre_ - get a list of genres.

*  GET / _musicrest_ / _genre_ / **agenre** - get a list of rhythms appropriate for the genre.

*  GET / _musicrest_ / _genre_ / **agenre** / _tune_ - get a paged list of tunes.

*  POST / _musicrest_ / _genre_ / **agenre** / _tune_ - submit a tune in ABC format.

*  GET / _musicrest_ / _genre_ / **agenre** / _exists_ - return true if the genre exists

*  GET / _musicrest_ / _genre_ / **agenre** / _tune_ / **title** - get a tune in the format suggested by the Accept header.

*  DELETE / _musicrest_ / _genre_  / **agenre** / _tune_ / **title** - delete the tune from the database.

*  GET / _musicrest_ / _genre_ / **agenre** / _tune_ / **title** / _exists_ - return true if the tune exists

*  GET / _musicrest_ / _genre_ / **agenre** / _tune_ / **title** / **format** - get a tune the requested format (which can be abc, html, wav, pdf, ps or midi)

*  GET / _musicrest_ / _genre_ / _search_ - get a paged list of tunes that correspond to the search parameters.

A user must be logged in before he can submit a tune. Only the original submitter or the *administrator* user is allowed to delete tunes.  


##### Comments

*  GET / _musicrest_ / _genre_ / **agenre** / _tune_ / **title** / _comments_ - get the comments attached to a tune

*  POST / _musicrest_ / _genre_ / **agenre** / _tune_ / **title** / _comments_ - add or edit a comment and attach it to a tune

*  GET / _musicrest_ / _genre_ / **agenre** / _tune_ / **title** / _comment_ / **auser** / **acommentid** - get a particular comment (written by the user with this id)

*  DELETE / _musicrest_ / _genre_ / **agenre** / _tune_ / **title** / _comment_ / **auser** / **acommentid** - delete this comment

*  DELETE / _musicrest_ / _genre_ / **agenre** / _comments_  - delete all comments in the genre

##### Users

There is also a fairly conventional set of URLs for user maintenance.

#### _URL Parameters_

URLs that return lists take optional paging parameters indicating the number of entries on a page and the identity of the page to display. 
