# Differences between tunebank-node and musicrest

## Transcoding

Musicrest supports a good deal of content negotiation which is largely unnecessary.  For example, many get requests can return responses in multiple formats depending on the request.  In most cases, Tunebank-node will only return responses in JSON format.

However, support for the different encodings of a tune (ABC, MIDI etc.) will be retained.

## Uniqueness of Tune Names

Musicrest allows a tune title to be shared across different rhythms within a genre and so a tune reference is a combination of both rhythm and title.  In practice, this has never been needed. Tunebank-node identifies a tune simply by its title within any given genre. In turn, this simplifies the REST API which will not require rhythm within a request URI.

## Comments

Musicrest requires the client to generate a comment key derived from the current timestamp and the client will send a Json form which includes both this id and the name of the user who is posting the comment. Tunebank-node generates a unique key for the comment and also generates a timestamp.  The client, when posting a new comment, simply sends a Json form with the title and the text and, if updating, includes also the commentid it has retrieved from the server which is used in the POST url.

## Tune Uploads

Tunebank-node merely requires an ABC file to be uploaded in string format whereas Musicrest required it to be sent as form-url-encoded using the tag `abc`.