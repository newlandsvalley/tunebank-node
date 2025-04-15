# Differences between tunebank-node and musicrest

## Transcoding

Musicrest supports a good deal of content negotiation which is largely unnecessary.  For example, many get requests can return responses in multiple formats depending on the request.  In most cases, Tunebank-node will only return responses in JSON format.

However, support for the different encodings of a tune (ABC, MIDI etc.) will be retained.

## Uniqueness of Tune Names

Musicrest allows a tune title to be shared across different rhythms within a genre and so a tune reference is a combination of both rhythm and title.  In practice, this has never been needed. Tunebank-node identifies a tune simply by its title within any given genre. In turn, this simplifies the REST API which will not require rhythm within a request URI.