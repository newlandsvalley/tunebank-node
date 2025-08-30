#!/bin/bash

EXPECTED_ARGS=1

if [ $# -ne $EXPECTED_ARGS ]
then
  echo "Usage: `basename $0` {genre}"
  exit $E_BADARGS
fi

genre=$1

psql -U john tunedbtest --command \
"\COPY (
  SELECT row_to_json(t) tunes
  FROM
    (
      SELECT submitter, ts, abc 
      FROM tunes
      wHERE genre = '$genre'
    ) t
) to '/home/john/services/tunebank-node/export/${genre}tunesexport.json'"


