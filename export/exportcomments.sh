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
  SELECT row_to_json(tc) tunecomments
  FROM
    (
      SELECT *
      FROM tunecomments
      wHERE genre = '$genre'
    ) tc
) to '/home/john/services/tunebank-node/export/${genre}commentsexport.json'"


