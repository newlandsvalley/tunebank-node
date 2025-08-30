#!/bin/bash

psql -U john tunedbtest --command \
"\COPY (
  SELECT row_to_json(users) :: text
  FROM users
 ) to '/home/john/services/tunebank-node/export/usersexport.json'"
