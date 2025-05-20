
-- export all users
\COPY (
  SELECT row_to_json(users) :: text
  FROM users
) to '/home/john/services/tunebank-node/export/usersexport.json';

-- export all Scandi tunes
\COPY (
  SELECT row_to_json(t) tunes
  FROM
    (
      SELECT submitter, ts, abc 
      FROM tunes
      wHERE genre = 'scandi'
    ) t
) to '/home/john/services/tunebank-node/export/scanditunesexport.json';

-- export all Scandi comments
\COPY (
  SELECT row_to_json(tc) tunecomments
  FROM
    (
      SELECT *
      FROM tunecomments
      wHERE genre = 'scandi'
    ) tc
) to '/home/john/services/tunebank-node/export/scandicommentsexport.json';