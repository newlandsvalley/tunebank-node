CREATE ROLE tunebank_api WITH
LOGIN
PASSWORD 'Brudmarsch';

GRANT CONNECT ON DATABASE tunedbtest TO tunebank_api;
GRANT SELECT, INSERT, UPDATE, DELETE ON tunes TO tunebank_api;
GRANT USAGE ON SEQUENCE tunes_id_seq TO tunebank_api;
GRANT SELECT, INSERT, UPDATE, DELETE ON users TO tunebank_api;
GRANT SELECT, INSERT, UPDATE, DELETE ON comments TO tunebank_api;
GRANT USAGE ON SEQUENCE comments_id_seq TO tunebank_api;
GRANT SELECT ON roles TO tunebank_api;
GRANT SELECT ON genres TO tunebank_api;
GRANT SELECT ON rhythms TO tunebank_api;
