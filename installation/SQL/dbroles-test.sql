CREATE ROLE test_database_user WITH
LOGIN
PASSWORD 'changeit';

GRANT CONNECT ON DATABASE tunedbtest TO test_database_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON tunes TO test_database_user;
GRANT USAGE ON SEQUENCE tunes_id_seq TO test_database_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON users TO test_database_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON comments TO test_database_user;
GRANT USAGE ON SEQUENCE comments_id_seq TO test_database_user;
GRANT SELECT ON roles TO test_database_user;
GRANT SELECT ON genres TO test_database_user;
GRANT SELECT ON rhythms TO test_database_user;
