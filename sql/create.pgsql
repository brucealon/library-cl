
CREATE TABLE library_users (
       library_user_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       email text,
       first_name text,
       last_name text,
       display_name text,
       hashed_password text,
       joined timestamptz,
       last_login timestamptz,
       admin boolean
);

CREATE TABLE publication_series (
       publication_series_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       title text,
       count int,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);


CREATE TABLE publication_edition_types (
       publication_edition_type_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_type text,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE publications (
       publication_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       title text,
       subtitle text,
       language text,
       sort_title text,
       url text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE publication_series_entries (
       publication_series_entry_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_id bigint REFERENCES publications(publication_id),
       publication_series_id bigint REFERENCES publication_series(publication_series_id),
       series_number text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE publication_editions (
       publication_edition_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_id bigint REFERENCES publications(publication_id),
       publication_year int,
       pages int,
       isbn text,
       type_id bigint REFERENCES publication_edition_types(publication_edition_type_id),
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE creators (
       creator_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       first_name text,
       middle_name text,
       last_name text,
       born date,
       died date,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE creator_roles (
       creator_role_id int GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       name text,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE publication_edition_creators (
       publication_edition_creator_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_edition_id bigint REFERENCES publication_editions(publication_edition_id),
       creator_id bigint REFERENCES creators(creator_id),
       creator_role_id int REFERENCES creator_roles(creator_role_id),
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_quotes (
       user_quote_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_edition_id bigint REFERENCES publication_editions(publication_edition_id),
       creator_id bigint REFERENCES creators(creator_id),
       user_id bigint REFERENCES library_users(library_user_id),
       quote text,
       page text,
       chapter text,
       section text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_creator_comments (
       user_creator_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       creator_id bigint REFERENCES creators(creator_id),
       comment text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_publication_editions (
       user_publication_edition_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_id bigint REFERENCES publications(publication_id),
       user_id bigint REFERENCES library_users(library_user_id),
       owned boolean,
       location text,
       purchased date,
       source text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_publication_edition_reads (
       user_publication_edition_read_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_edition_id bigint REFERENCES publication_editions(publication_edition_id),
       user_id bigint REFERENCES library_users(library_user_id),
       started date,
       read date,
       finished boolean,
       last_page_read text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_publication_series_comments (
       user_publication_series_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_series_id bigint REFERENCES publication_series(publication_series_id),
       user_id bigint REFERENCES library_users(library_user_id),
       comment text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_publication_comments (
       user_publication_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_id bigint REFERENCES publications(publication_id),
       user_id bigint REFERENCES library_users(library_user_id),
       comment text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_publication_reviews (
       user_publication_review_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       user_publication_edition_read_id bigint REFERENCES user_publication_edition_reads(user_publication_edition_read_id),
       stars int,
       review text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_quote_comments (
       user_quote_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       user_quote_id bigint REFERENCES user_quotes(user_quote_id),
       comment text,
       private boolean,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_users(library_user_id),
       updated_at timestamptz
);

CREATE TABLE library_audit_log (
       library_audit_log_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       event text,
       table_name text,
       inserted_by bigint REFERENCES library_users(library_user_id),
       inserted_at timestamptz
);
