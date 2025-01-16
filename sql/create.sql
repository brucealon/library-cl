
CREATE TABLE library_user (
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
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_user(library_user_id),
       updated_at timestamptz
);


CREATE TABLE publication_edition_type (
       publication_edition_type_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_type text,
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_user(library_user_id),
       updated_at timestamptz
);

CREATE TABLE publication (
       publication_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_series_id bigint REFERENCES publication_series(publication_series_id),
       title text,
       subtitle text,
       language text,
       sort_title text,
       url text,
       private boolean,
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_user(library_user_id),
       updated_at timestamptz
);

CREATE TABLE publication_edition (
       publication_edition_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_id bigint REFERENCES publication(publication_id),
       publication_year int,
       pages int,
       isbn text,
       type_id bigint REFERENCES publication_edition_type(publication_edition_type_id),
       private boolean,
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_user(library_user_id),
       updated_at timestamptz
);

CREATE TABLE creator (
       creator_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       first_name text,
       middle_name text,
       last_name text,
       born date,
       died date,
       private boolean,
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_user(library_user_id),
       updated_at timestamptz
);

CREATE TABLE creator_role (
       creator_role_id int GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       name text,
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_user(library_user_id),
       updated_at timestamptz
);

CREATE TABLE publication_edition_creator (
       publication_edition_creator_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_edition_id bigint REFERENCES publication_edition(publication_edition_id),
       creator_id bigint REFERENCES creator(creator_id),
       role int REFERENCES creator_role(creator_role_id),
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz,
       updated_by bigint REFERENCES library_user(library_user_id),
       updated_at timestamptz
);

CREATE TABLE user_quote (
       user_quote_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_edition_id bigint REFERENCES publication_edition(publication_edition_id),
       creator_id bigint REFERENCES creator(creator_id),
       user_id bigint REFERENCES library_user(library_user_id),
       quote text,
       page text,
       chapter text,
       section text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE user_creator_comment (
       user_creator_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       creator_id bigint REFERENCES creator(creator_id),
       comment text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE user_publication_edition (
       user_publication_edition_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_id bigint REFERENCES publication(publication_id),
       user_id bigint REFERENCES library_user(library_user_id),
       owned boolean,
       location text,
       purchased date,
       source text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE user_publication_edition_read (
       user_publication_edition_read_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_edition_id bigint REFERENCES publication_edition(publication_edition_id),
       user_id bigint REFERENCES library_user(library_user_id),
       started date,
       read date,
       finished boolean,
       last_page_read text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE user_publication_series_comment (
       user_publication_series_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_series_id bigint REFERENCES publication_series(publication_series_id),
       user_id bigint REFERENCES library_user(library_user_id),
       comment text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE user_publication_comment (
       user_publication_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       publication_id bigint REFERENCES publication(publication_id),
       user_id bigint REFERENCES library_user(library_user_id),
       comment text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE user_publication_review (
       user_publication_review_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       user_publication_edition_read_id bigint REFERENCES user_publication_edition_read(user_publication_edition_read_id),
       stars int,
       review text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE user_quote_comment (
       user_quote_comment_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       user_quote_id bigint REFERENCES user_quote(user_quote_id),
       comment text,
       private boolean,
       inserted_at timestamptz,
       updated_at timestamptz
);

CREATE TABLE library_audit_log (
       library_audit_log_id bigint GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
       event text,
       table_name text,
       inserted_by bigint REFERENCES library_user(library_user_id),
       inserted_at timestamptz
);
