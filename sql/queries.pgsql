
-- name: user-id
select library_user_id
  from library_users
  where email = :email

-- name: add-user
insert into library_users (email, first_name, last_name, display_name, joined, admin)
  values (:email, :first, :last, :display, now(), :admin)
  returning library_user_id

-- name: publication-by-title
select publication_id,
       title,
       subtitle
  from publications
  where title = :title
    and subtitle = :subtitle

-- name: add-publication
insert into publications (title, subtitle, private, inserted_by, inserted_at)
  values (:title, :subtitle, :private, :inserter, now())
  returning publication_id

-- name: creator-by-name
select c.creator_id,
       c.first_name,
       c.middle_name,
       c.last_name,
       c.private
  from creators as c
  where c.first_name = :first
    and c.middle_name = :middle
    and c.last_name = :last

-- name: add-creator
insert into creators (first_name, middle_name, last_name, private, inserted_by, inserted_at)
  values (:first, :middle, :last, :private, :inserter, now())
  returning creator_id

-- name: creator-role-by-name
select cr.creator_role_id
  from creator_roles as cr
  where cr.name = :name

-- name: add-creator-role
insert into creator_roles (name, inserted_by, inserted_at)
  values (:name, :inserter, now())
  returning creator_role_id

-- name: series-id-by-title
select publication_series_id
   from publication_series
   where title = :title

-- name: add-series
insert into publication_series (title, private, inserted_by, inserted_at)
  values (:title, :private, :inserter, now())
  returning publication_series_id

-- name: series-entry
select publication_series_entry_id
  from publication_series_entries
  where publication_id = :publication
    and publication_series_id = :series

-- name: add-series-entry
insert into publication_series_entries (publication_id,
                                        publication_series_id,
                                        series_number,
                                        private,
                                        inserted_by,
                                        inserted_at)
  values (:publication, :series, :number, :private, :inserter, now())
  returning publication_series_entry_id

-- name: series-entries
select p.title as book_title,
       ps.title as series_title,
       pse.series_number
  from publications as p
    join publication_series_entries as pse on p.publication_id = pse.publication_id
    join publication_series ps on pse.publication_series_id = ps.publication_series_id

-- name: edition-by-publication
select publication_edition_id
  from publication_editions
  where publication_id = :publication

-- name: add-edition
insert into publication_editions (publication_id, pages, isbn, private, inserted_by, inserted_at)
  values (:publication, :pages, :isbn, :private, :inserter, now())
  returning publication_edition_id

-- name: edition-creator-by-id
select publication_edition_creator_id
  from publication_edition_creators
  where publication_edition_id = :edition

-- name: add-edition-creator
insert into publication_edition_creators (publication_edition_id, creator_id, creator_role_id, private, inserted_by, inserted_at)
  values (:edition, :creator, :role, :private, :inserter, now())
  returning publication_edition_creator_id

-- name: user-publication-edition-read
select user_publication_edition_read_id
  from user_publication_edition_reads
  where publication_edition_id = :edition
    and user_id = :user

-- name: add-user-publication-edition-read
insert into user_publication_edition_reads (publication_edition_id, user_id, read, finished, private, inserted_by, inserted_at)
  values (:edition, :user, :read, :finished, :private, :inserter, now())
  returning user_publication_edition_read_id

-- name: user-publication-review
select user_publication_review_id,
       stars,
       review
  from user_publication_reviews
  where user_publication_edition_read_id = :read

-- name: add-user-publication-review
insert into user_publication_reviews (user_publication_edition_read_id, stars, review, private, inserted_by, inserted_at)
  values (:read, :rating, :review, :private, :inserter, now())
  returning user_publication_review_id

-- name: publication-edition-quote
select user_quote_id
  from user_quotes
  where publication_edition_id = :edition
    and user_id = :user
    and page = :page

-- name: add-publication-edition-quote
insert into user_quotes (publication_edition_id, quote, page, user_id, private, inserted_by, inserted_at)
  values (:edition, :quote, :page, :user, :private, :inserter, now())
  returning user_quote_id

-- name: user-quote-comment
select user_quote_comment_id
  from user_quote_comments
  where user_quote_id = :quote

-- name: add-user-quote-comment
insert into user_quote_comments (user_quote_id, comment, private, inserted_by, inserted_at)
  values (:quote, :comment, :private, :inserter, now())
  returning user_quote_comment_id
