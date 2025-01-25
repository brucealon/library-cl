
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
  values (:title, :subtitle, :private, :user, now())
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
  values (:first, :middle, :last, :private, :user, now())
  returning creator_id

-- name: series-id-by-title
select publication_series_id
   from publication_series
   where title = :title

-- name: add-series
insert into publication_series (title, private, inserted_by, inserted_at)
  values (:title, :private, :user, now())
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
  values (:publication, :series, :number, :private, :user, now())
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
insert into publication_editions (publication_id, private, inserted_by, inserted_at)
  values (:publication, :private, :user, now())
  returning publication_edition_id

-- name: edition-creator-by-id
select publication_edition_creator_id
  from publication_edition_creators
  where publication_edition_id = :edition

-- name: add-edition-creator
insert into publication_edition_creators (publication_edition_id, creator_id, private, inserted_by, inserted_at)
  values (:edition, :creator, :private, :user, now())
  returning publication_edition_creator_id
