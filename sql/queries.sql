
-- name: library-user
-- Get the full user information by email
select library_user_id,
       first_name,
       last_name,
       display_name,
       joined,
       last_login,
       admin
  from library_user
  where email = :email

-- name: user-id @single
-- Get a user id by email
select library_user_id
  from library_user
  where email = :email

-- name: add-user @single
-- Insert a user into the database
insert into library_user (email, first_name, last_name, display_name, admin)
  values (:email, :first, :last, :display, :admin)
  returning library_user_id

-- name: publications
-- Get publications with creators
select p.title,
       p.subtitle,
       c.first_name,
       c.last_name
  from publication as p
    join publication_edition as pe on p.publication_id = pe.publication_id
    join publication_edition_creator as pec on pe.publication_edition_id = pec.publication_edition_id
    join creator as c on pec.creator_id = c.creator_id

-- name: publication-by-title
select title,
       subtitle
  from publication
  where title = :title
    and subtitle = :subtitle

-- name: add-publication
insert into publication (title, subtitle, private, inserted_by, inserted_at)
  values (:title, :subtitle, :private, :user, now())
  returning publication_id

-- name: creators
-- Get all creators in the database
select c.first_name,
       c.middle_name,
       c.last_name
  from creator as c

-- name: creator-by-name
-- Get one creator by first, middle, and last
select c.creator_id,
       c.first_name,
       c.middle_name,
       c.last_name,
       c.private
  from creator as c
  where c.first_name = :first
    and c.middle_name = :middle
    and c.last_name = :last

-- name: add-creator
-- Insert one creator into the table with some default values.
insert into creator (first_name, middle_name, last_name, private, inserted_by, inserted_at)
  values (:first, :middle, :last, :private, :user, now())
  returning creator_id
