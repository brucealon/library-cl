
-- name: user-id @single
-- Get a user id by email
select library_user_id
  from library_user
  where email = :email

-- name: publication-by-title
select publication_id,
       title,
       subtitle
  from publication
  where title = :title
    and subtitle = :subtitle

-- name: add-publication
insert into publication (title, subtitle, private, inserted_by, inserted_at)
  values (:title, :subtitle, :private, :user, now())
  returning publication_id

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
