
-- name: user-id @single
select library_user_id
  from library_users
  where email = :email

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
