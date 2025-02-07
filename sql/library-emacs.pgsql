
select p.title as book_title,
       c.first_name,
       c.last_name,
       coalesce(ps.title, '') as series_title,
       coalesce(pse.series_number, '') as series_number,
       luq.display_name as quote_entered_by,
       uq.quote
  from publications p
    join publication_editions pe on p.publication_id = pe.publication_id
    join publication_edition_creators pec on pe.publication_edition_id = pec.publication_edition_id
    join creators c on pec.creator_id = c.creator_id
    left join publication_series_entries pse on p.publication_id = pse.publication_id
    left join publication_series ps on pse.publication_series_id = ps.publication_series_id
    left join user_quotes uq on pe.publication_edition_id = uq.publication_edition_id
    left join library_users luq on uq.user_id = luq.library_user_id
  where c.last_name = 'Friedman'
    and uq.quote is not null
    and uq.quote like '%challenge%'
  order by series_title, series_number, book_title
