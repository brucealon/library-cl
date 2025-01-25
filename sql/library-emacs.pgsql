
select p.title as book_title,
       c.first_name,
       c.last_name,
       coalesce(ps.title, '') as series_title,
       coalesce(pse.series_number, '') as series_number
  from publications p
    join publication_editions pe on p.publication_id = pe.publication_id
    join publication_edition_creators pec on pe.publication_edition_id = pec.publication_edition_id
    join creators c on pec.creator_id = c.creator_id
    left join publication_series_entries pse on p.publication_id = pse.publication_id
    left join publication_series ps on pse.publication_series_id = ps.publication_series_id
  where c.last_name = 'Friedman'
  order by series_title, series_number, book_title
