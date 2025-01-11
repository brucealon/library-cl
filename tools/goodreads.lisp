
(ql:quickload "cl-csv")

;;=> ("Book Id" "Title" "Author" "Author l-f" "Additional Authors" "ISBN"
;;    "ISBN13" "My Rating" "Average Rating" "Publisher" "Binding"
;;    "Number of Pages" "Year Published" "Original Publication Year" "Date Read"
;;    "Date Added" "Bookshelves" "Bookshelves with positions" "Exclusive Shelf"
;;    "My Review" "Spoiler" "Private Notes" "Read Count" "Owned Copies")
(defstruct (goodread
            (:constructor new-goodread
                (id
                 title
                 author author-l-f add-authors
                 isbn isbn13
                 rating avg-rating
                 publisher
                 binding
                 pages
                 year-published org-pub-year
                 date-read date-add
                 shelves shelves-with-pos exclusive-shelf
                 review
                 spoiler
                 notes
                 read-count
                 owned)))
  id
  title
  author author-l-f add-authors
  isbn isbn13
  rating avg-rating
  publisher
  binding
  pages
  year-published org-pub-year
  date-read date-add
  shelves shelves-with-pos exclusive-shelf
  review
  spoiler
  notes
  read-count
  owned)

(let ((books (mapcar (lambda (csv) (apply 'new-goodread csv))
                     (rest (cl-csv:read-csv #P"/home/brking/Downloads/goodreads_library_export.csv")))))
  (goodread-author (nth 31 books)))
