
(ql:quickload "cl-ppcre")

;; Data structure for reading in the Goodreads exported CSV.
;; I want to be able to just give a row list and get back a
;; structure, without all the additional keywords.
;;
;; title is renamed grtitle because I want extra title "fields",
;; especially "title" should just be the book title, not the whole
;; thing Goodreads stuffs in. So grtitle is short for goodreadstitle.
;;
;;=> ("Book Id" "Title" "Author" "Author l-f" "Additional Authors" "ISBN"
;;    "ISBN13" "My Rating" "Average Rating" "Publisher" "Binding"
;;    "Number of Pages" "Year Published" "Original Publication Year" "Date Read"
;;    "Date Added" "Bookshelves" "Bookshelves with positions" "Exclusive Shelf"
;;    "My Review" "Spoiler" "Private Notes" "Read Count" "Owned Copies")
(defstruct (goodreads
            (:constructor new-goodreads
                (id
                 grtitle
                 author author-lf add-authors
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
  grtitle
  author author-lf add-authors
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

(defun goodreads-author-first (book)
  (first (uiop:split-string (goodreads-author book) :separator " ")))

(defun goodreads-author-middle (book)
  (let ((names (uiop:split-string (goodreads-author book) :separator " ")))
    (if (= 3 (length names))
        (second names)
        "")))

(defun goodreads-author-last (book)
  (first (last (uiop:split-string (goodreads-author book) :separator " "))))

(defun goodreads-title (book)
  (let* ((fulltitle (goodreads-grtitle book))
         (pos (search ": " fulltitle))
         (title (if (null pos)
                    fulltitle
                    (subseq fulltitle 0 pos))))
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings "^([^\\(]+) \\(" title)
      (if match
          (aref groups 0)
          title))))

(defun goodreads-subtitle (book)
  (let* ((fulltitle (goodreads-grtitle book))
         (pos (search ": " fulltitle)))
    (if (null pos)
        ""
        (subseq fulltitle (+ pos 2)))))

(defun parse-one-series (series)
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "^([^,]+),? +#([\\d\.-]+)" series)
    (if match
        (list (aref groups 0) (aref groups 1))
        nil)))

(defun parse-series (series)
  (if (null (search ";" series))
      (if (null (search "#" series))
          (list (list series))
          (list (parse-one-series series)))
      (mapcar (lambda (s) (parse-one-series s)) (ppcre:split "; " series))))

(defun goodreads-series (book)
  (let* ((fulltitle (goodreads-grtitle book)))
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings "\\((.*)\\)" fulltitle)
      (if match
          (parse-series (aref groups 0))
          nil))))

(defun read-goodreads-csv (csv-file)
  (mapcar (lambda (csv) (apply 'new-goodreads csv))
                       (rest (cl-csv:read-csv csv-file))))
