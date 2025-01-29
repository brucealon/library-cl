
(ql:quickload "cl-csv")
(ql:quickload "cl-ppcre")

(load "library-models.lisp")

;; 00 "Book Id"
;; 01 "Title"
;; 02 "Author"
;; 03 "Author l-f"
;; 04 "Additional Authors"
;; 05 "ISBN"
;; 06 "ISBN13"
;; 07 "My Rating"
;; 08 "Average Rating"
;; 09 "Publisher"
;; 10 "Binding"
;; 11 "Number of Pages"
;; 12 "Year Published"
;; 13 "Original Publication Year"
;; 14 "Date Read"
;; 15 "Date Added"
;; 16 "Bookshelves"
;; 17 "Bookshelves with positions"
;; 18 "Exclusive Shelf"
;; 19 "My Review"
;; 20 "Spoiler"
;; 21 "Private Notes"
;; 22 "Read Count"
;; 23 "Owned Copies"

(defclass goodreads-book ()
  (data))

(defun new-goodreads (csv-row)
  (let ((instance (make-instance 'goodreads-book)))
    (setf (slot-value instance 'data) csv-row)
    instance))

(defun read-goodreads-csv (csv-file)
  (mapcar (lambda (csv) (funcall 'new-goodreads csv))
                       (rest (cl-csv:read-csv csv-file))))

(defmethod book-author-first (goodreads-book)
  (first (uiop:split-string (nth 2 (slot-value goodreads-book 'data)) :separator " ")))

(defmethod book-author-last (goodreads-book)
  (first (last (uiop:split-string (nth 2 (slot-value goodreads-book 'data)) :separator " "))))

(defmethod book-author-middle (goodreads-book)
  (let ((names (uiop:split-string (nth 2 (slot-value goodreads-book 'data)) :separator " ")))
    (if (= 3 (length names))
        (second names)
        "")))

(defmethod book-date-read (goodreads-book)
  (nth 14 (slot-value goodreads-book 'data)))

(defmethod book-isbn (goodreads-book)
  (nth 5 (slot-value goodreads-book 'data)))

(defmethod book-pages (goodreads-book)
  (nth 11 (slot-value goodreads-book 'data)))

(defmethod book-rating (goodreads-book)
  (nth 7 (slot-value goodreads-book 'data)))

(defmethod book-read (goodreads-book)
  (let* ((count (nth 22 (slot-value goodreads-book 'data)))
         (intcount (cond ((typep count 'integer) count)
                         ((typep count 'string) (parse-integer count))
                         (t 0))))
     (> intcount 0)))

(defmethod book-series (goodreads-book)
  (let* ((fulltitle (nth 1 (slot-value goodreads-book 'data))))
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings "\\((.*)\\)" fulltitle)
      (if match
          (parse-series (aref groups 0))
          nil))))

(defmethod book-subtitle (goodreads-book)
  (let* ((fulltitle (nth 1 (slot-value goodreads-book 'data)))
         (pos (search ": " fulltitle)))
    (if (null pos)
        ""
        (subseq fulltitle (+ pos 2)))))

(defmethod book-title (goodreads-book)
  (let* ((fulltitle (nth 1 (slot-value goodreads-book 'data)))
         (pos (search ": " fulltitle))
         (title (if (null pos)
                    fulltitle
                    (subseq fulltitle 0 pos))))
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings "^([^\\(]+) \\(" title)
      (if match
          (aref groups 0)
          title))))

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
