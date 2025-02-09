
(defgeneric book-creator-first (book))

(defgeneric book-creator-last (book))

(defgeneric book-creator-middle (book))

(defgeneric book-creator-role (book))

(defgeneric book-date-read (book))

(defgeneric book-isbn (book))

(defgeneric book-pages (book))

(defgeneric book-quote-comment (book))

(defgeneric book-quote-end (book))

(defgeneric book-quote-start (book))

(defgeneric book-quote-text (book))

(defgeneric book-rating (book))

(defgeneric book-read (book))

(defgeneric book-series (book)
  (:documentation "Must be nil or a list of lists where each inner list
contains the series title and the series number,
so ((\"Elvis Cole\" 7) (\"Joe Pike\" 2))."))

(defgeneric book-subtitle (book))

(defgeneric book-title (book))
