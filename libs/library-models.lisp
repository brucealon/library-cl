
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

(defun user-id (user)
  (nth 0 user))

(defun user-email (user)
  (nth 1 user))

(defun user-first-name (user)
  (nth 2 user))

(defun user-last-name (user)
  (nth 3 user))

(defun user-username (user)
  (nth 4 user))

(defun user-joined (user)
  (nth 5 user))

(defun user-last-login (user)
  (nth 6 user))

(defun user-admin-p (user)
  (nth 7 user))

(defun user-hashed-password (user)
  (nth 8 user))
