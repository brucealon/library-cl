
(load "goodreads-models.lisp")

(defparameter *title-tests*
  '(("Ancillary Justice (Imperial Radch, #1)"                    "Ancillary Justice"  (("Imperial Radch" "1")))
    ("This Virtual Night (Alien Shores #2)"                      "This Virtual Night" (("Alien Shores" "2")))
    ("1Q84 (1Q84, #1-3)"                                         "1Q84"               (("1Q84" "1-3")))
    ("A Dangerous Man (Elvis Cole, #18; Joe Pike, #7)"           "A Dangerous Man"    (("Elvis Cole" "18") ("Joe Pike" "7")))
    ("The Last Wish (The Witcher, #0.5)"                         "The Last Wish"      (("The Witcher" "0.5")))
    ("AI Assistants (The MIT Press Essential Knowledge series)"  "AI Assistants"      (("The MIT Press Essential Knowledge series")))
    ("Sabriel (Abhorsen,  #1)"                                   "Sabriel"            (("Abhorsen" "1")))))

(defun new-goodreads-test (title)
  (new-goodreads nil
                 title
                 nil nil nil nil nil nil nil nil nil nil nil
                 nil nil nil nil nil nil nil nil nil nil nil))

(let ((title-failures 0)
      (series-failures 0))
  (format t "~%Running tests...~%")
  (loop for title-test being the elements of *title-tests* do
    (let* ((grtitle (nth 0 title-test))
           (title (nth 1 title-test))
           (series (nth 2 title-test))
           (book (new-goodreads-test grtitle)))
      (if (not (equal title (goodreads-title book)))
          (progn
            (format t "~a does not match ~a~%" (goodreads-title book) title)
            (setf title-failures (1+ title-failures))))
      (if (not (equal series (goodreads-series book)))
          (progn
            (format t "~a does not match ~a~%" (goodreads-series book) series)
            (setf series-failures (1+ series-failures))))))
  (format t "~a failures.~%" title-failures)
  (format t "~a series failures.~%~%" series-failures))
