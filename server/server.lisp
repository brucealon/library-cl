
(ql:quickload :babel)
(ql:quickload :djula)
(ql:quickload :easy-routes)
(ql:quickload :hunchentoot)
(ql:quickload :ironclad)

(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "libs/library-database.lisp" *project-dir*))
(load (merge-pathnames "libs/library-models.lisp" *project-dir*))

(defparameter *server* nil)
(defparameter *server-port* 4242)

(defun log-info (&rest messages)
  (print (apply 'concatenate `(string ,@messages))))

(defun session-username ()
  (and
   (boundp 'hunchentoot:*session*)
   (hunchentoot:session-value 'username)))

(defun hash-password (password)
  (ironclad:pbkdf2-hash-password-to-combined-string (babel:string-to-octets password)))

(defun valid-user-p (username password)
  (let ((user (with-library-db (get-user username))))
    (and user
         (user-hashed-password user)
         (not (eq :null (user-hashed-password user)))
         (string= username (user-username user))
         (ironclad:pbkdf2-check-password (babel:string-to-octets password) (user-hashed-password user)))))

(defun logged-in-p ()
  (log-info "Checking session username: '" (session-username) "'")
  (session-username))

(djula:add-template-directory (merge-pathnames "html/" *project-dir*))

(easy-routes:defroute root ("/") ()
  (if (logged-in-p)
      (djula:render-template* (djula:compile-template* "library.html"))
      (djula:render-template* (djula:compile-template* "login.html"))))

(easy-routes:defroute route-login ("/login" :method :post) (username password)
  (log-info "Logging in as '" username "'")
  (cond
    ((valid-user-p username password)
     (hunchentoot:start-session)
     (setf (hunchentoot:session-value 'username) username)
     (hunchentoot:redirect "/"))
    (t
     (djula:render-template* (djula:compile-template* "login.html")
                             nil
                             :username username :error t))))

(easy-routes:defroute route-logout ("/logout") ()
  (if (logged-in-p)
      (progn
        (log-info "Logging out user " (session-username))
        (hunchentoot:remove-session hunchentoot:*session*)))
  (hunchentoot:redirect "/"))

(easy-routes:defroute route-publications ("/publications") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "publications.html")
                            nil
                            :publications (all-publications))))

(easy-routes:defroute route-publication ("/publications/:publication") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "publication.html")
                            nil
                            :publication (first (publication-by-id :id publication)))))

(easy-routes:defroute route-creators ("/creators") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "creators.html")
                            nil
                            :creators (all-creators))))

(easy-routes:defroute route-creator ("/creators/:creator") ()
  (with-library-db
    (djula:render-template* (djula:compile-template* "creator.html")
                            nil
                            :creator (first (creator-by-id :id creator)))))

(if (not (null *server*))
    (format nil "Need to stop the current server first?")
    (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port *server-port*)))
