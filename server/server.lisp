
(ql:quickload :djula)
(ql:quickload :easy-routes)
(ql:quickload :hunchentoot)

(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "libs/library-database.lisp" *project-dir*))
(load (merge-pathnames "libs/library-models.lisp" *project-dir*))

(defparameter *server* nil)
(defparameter *server-port* 4242)

(djula:add-template-directory (merge-pathnames "html/" *project-dir*))

(defun log-info (&rest messages)
  (print (apply 'concatenate `(string ,@messages))))

(defun session-username ()
  (and
   (boundp 'hunchentoot:*session*)
   (hunchentoot:session-value 'username)))


(defun owner-id ()
  (let ((username (session-username)))
    (if username
        (user-id (with-library-db (get-user username)))
        -1)))

(defun logged-in-p (&optional (page "unknown"))
  (let ((username (session-username)))
    (log-info "Have session username '" username "' for page " page)
    username))

(defmacro render-template (file &rest rest)
  `(progn
     (log-info "Rendering template for " ,file)
     (djula:render-template* (djula:compile-template* ,file)
                             nil
                             ,@rest)))

(defmacro render-logged-in-template (file &rest rest)
  `(if (logged-in-p ,file)
       (render-template ,file ,@rest)
       (render-template "login.html")))

(defmacro render-logged-in-db-template (file &rest rest)
  `(if (logged-in-p ,file)
       (with-library-db
         (render-template ,file ,@rest))
       (render-template "login.html")))

(easy-routes:defroute route-login ("/login" :method :post) (username password)
  (log-info "Logging in as '" username "'")
  (cond
    ((valid-user-p username password)
     (hunchentoot:start-session)
     (setf (hunchentoot:session-value 'username) username)
     (hunchentoot:redirect "/"))
    (t
     (render-template "login.html" :username username :error t))))

(easy-routes:defroute route-logout ("/logout") ()
  (if (logged-in-p)
      (progn
        (log-info "Logging out user " (session-username))
        (hunchentoot:remove-session hunchentoot:*session*)))
  (hunchentoot:redirect "/"))

(easy-routes:defroute root ("/") ()
  (render-logged-in-template "library.html"))

(easy-routes:defroute route-publications ("/publications") ()
  (render-logged-in-db-template
   "publications.html"
   :publications (all-publications (owner-id))))

(easy-routes:defroute route-publication ("/publications/:publication") ()
  (render-logged-in-db-template
   "publication.html"
   :publication (get-publication-by-id publication (owner-id))))

(easy-routes:defroute route-creators ("/creators") ()
  (render-logged-in-db-template
   "creators.html"
   :creators (all-creators (owner-id))))

(easy-routes:defroute route-creator ("/creators/:creator") ()
  (render-logged-in-db-template
   "creator.html"
   :creator (get-creator-by-id creator (owner-id))))

(if (not (null *server*))
    (format nil "Need to stop the current server first?")
    (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port *server-port*)))
