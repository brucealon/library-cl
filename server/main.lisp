(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "server/server.lisp" *project-dir*))

(hunchentoot:start *server*)

(hunchentoot:stop *server*)
(setf *server* nil)
