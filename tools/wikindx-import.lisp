
(ql:quickload "cl-mysql")
(load (merge-pathnames ".library-config.lisp" (user-homedir-pathname)))
(load (merge-pathnames "tools/wikindx-models.lisp" *project-dir*))
(load (merge-pathnames "libs/library-database.lisp" *project-dir*))

(defparameter *wikindx-query*
  "select r.resourceId as id,
          m.resourcemiscCollection as collection,
          r.resourceType as resource_type,
          r.resourceTitle as title,
          r.resourceSubtitle as subtitle,
          r.resourceField1 as seriestitle,
          r.resourceField2 as edition,
          r.resourceField4 as volumenumber,
          r.resourceField6 as pages,
          r.resourceIsbn as isbn,
          coalesce(t.resourcetextUrls, '') as url,
          m.resourcemiscField4 as volumes,
          c.creatorFirstname as name_first,
          c.creatorInitials as name_middle,
          c.creatorSurname as name_last,
          rc.resourcecreatorRole as role,
          rq.resourcequotePageStart as quote_start,
          rq.resourcequotePageEnd as quote_end,
          rqt.resourcequotetextText as quote,
          rqc.resourcequotecommentComment as quote_comment
     from WKX_resource r
       inner join WKX_resource_misc m on r.resourceId = m.resourcemiscId
       left outer join WKX_resource_creator rc on rc.resourcecreatorResourceId = r.resourceId
       left outer join WKX_creator c on rc.resourcecreatorCreatorId = c.creatorId
       left outer join WKX_resource_text t on r.resourceID = t.resourcetextId
       left outer join WKX_resource_quote rq on rq.resourcequoteResourceId = r.resourceId
       left outer join WKX_resource_quote_comment rqc on rqc.resourcequotecommentQuoteId = rq.resourcequoteId
       left outer join WKX_resource_quote_text rqt on rqt.resourcequotetextId = rq.resourcequoteId")

(defun read-wikindx-db ()
  (cl-mysql:connect :host *mysql-host*
                    :database *mysql-db*
                    :user *mysql-user*
                    :password *mysql-password*)
  (cl-mysql:query "set names 'utf8'")
  (let* ((result-set (cl-mysql:query *wikindx-query*))
         (books (first (first result-set))))
    (mapcar (lambda (row) (funcall 'new-wikindx row)) books)))
