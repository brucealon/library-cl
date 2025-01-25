
(ql:quickload "cl-mysql")
(load "/home/brking/.library-config.lisp")

(cl-mysql:connect :host *mysql-host*
                  :database *mysql-db*
                  :user *mysql-user*
                  :password *mysql-password*)

(let* ((result-set (cl-mysql:query "select r.resourceType as resource_type,
                                           r.resourceTitle as title,
                                           r.resourceSubtitle as subtitle,
                                           r.resourceShortTitle as shorttitle,
                                           r.resourceField1 as seriestitle,
                                           r.resourceField2 as edition,
                                           r.resourceField4 as volumenumber,
                                           r.resourceField6 as pages,
                                           r.resourceIsbn as isbn,
                                           c.creatorFirstname as author_first,
                                           c.creatorInitials as author_middle,
                                           c.creatorSurname as author_last,
                                           coalesce(t.resourcetextUrls, '') as url,
                                           m.resourcemiscField2 as pub_day,
                                           m.resourcemiscField3 as pub_month,
                                           m.resourcemiscField4 as volumes
                                      from WKX_resource r
                                        inner join WKX_resource_misc m on r.resourceId = m.resourcemiscId
                                        left outer join WKX_resource_creator rc on r.resourceId = rc.resourcecreatorResourceId
                                        left outer join WKX_creator c on rc.resourcecreatorCreatorId = c.creatorId
                                        left outer join WKX_resource_text t on r.resourceID = t.resourcetextId"))
       (books   (first  (first result-set)))
       (headers (second (first result-set))))
  headers)
