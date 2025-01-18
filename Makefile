
reset:
	sudo -u postgres dropdb library_cl
	sudo -u postgres createdb --owner=brking library_cl
	cat sql/create.sql | psql library_cl
