
reset:
	sudo -u postgres dropdb library_cl
	sudo -u postgres createdb --owner=brking library_cl
	cat sql/create.pgsql | psql library_cl
