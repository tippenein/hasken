serve:
	stack exec hasken serve
setup_db:
	psql --username postgres --command 'CREATE DATABASE hasken_development'
	psql --username postgres --command 'CREATE DATABASE hasken_production'
	psql --username postgres --command 'CREATE DATABASE hasken_test'
