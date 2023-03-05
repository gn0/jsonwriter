# 3/5/2023

document:
	Rscript -e "devtools::document()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install()"

deploy:
	Rscript -e "pkgdown::deploy_to_branch()"
