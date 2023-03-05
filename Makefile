# 3/5/2023

document:
	Rscript -e "devtools::document()"

check: check-devtools check-as-cran

check-devtools:
	Rscript -e "devtools::check()"

check-as-cran:
	R CMD check --as-cran .

build:
	R CMD build .

install:
	Rscript -e "devtools::install()"

deploy:
	Rscript -e "pkgdown::deploy_to_branch()"
