# tools for active package development


vignettes:
	Rscript -e "devtools::build_vignettes()"

check:
	Rscript -e "devtools::check()"

document:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"
	Rscript -e "pkgdown::build_site()"

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

local-install:
	rm -rf .local
	mkdir .local
	R CMD Install --library=.local .

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
