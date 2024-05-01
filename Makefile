build:
	export R_LIBS_USER=~/R/x86_64-pc-linux-gnu-library/4.4:~/R/x86_64-pc-linux-gnu-library/suggested/; \ 
	R CMD build --compact-vignettes="gs+qpdf" hyperSpec/

novignette:
	R CMD build --no-build-vignettes hyperSpec/

checkLean: 
	export _R_CHECK_CRAN_INCOMING_=FALSE; \
	export _R_CHECK_DEPENDS_ONLY_=TRUE; \
	R --no-init-file CMD check --as-cran hyperSpec_0.100.2.tar.gz

checkFull: 
	export _R_CHECK_CRAN_INCOMING_=FALSE; \
	R --no-init-file CMD check --as-cran  hyperSpec_0.100.2.tar.gz

checkLeanDevel: 
	export _R_CHECK_CRAN_INCOMING_=FALSE; \
	export _R_CHECK_DEPENDS_ONLY_=TRUE; \
	~/R-devel/trunk/bin/R --no-init-file CMD check --as-cran hyperSpec_0.100.2.tar.gz

checkFullDevel: 
	export _R_CHECK_CRAN_INCOMING_=FALSE; \
	~/R-devel/trunk/bin/R --no-init-file CMD check --as-cran  hyperSpec_0.100.2.tar.gz