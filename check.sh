Rscritp -e "library(roxygen2);roxygenize('ezsim',clean=TRUE)"
R CMD build ezsim
R CMD check ezsim_0.5.5.tar.gz