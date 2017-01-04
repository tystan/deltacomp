install_load_pkg<-function(pkgname){
  if(!require(pkgname,character.only=TRUE)){
    install.packages(pkgname,dep=TRUE,repos="http://cran.ms.unimelb.edu.au/")
    if(!require(pkgname,character.only=TRUE))
      cat("######",pkgname,"\n###### unable to be loaded\n")
  }
}
install_load_pkg("compositions")
install_load_pkg("robCompositions")
install_load_pkg("lmtest")