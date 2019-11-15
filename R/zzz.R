.onLoad = function(libname, pkgname) {
  options(set6.unicode = TRUE)
}

.onUnload = function(libname, pkgname) {
  options(set6.unicode = NULL)
}
