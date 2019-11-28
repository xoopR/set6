.onLoad = function(libname, pkgname) {
  options(set6.unicode = TRUE)
}

.onUnload = function(libname, pkgname) {
  options(set6.unicode = NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n-----------------------------")
  packageStartupMessage("\tset6 v",utils::packageVersion("set6"),
                        "\n\nGet started:\t?set6
Changelog:\tset6News()")
  packageStartupMessage("-----------------------------\n")
}

