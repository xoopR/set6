.onLoad = function(libname, pkgname) {
  options(set6.unicode = TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n-----------------------------")
  packageStartupMessage("\tset6 v",utils::packageVersion("set6"),
                        "\n\nGet started:\t?set6
Changelog:\tset6News()")
  packageStartupMessage("-----------------------------\n")
}

