#' @title Show set6 NEWS.md File
#' @description Displays the contents of the NEWS.md file for viewing set6
#' release information.
#' @return NEWS.md in viewer.
#' @examples
#' set6News()
#' @export
set6News <- function (){
  file.show(file.path(system.file(package = "set6"), "NEWS.md"),
            title = "set6 Changelog")
}
