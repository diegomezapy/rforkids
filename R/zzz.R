#' @import tcltk
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Cargando 'rforkids': iniciando la app educativa...")
  launch_rforkids()
}
