.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  S7::method(print, Rstripe) <- print_rstripe
  S7::method(fetch, Rstripe) <- fetch_rstripe
}
