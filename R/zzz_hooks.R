# -----------------------------------------------------------------------------
# Hooks that specify behaviour when this package is loaded, attached, etc.
.onAttach <- function(libname, pkgname) {
  # libname, pkgname seem to be some standard stuff, like the zzz-Name
  packageStartupMessage(
    "wuelib is currently under development.\n",
    "The version you are using is: ", utils::packageVersion("wuelib"),", ",
    "last updated ", utils::packageDate("wuelib"),".\n",
    "Package is usable but the function interfaces may be subject to change.\n")
}
