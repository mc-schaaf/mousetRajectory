# -----------------------------------------------------------------------------
# Hooks that specify behaviour when this package is loaded, attached, etc.
.onAttach <- function(libname, pkgname) {
  # libname, pkgname seem to be some standard stuff, like the zzz-Name
  packageStartupMessage(
    "mousetRajectory is currently under development.\n",
    "The version you are using is: ", utils::packageVersion("mousetRajectory"),
    ", ", "last updated ", utils::packageDate("mousetRajectory"), ".\n",
    "Package is usable but the function interfaces may change.\n",
    "If you aim to make results replicable in the future, indicate the current",
    " version number in your script and use position-based argument ",
    "matching only with care."
  )
}
