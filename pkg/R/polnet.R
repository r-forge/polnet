
# display version number and date when the package is loaded
.onAttach <- function(libname, pkgname) {
  desc  <- packageDescription(pkgname, libname)
  packageStartupMessage('Version:  ', desc$Version, '\n', 'Date:     ', 
      desc$Date)
}

