
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the Irish Renal Registry R Package developed by Julio, David and Conor")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Conor Judge",
    devtools.desc.author = "Conor Judge <conorjudge@Gmail.com> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}



