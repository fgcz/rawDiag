#R

#' Title
#'
#' @param lib 
#' @param pkg 
#' @importFrom utils data packageVersion
.onAttach <- function(lib, pkg){
	if(interactive()){
		version <- packageVersion('rawDiag')
		packageStartupMessage("Package 'rawDiag' version ", version)
	  invisible()
	}
}
