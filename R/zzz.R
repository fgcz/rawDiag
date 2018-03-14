#R

.onAttach <- function(lib, pkg){
	if(interactive()){
		version <- packageVersion('rawDiag')
		packageStartupMessage("Package 'rawDiag' version ", version)
	  invisible()
	}
}
