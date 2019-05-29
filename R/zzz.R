#R


.onAttach <- function(lib, pkg){
	if(interactive()){
		version <- packageVersion('rawDiag')
		packageStartupMessage("Package 'rawDiag' version ", version)
	
		.cinit(dll=file.path(path.package(package = "rawDiag"),
		                     "exec", "fgcz_raw.dll"))
	  invisible()
	}
}
