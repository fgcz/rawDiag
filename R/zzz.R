#R


.onAttach <- function(lib, pkg){
	if(interactive()){
		version <- packageVersion('rawDiag')
		thermocopyright <- "RawFileReader reading tool. Copyright \u00A9 2016 by Thermo Fisher Scientific, Inc. All rights reserved."
		packageStartupMessage("Package 'rawDiag' version ", version , " using\n", thermocopyright)

		.isRawFileReaderLicenseAccepted()
	
	  invisible()
	}
}
