#R


.promega6x5mix.sanity.check <- function(x, iontol = 0.01){
    stopifnot(!is.null(x))
    stopifnot(nchar(x$sequence) == length(x$mod))
    stopifnot(abs((((parentIonMass(x$sequence) + sum(x$mod) + 2 * 1.00794) / 2) - x$mp2h2p) ) < 0.0007)
}


extractPromegaXIC <- function(rawfile, promega = getPromega6x5mix(), tol = 10){
    mZ <- sapply(promega, function(x){ (parentIonMass(x$sequence) + sum(x$mod) + 2 * 1.00794) / 2 })
    abundance <- sapply(promega, function(x){x$abundance})
    sequence <- sapply(promega, function(x){x$sequence})
    
    XIC <- readXICs(rawfile, mZ, tol = 10)
    XIC.log.max <- sapply(XIC, function(x){log(max(x$intensities))})
    
    data.frame(rawfile=basename(rawfile), sequence, mZ, abundance, XIC.log.max)
}



#' Get in-silico promega's 6 x 5 lc ms ms peptide reference mix
#'
#' @details contains the data contained in Table 5. 
#' 'Masses of Various Peptides. Underlined letters indicate peptides labeled
#' with stable, heavy isotopes'. 
#' @return a nested list containing the in-silico peptide information 
#' @export getPromega6x5mix
#' @export extractPromegaXIC
#' @aliases extractPromegaXIC
#' @author Tobias Kockmann and Christian Panse, 2018
#' @importFrom protViz parentIonMass
#' @references 
#' \href{https://www.promega.com/-/media/files/resources/protocols/technical-manuals/101/6-x-5-lc-ms-ms-peptide-reference-mix-protocol.pdf}{6-x-5-lc-ms-ms-peptide-reference-mix-protocol}
#' @examples 
#' promega <- getPromega6x5mix()
#' \dontrun{
#' start <- Sys.time()
#' filepathroot <- '/Users/cp/Downloads/'
#' rawfile <- file.path(filepathroot, "20181012_002_S186065_6x5LC-MS2_A_Method_1.raw")
#' 
#' mZ <- sapply(promega, function(x){ (parentIonMass(x$sequence) + sum(x$mod) + 2 * 1.00794) / 2 })
#' abundance <- sapply(promega, function(x){x$abundance})
#' sequence <- sapply(promega, function(x){x$sequence})
#' 
#' XIC <- readXICs(rawfile, mZ, tol = 10)
#' XIC.log.max <- sapply(XIC, function(x){log(max(x$intensities))})
#' 
#' library(lattice)
#' xyplot(XIC.log.max ~ log(abundance) | sequence,
#'        panel = function(x, y, ...){panel.xyplot(x,y,... ); 
#'            fm <- lm(y ~ x); panel.lines(x, fitted(fm))}, pch=16, main=basename(rawfile),
#'        sub=list(paste("analyse the promega 6-x-5-lc-ms-ms-peptide-reference-mix-protocol
#'                       with rawDiag in less than 10 lines of R code in less than", 
#'                       round(Sys.time() - start) +1, "seconds."), cex=1))
#' }
getPromega6x5mix <- function(){
    L <- list()
    
    L[[1]] <- list(sequence = 'VTSGSTSTSR',
                   mod = c(6.013809, 5.010161, 4.007678, 0.0, 0.0, 5.010161, 0.0, 5.010161, 0.0, 8.999969),
                   mp2h2p = 509.2739,
                   mass = 1016.533,
                   abundance = 1
    )
    
    L[[2]] <- list(sequence = 'VTSGSTSTSR',
                   mod = c(6.013809, 5.010161, 0.0, 0.0, 0.0, 0.0, 0.0, 5.010161, 0.0, 8.999969),
                   mp2h2p = 504.7651,
                   mass = 1007.515,
                   abundance = 1e-01
    )
    # 2 * 502.2599 - 2 * 1.00794 - ( parentIonMass("VTSGSTSTSR") +  sum(c(6.013809, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 8.999969)))
    # T 5.010161
    L[[3]] <- list(sequence = 'VTSGSTSTSR',
                   mod = c(6.013809, 5.010161, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 8.999969),
                   mp2h2p = 502.2599,
                   mass = 1002.505,
                   abundance = 1e-02
    )
    
    
    L[[4]] <- list(sequence = 'VTSGSTSTSR',
                   mod = c(6.013809, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 8.999969),
                   mp2h2p = 499.7547,
                   mass = 997.494,
                   abundance = 1e-03
    )
    
   
    L[[5]] <- list(sequence = 'VTSGSTSTSR',
                    mod = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 8.999969),
                    mp2h2p = 496.7478,
                    mass = 991.480,
                    abundance = 1e-04
    )
    
    # 2 * 428.2738 - 2 * 1.00794 - ( parentIonMass("LASVSVSR") +  sum(c(7.016793,0,0,6.013809,0,6.013809,0,8.999969)))
    L[[6]] <- list(sequence = 'LASVSVSR',
                   mod = c(7.016793, 8.014309, 0.0,  6.013809, 0.0, 6.013809, 0.0, 8.999969),
                   mp2h2p = 428.2738,
                   abundance = 1.0
    )
    
    L[[7]] <- list(sequence = 'LASVSVSR',
                   mod = c(7.016793,0,0,6.013809,0,6.013809,0,8.999969),
                   mp2h2p =424.2667,
                   abundance = 1e-01
    )
    
    
    L[[8]] <- list(sequence = 'LASVSVSR',
                   mod = c(0,0,0,6.013809,0,6.013809,0,8.999969),
                   mp2h2p = 420.7581,
                   abundance = 1e-02
    )
    
    L[[9]] <- list(sequence = 'LASVSVSR',
                   mod = c(0,0,0,0,0,6.013809,0,8.999969),
                   mp2h2p = 417.7512,
                   abundance =1e-03
    )
    # K 7.005559
    # A 6.013809
    # V 6.013809
    # L 7.016793
    # R 8.999969
    L[[10]] <- list(sequence = 'LASVSVSR',
                   mod = c(0,0,0,0,0,0,0,8.999969),
                   mp2h2p = 414.7443,
                   abundance = 1e-04
    )
    L[[11]] <- list(sequence = 'YVYVADVAAK',
                    mod = c(0, 6.013809, 0, 0, 6.013809, 0, 6.013809, 4.007,4.007, 7.005559),
                    mp2h2p = 566.8300,
                    abundance = 1.0
    )
    L[[12]] <- list(sequence = 'YVYVADVAAK',
                    mod = c(0, 6.013809, 0, 0, 6.013809, 0, 6.013809, 0,0, 7.005559),
                    mp2h2p = 562.8229,
                    abundance = 0.1
    )
    L[[13]] <- list(sequence = 'YVYVADVAAK',
                    mod = c(0, 0, 0, 0, 6.013809, 0, 6.013809, 0,0, 7.005559),
                    mp2h2p = 559.8160,
                    abundance = 0.01
    )
    L[[14]] <- list(sequence = 'YVYVADVAAK',
                    mod = c(0, 0, 0, 0, 0, 0, 6.013809, 0,0, 7.005559),
                    mp2h2p = 556.8091,
                    abundance = 0.001
    )
   
    # replace C12 with C13
    L[[15]] <- list(sequence = 'YVYVADVAAK',
                    mod = c(0, 0, 0, 0, 0, 0, 0, 0,0, 7.005559),
                    mp2h2p = 553.8022,
                    abundance = 1e-04
    )
    
    # 2 * 459.8232 - 2 * 1.00794 - ( parentIonMass("VVGGLVALR") +  sum(c(6.013809, 6.013809, 0, 0,  6.013809, 0, 0,0, 8.999969)))
    L[[16]] <- list(sequence = 'VVGGLVALR',
                    mod = c(6.013809, 6.013809, 0, 7.016793,  6.013809, 0, 0,0, 8.999969),
                    mp2h2p = 459.8232,
                    abundance = 1
    )
   
    L[[17]] <- list(sequence = 'VVGGLVALR',
                    mod =  c(6.013809, 6.013809, 0, 0,  6.013809, 0, 0,0, 8.999969),
                    mp2h2p = 456.3147,
                    abundance = 0.1
    )
   
    L[[18]] <- list(sequence = 'VVGGLVALR',
                    mod = c(6.013809, 6.013809, 0, 0, 0, 0, 0,0, 8.999969),
                    mp2h2p = 453.3078,
                    abundance = 0.01
    )
    
    L[[19]] <- list(sequence = 'VVGGLVALR',
                    mod = c(6.013809, 0, 0, 0, 0, 0, 0,0, 8.999969),
                    mp2h2p = 450.3009,
                    abundance = 1e-03
    )
    # 2 * 447.2940 - 2 * 1.00794 - parentIonMass("VVGGLVALR")
    L[[20]] <- list(sequence = 'VVGGLVALR',
                    mod = c(0, 0, 0, 0, 0, 0, 0,0, 8.999789),
                    mp2h2p = 447.2940,
                    abundance = 1e-04
    )
    # K 7.005559
    # A 6.013809
    # V 6.013809
    # L 7.016793
    # R 8.999969
    # F 10.02834
    # 2 * 537.3441 - 2 * 1.00794 - ( parentIonMass("LLSLGAGEFK") + sum (c( 7.016793, 7.016793, 0.0, 7.016793, 0.0, 0.0, 0.0, 0.0, 0.0, 7.005559)))
    L[[21]] <- list(sequence = 'LLSLGAGEFK',
                    mod = c( 7.016793, 7.016793, 0.0, 7.016793, 0.0, 0.0, 0.0, 0.0, 10.02834, 7.005559),
                    mp2h2p = 537.3441,
                    mass = 1072.673,
                    abundance = 1.0
    )
    
    L[[22]] <- list(sequence = 'LLSLGAGEFK',
                    mod = c( 7.016793, 7.016793, 0.0, 7.016793, 0.0, 0.0, 0.0, 0.0, 0.0, 7.005559),
                    mp2h2p = 532.3305,
                    mass = 1062.646,
                    abundance = 1e-01
    )
    L[[23]] <- list(sequence = 'LLSLGAGEFK',
                    mod = c( 7.016793, 7.016793, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.005559),
                    mp2h2p = 528.8219,
                    mass = 1055.629,
                    abundance = 1e-02
    )
    L[[24]] <- list(sequence = 'LLSLGAGEFK',
                    mod = c( 7.016793,0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.005559),
                    mp2h2p = 525.3134,
                    mass = 1048.612,
                    abundance = 1e-03
    )
    L[[25]] <- list(sequence = 'LLSLGAGEFK',
                    mod = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.005559),
                    mp2h2p = 521.8048,
                    mass = 1041.594,
                    abundance = 1e-04
    )
    
    L[[26]] <- list(sequence = 'LGFTDLFSK',
                    mod = c(7.016793, 0.0,10.02834, 0.0, 0.0, 7.016793, 10.02834, 0.0, 7.005559),
                    mp2h2p = 535.3281,
                    mass = 1068.641,
                    abundance = 1.0
    )
    
    L[[27]] <- list(sequence = 'LGFTDLFSK',
                    mod = c(7.016793, 0.0, 0.0, 0.0, 0.0, 7.016793, 10.02834, 0.0, 7.005559),
                    mp2h2p = 530.3145,
                    mass = 1058.614,
                    abundance = 1e-01
    )
    
    
    L[[28]] <- list(sequence = 'LGFTDLFSK',
                    mod = c(7.016793, 0.0, 0.0, 0.0, 0.0, 7.016793, 0.0, 0.0, 7.005559),
                    mp2h2p = 525.3008,
                    mass = 1048.587,
                    abundance = 1e-02
    )
    
    L[[29]] <- list(sequence = 'LGFTDLFSK',
                    mod = c(7.016793, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.005559),
                    mp2h2p = 521.7923,
                    mass = 1041.569,
                    abundance = 1e-03
    )
    
    L[[30]] <- list(sequence = 'LGFTDLFSK',
                    mod = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.005559),
                    mp2h2p = 518.2837,
                    mass = 1034.552,
                    abundance = 1e-04
    )
    rv <- lapply(L, .promega6x5mix.sanity.check)
    L
}

