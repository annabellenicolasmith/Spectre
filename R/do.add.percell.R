#' do.add.percell
#'
#' @author Thomas M Ashhurst, \email{thomas.ashhurst@@sydney.edu.au}
#'
#' @references \url{https://github.com/ImmuneDynamics/Spectre}.
#'
#' @import data.table
#' 
#' @usage do.add.percell(spatial.dat, percell.dat, roi.col, name = "per.cell")
#'
#' @export do.add.percell

do.add.percell <- function(spatial.dat,
                           percell.dat,
                           roi.col,
                           name = "per.cell"){
  
  ### Setup
  message("This is a developmental Spectre-spatial function that is still in testing phase with limited documentation. We recommend only using this function if you know what you are doing.")
  
  ### Loop
  
  for(i in names(spatial.dat)){
    # i <- "20171228_spleen315_500x500_editedforFAS_s1_p9_r2_a2_ac"
    temp <- percell.dat[percell.dat[[roi.col]] == i,]
    spatial.dat[[i]]$CPDATA[[name]] <- temp
  }
  
  return(spatial.dat)
  
}
