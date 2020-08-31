#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param age
#' @param Linf
#' @param k
#' @param t0
#'
#' @return A vector of  
#'
#' @author Chantel Wetzel
#' @export
#'

VB_fn <- function(age_vec, Linf, k, t0) {

    vec <- Linf * (1 - exp( -k * (age_vec - t0)))
    
    return(vec)
}

VBopt_fn <- function(x, age, lengths) { 

	sum( (data$Lengths - VB.fn( age, Linf = x[1], k = x[2], t0 = x[3]) )^2 )

}