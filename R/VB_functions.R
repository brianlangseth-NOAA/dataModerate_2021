#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param age vector of ages to predict length for (e.g., age = 1:60)
#' @param Linf maximum length parameter
#' @param L0 initial length parameter when growth changes 
#' @param k von Bertenlaffy growth parameter
#'
#' @return A vector of  
#'
#' @author Chantel Wetzel
#' @export
#'
vb_fn <- function(age, Linf, L0, k) {
    #vec <- Linf * (1 - exp( -k * (age - t0)))
    vec <- Linf - (Linf - L0) * exp(-age * k)
    return(vec)
}
#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param x vector of parameters c(Linf, L0, k)
#' @param age data vector of ages
#' @param length data vector of lengths by age
#'
#' @return A vector of  
#'
#' @author Chantel Wetzel
vb_opt_fn <- function(x, age, lengths) { 

	sum( (lengths - vb_fn( age, Linf = x[1], L0 = x[2], k = x[3]) )^2 )
}
