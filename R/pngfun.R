#' Function to simplify the creation of png files
#'
#' @param wd Directory to save files
#' @param file File name
#' @param w width of png figure
#' @param h height of png figure
#' @param pt Pixel size in figure 
#'
#' @return  
#'
#' @author Chantel Wetzel
#' @export
#'


pngfun <- function(wd, file,w=7,h=7,pt=12){
  file <- file.path(wd, file)
  cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}