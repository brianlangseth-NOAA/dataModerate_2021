#' Compare length frequencies by data set
#'
#' @param dir
#' @param data data frame created by create_data_frame
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
length_by_depth_plot <- function(dir, data, xlim = NULL, ylim = NULL){

	dir.create(file.path(dir, "plots"), showWarnings = FALSE)

  sub_data = data[data$Source %in% c("Triennial", "NWFSC.Combo", "nwfsc_hkl", "NWFSC.Slope"), ]
	remove = NULL
	# Determine if all data sources have lengths 
	for (s in unique(data$Source)){
		check_len  <- check <- sum( !is.na( sub_data[sub_data$Source == s, "Length"])) == 0
		if (check_len) {remove <- c(remove, s)}
	}
	
	sub_data <- sub_data[!sub_data$Source %in% remove, ]

	sources = unique(sub_data$Source)
	panels = c(length(sources), 1)

	colvec <- c(rgb(1, 0, 0, alpha = 0.4), 
				      rgb(0, 0, 1, alpha = 0.4),
				      rgb(0, 0, 0, alpha = 0.25))

	if(is.null(xlim)) { 
		xlim = c(floor(min(sub_data[,"Depth"], na.rm = TRUE)), ceiling(max(sub_data[,"Depth"], na.rm = TRUE) ))
	}
  
  if(is.null(ylim)) { 
    ylim = c(floor(min(sub_data[,"Length"], na.rm = TRUE)), ceiling(max(sub_data[,"Length"], na.rm = TRUE) ))
  }
	
	pngfun(wd = file.path(dir, "plots"), file = "Length_by_Depth_by_Source.png", w = 7, h = 7, pt = 12)

	par(mfrow = panels)	

	# empty plot for legend	
  for (s in sources) 
  {
    plot(sub_data[sub_data$Source == s & sub_data$Sex == "F", "Depth"], 
         sub_data[sub_data$Source == s & sub_data$Sex == "F", "Length"],
         main = s, xlim = xlim, ylim = ylim, xlab = "Depth (m)", ylab = "Length (m)", 
         type = 'p', pch = 16, col = colvec[1])

    ind = 2
    for( g in c("M", "U")){
      if(dim(sub_data[sub_data$Source == s & sub_data$Sex == g, ])[1] > 0){
        points(sub_data[sub_data$Source == s & sub_data$Sex == g, "Depth"], 
               sub_data[sub_data$Source == s & sub_data$Sex == g, "Length"],
               pch = 16, col = colvec[ind])
        ind = ind + 1
      }
    } # sex
  } # source

	dev.off()
	
}