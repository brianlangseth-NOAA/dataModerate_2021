#' Compare length frequencies by data set
#'
#' @param dir the directory to save the figures
#' @param data data frame created by create_data_frame
#' @param xlim custom xlim input, otherwise dynamically determined
#' @param ylim custom ylim input, otherwise dynamically determined
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
length_freq_plot <- function(dir, data, xlim = NULL, ylim = NULL){

	dir.create(file.path(dir, "plots"), showWarnings = FALSE)

	remove = NULL
	# Determine if all data sources have lengths 
	for (s in unique(data$Source)){
		check_len  <- check <- sum( !is.na( data[data$Source == s, "Length"])) == 0
		if (check_len) {remove <- c(remove, s)}
	}
	
	sub_data <- data[!data$Source %in% remove, ]

	sources = unique(sub_data$Source)
	n = length(sources)
	if (n == 1) { panels = c(2, 1)}
	if (n == 2) { panels = c(3, 1)}
	if (n == 3) { panels = c(2, 2)}
	if (n == 4) { panels = c(3, 2)}
	if (n == 5) { panels = c(3, 2)}
	if (n == 6) { panels = c(3, 2)}
	

	colvec <- c(rgb(1, 0, 0, alpha = 0.8), 
				      rgb(0, 0, 1, alpha = 0.5),
				      rgb(0, 0, 0, alpha = 0.25))


	if(is.null(xlim)) { 
		xlim = c(min(sub_data[,"Length"], na.rm = TRUE), ceiling(max(sub_data[,"Length"], na.rm = TRUE) ))
	}
  if(is.null(ylim)) { ylim = c(0, 0.25) }

  step <- 2
  if(xlim[2] - xlim[1] > 80) { step <- 4 }  

  bins <- seq(xlim[1], xlim[2], step)
  if(max(bins) < xlim[2]) { bins <- c(bins, max(bins) + step) }
	
	pngfun(wd = file.path(dir, "plots"), file = "Length_by_Source.png", w = 7, h = 7, pt = 12)

	par(mfrow = panels)	

	# empty plot for legend
  	plot(0, type = 'n', ylab = "", xlab = "", axes = FALSE)
  	legend('left',
  	       bty = 'n',
  	       fill = colvec,
  	       cex = 1.5,
  	       legend = c("Females", "Males", "Unsexed"))
	
  	mtext("Length (cm)", side = 1, line = 2.5, outer = TRUE)
	
  	for (y in sources) 
  	{
  	  # make empty plot (note: xlim and ylim were set by trial and error)
  	  plot(0, type = 'n',
  	       xlim = xlim,
  	       xaxs = 'i',
  	       ylim = ylim,
  	       ylab = "",
  	       xlab = "Length (cm)",
  	       yaxs = 'i',
  	       axes = FALSE)
  	  grid()	
  	  
  	  axis(2, las = 1)
	    axis(1)
	
  	  male_len <- sub_data$Length[sub_data$Source == y & sub_data$Sex == "M"]
  	  fem_len  <- sub_data$Length[sub_data$Source == y & sub_data$Sex == "F"]
  	  unsex_len  <- sub_data$Length[sub_data$Source == y & sub_data$Sex == "U"]
	
	  if (length(fem_len) > 0) {
  	  hist(fem_len,
  	       breaks = bins,
  	       freq = FALSE,
  	       col = colvec[1],
  	       add = TRUE)
  	  }
  	  if (length(male_len) > 0) {
  	    hist(male_len,
  	         breaks = bins,
  	         freq = FALSE,
  	         col = colvec[2],
  	         add = TRUE)
  	  }
  	  if (length(unsex_len) > 0) {
  	    hist(unsex_len,
  	         breaks = bins,
  	         freq = FALSE,
  	         col = colvec[3],
  	         add = TRUE)
  	  }
  	  legend('topleft', legend = NA, bty = 'n', title = y, cex = 1.5)
  	  legend('right', legend = NA, bty = 'n',
  	         title = paste0("N Females  = ", length(fem_len),
  	                        "\nN Males = ", length(male_len),
  	                        "\nN Unsexed = ", length(unsex_len) ),
  	         cex = 1.0)
  	} # source

	dev.off()
	
}