#' Create histogram of length or age samples by area and by fleet
#'
#' @param dir directory of where save save plots
#' @param data data frame created by the create_data_frame function
#' @param data_type plot either the length or age data (default "Length")
#' @param group_column column name from data frame to group the data for plotting (e.g., State for
#' to see the data by state)
#' @param fleet_column column name from data frame to group for fleet structure
#' @param ymax vector of ymax values that should be equal in to unique group_column values. Default
#' input is NULL which sets the ymax to 0.15 for all group_column values.
#' @param do_abline
#'
#' @return Nothing - plots
#'
#' @author Chantel Wetzel
#' @export
#'
data_hist <- function(dir, data, data_type = "Length", group_column = "State_Areas", 
					  fleet_column = "Fleet", ymax = NULL, do_abline = TRUE){

areas  = sort(unique(data[!is.na(data[,data_type]), group_column]))
fleets = sort(unique(data[!is.na(data[,data_type]), fleet_column]))

colvec <- c(rgb(1, 0, 0, alpha = 0.8), rgb(0, 0, 1, alpha = 0.5), rgb(0, 0, 0, alpha = 0.30))
xlim = c(0, ceiling(max(data[,data_type], na.rm = TRUE)))

step <- 2
if(xlim[2] - xlim[1] > 80) { step <- 4 }  
bins <- seq(xlim[1], xlim[2], step)
if(max(bins) < xlim[2]) { bins <- c(bins, max(bins) + step) }

if (!is.null(ymax) & length(ymax) != length(areas)){
	stop("The ymax entries needs to equal the number of unique group_column requested" )
}
if(is.null(ymax)) {  
	ymax = rep(0.15, length(areas)) 
}


colm = which(colnames(data) == data_type)
ind = 1

for (aa in areas){

	find = which(data[, group_column] == aa)
	tmp  = data[find, ]

	num_unique = aggregate(tmp[,data_type] ~ tmp[,fleet_column], data = data[which(!is.na(data[,data_type])),],  FUN = function(x) length(x))

	f_by_a = sum(num_unique[,2] > 20)

	if(f_by_a == 1) { panels = c(2,1) }
	if(f_by_a %in% 2:3) { panels = c(2, 2) }
	if(f_by_a > 3) { panels = c(3, 2) }
	if(f_by_a > 5) { panels = c(3,3) }
	
	pngfun(wd = dir, file = paste0(data_type,"_", aa, ".png"), w = 7, h = 7, pt = 12)
	
	par(mfrow = panels, oma = c(1, 1, 2, 1))
	plot(0, type = 'n', xlim = xlim, xaxs = 'i', ylim = c(0, ymax[ind]), yaxs = 'i', ylab = "Proportion", 
		xlab = data_type, axes = FALSE, , main = "All Combined")
	grid(); axis(2, las = 1); axis(1)	
	hist(tmp[, data_type], breaks = bins, freq = FALSE, col = "grey", add = TRUE)
	if (do_abline){ abline(v = median(tmp[!is.na(tmp[, data_type]), data_type]), col = 1, lwd = 2, lty = 2) }
	
	for (f in fleets){
	
		f_column = which(colnames(tmp) == fleet_column)

		all <- tmp[tmp[,f_column] == f & !is.na(tmp[, colm]), data_type]
		fem <- tmp[tmp[,f_column] == f & tmp$Sex == "F" & !is.na(tmp[, colm]), data_type]
		mal <- tmp[tmp[,f_column] == f & tmp$Sex == "M" & !is.na(tmp[, colm]), data_type]
		uns <- tmp[tmp[,f_column] == f & tmp$Sex == "U" & !is.na(tmp[, colm]), data_type]
	
		if (length(all) > 20) {
			plot(0, type = 'n', xlim = xlim, xaxs = 'i', ylim = c(0, ymax[ind]), yaxs = 'i', ylab = "Proportion", 
				xlab = data_type, axes = FALSE, main = f)
			grid()
			mtext(data_type, side = 1, line = 2.5, outer = TRUE)
			axis(2, las = 1); axis(1)
			mtext(side = 3, aa, outer = TRUE)
		
			if (length(fem) > 10) {
				hist(fem, breaks = bins, freq = FALSE, col = colvec[1], add = TRUE)
				if (do_abline){ abline(v = median(fem), col = 'red', lwd = 2, lty = 1) }
			}
			if (length(mal) > 10) {
			  	hist(mal, breaks = bins, freq = FALSE, col = colvec[2], add = TRUE)
			  	if (do_abline){ abline(v = median(mal), col = 'blue', lwd = 2, lty = 2) }
			}
			if (length(uns) > 10){
				hist(uns, breaks = bins, freq = FALSE, col = colvec[3], add = TRUE)	
				if (do_abline){ abline(v = median(uns), col = 1, lwd = 2, lty = 3) }

			}
			legend('topleft', legend = NA, bty = 'n', title = paste0("N = ", length(all)), cex = 1.2)
		} # check all loop
	} # fleets loop

	ind = ind + 1
	dev.off()
} # area loop

}
