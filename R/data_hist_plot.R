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
#'
#' @return Nothing - plots
#'
#' @author Chantel Wetzel
#' @export
#'
data_hist <- function(dir, data, data_type = "Length", group_column = "State_Areas", 
					  fleet_column = "Fleet", ymax = NULL){

areas  = sort(unique(data[, group_column]))
fleets = sort(unique(data[, fleet_column]))

if (!is.null(ymax) & length(ymax) != length(areas)){
	stop("The ymax entries needs to equal the number of unique group_column requested" )
}
if(is.null(ymax)) {  
	ymax = rep(0.15, length(areas)) 
}


colm = which(colnames(tmp) == data_type)
ind = 1

for (aa in areas){

	find = which(data[, group_column] == aa)
	tmp  = data[find, ]
	f_by_a = length(unique(tmp[,fleet_column]))

	if(f_by_a == 1) { panels = c(2,1) }
	if(f_by_a %in% 2:3) { panels = c(2, 2) }
	if(f_by_a > 3) { panels = c(3, 2) }
	if(f_by_a > 5) { panels = c(3,3) }

	find = which(data[, group_column] == aa)
	tmp  = data[find, ]
	
	#pngfun(wd = dir, file = paste0(data_type,"_", aa, ".png", w = 7, h = 7, pt = 12)
	
	par(mfrow = panels, oma = c(1, 1, 2, 1))
	plot(0, type = 'n', xlim = xlim, xaxs = 'i', ylim = c(0, ymax[ind]), yaxs = 'i', ylab = "Proportion", 
		axes = FALSE, , main = "All Combined")
	grid(); axis(2, las = 1); axis(1)	
	hist(tmp[!is.na(tmp[, colm]), data_type], breaks = bins, freq = FALSE, col = colvec[3], add = TRUE)
	
	for (f in fleets){
	
		f_colum = which(colnames(tmp) == fleet_column)

		all <- tmp[tmp[,f_colm] == f & !is.na(tmp[, colm]), data_type]
		fem <- tmp[tmp[,f_colm] == f & tmp$Sex == "F" & !is.na(tmp[, colm]), data_type]
		mal <- tmp[tmp[,f_colm] == f & tmp$Sex == "M" & !is.na(tmp[, colm]), data_type]
		uns <- tmp[tmp[,f_colm] == f & tmp$Sex == "U" & !is.na(tmp[, colm]), data_type]
	
		if (length(all) != 0) {
			plot(0, type = 'n', xlim = xlim, xaxs = 'i', ylim = c(0, ymax[ind]), yaxs = 'i', ylab = "Proportion", 
				axes = FALSE, main = f)
			grid()
			mtext(data_type, side = 1, line = 2.5, outer = TRUE)
			axis(2, las = 1); axis(1)
			mtext(side = 3, aa, outer = TRUE)
		
			if (length(fem) > 1) {
				hist(fem, breaks = bins, freq = FALSE, col = colvec[1], add = TRUE)
			}
			if (length(mal) > 1) {
			  hist(mal, breaks = bins, freq = FALSE, col = colvec[2], add = TRUE)
			}
			if (length(uns) > 1){
				hist(uns, breaks = bins, freq = FALSE, col = colvec[3], add = TRUE)	
			}
			legend('topleft', legend = NA, bty = 'n', title = paste0("N = ", length(all)), cex = 1.2)
		} # check all loop
	} # fleets loop
	ind = ind + 1
	dev.off()
} # area loop

}
