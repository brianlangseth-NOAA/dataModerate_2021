#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param dir directory of where save save plots
#' @param data data frame created by the create_data_frame function
#' @param splits not implemented
#' @param nm_append text to append to the plot name
#' @param ests list of estimates created by the estimate_length_weight function
#'
#' @return Nothing - plots
#'
#' @author Chantel Wetzel
#' @export
#'
length_age_plot <- function(dir, data, splits = NA, nm_append = NULL, ests = NULL){

dir.create(file.path(dir, "plots"), showWarnings = FALSE)

keep = which(!is.na(data$Age))
data = data[keep, ]

sources = unique(data$Source)
n = length(sources)
if (n == 1) { panels = c(1, 1)}
if (n == 2) { panels = c(3, 1)}
if (n == 3) { panels = c(2, 2)}
if (n == 4) { panels = c(3, 2)}


colors = rich.colors.short(n + 1, alpha = 0.4)

lens = 1:max(data$Length, na.rm = TRUE)

xmax = max(data$Age,    na.rm = TRUE)
ymax = max(data$Length, na.rm = TRUE)

file = ifelse(is.null(nm_append), "Length_Age_by_Source.png",
			  paste0("Length_Age_by_Source_", nm_append, ".png"))

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = panels)	
	get = 1
	plot(data$Age, data$Length, xlab = "Age", ylab = "Length (cm)", main = "All Data", 
		  xaxs = "i", yaxs = "i", ylim = c(0, ymax), xlim = c(0, xmax), pch = 1, col = colors[1])

	if (!is.null(ests) & "all" %in% names(ests)) {
		lines(0:ymax, vb_fn(age = 0:ymax, Linf = ests$all[1], L0 = ests$all[2], k = ests$all[3]), col = 1, lwd = 2)
		legend("bottomright", bty = 'n', 
				legend = paste0("Combined: Linf = ", round(ests$all[1], 2), 
										  " L0 = " , round(ests$all[2], 2),
										  " k = "  , round(ests$all[3], 3)), 
				lty = 1, col = 1, lwd = 2)
	}
	

	for (a in sources){
		ind = data$Source == a
		plot(data$Age[ind], data$Length[ind], xlab = "Age", ylab = "Length (cm)", main = a,
			  xaxs = "i", yaxs = "i", ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = colors[get])

		if (!is.null(ests) & a  %in% names(ests)) {
			lines(0:ymax, vb_fn(age = 0:ymax, Linf = ests[a][[1]][1], L0 = ests[a][[1]][2], k = ests[a][[1]][3]), col = 1, lwd = 2)
			legend("bottomright", bty = 'n', 
				   	legend = paste0("Combined: Linf = ", round(ests[a][[1]][1], 2), 
							  				  " L1 = " , round(ests[a][[1]][2], 2),
							  				  " k = "  , round(ests[a][[1]][3], 3)), 
				    lty = 1, col = 1, lwd = 2)}	
	}
		
dev.off()

file = ifelse(is.null(nm_append), "Length_Age_by_Sex.png",
			  paste0("Length_Age_by_Sex_", nm_append, ".png"))

line_col = c("red", 'blue', "grey")
sex_col = alpha(line_col, 0.15)

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = c(panels[1], panels[2]) )
		
	ind = 0; leg = NULL
	for(s in unique(data$Sex)) {
		ind = ind + 1	
		if (ind == 1) {
			plot(data[data$Sex == s, "Age"], data[data$Sex == s, "Length"], 
			     xlab = "Age", ylab = "Length (cm)", main = "All Sources", xaxs = "i", yaxs = "i",
			     ylim = c(0, ymax), xlim = c(0, xmax), pch = 1, col = sex_col[ind]) 
		}
		if (ind > 1){
			points(data[data$Sex == s, "Age"], data[data$Sex == s, "Length"], pch = 1, col = sex_col[ind])
		}	
		
		if (!is.null(ests) & paste0("all_", s)  %in% names(ests)) {
			lines(0:ymax, vb_fn(age = 0:ymax, Linf = ests[paste0("all_", s)][[1]][1], L0 = ests[paste0("all_", s)][[1]][2], k = ests[paste0("all_", s)][[1]][3]), 
				col = line_col[ind], lty = ind, lwd = 2)
			leg = c(leg, paste0(s, ": Linf = ", round(ests[paste0("all_", s)][[1]][1], 2),  
									" L1 = "  , round(ests[paste0("all_", s)][[1]][2], 2),
									" k = "   , round(ests[paste0("all_", s)][[1]][3], 3) ) )
			if (length(leg) == 3) {
				legend("bottomright", bty = 'n', legend = leg, lty = 1:3, col = line_col, lwd = 2)
			}
		}
	} # close sex loop	

	
	for (a in sources){
		get = 0
		leg = NULL
		for(s in unique(data$Sex)){
		ind = data$Source == a & data$Sex == s
		# Only plot  a sex if it is a certain amount
		if ( length(data[ind, "Sex"]) / dim(data[data$Source == a, ])[1] > 0.10 ) {	
		get = get + 1
	
		if (get == 1) {
		plot(data$Age[ind], data$Length[ind], xlab = "Age", ylab = "Length (cm)", main = a,
			  xaxs = "i", yaxs = "i", ylim = c(0, ymax), xlim = c(0, xmax), pch = 1, col = sex_col[get]) 
		}
		if (get > 1){
			points(data$Age[ind], data$Length[ind], pch = 1, col = sex_col[get]) 
		}	

		if (!is.null(ests) & paste0(a, "_", s)  %in% names(ests)) {
			lines(0:ymax, vb_fn(age = 0:ymax, Linf = ests[paste0(a,"_", s)][[1]][1], L0 = ests[paste0(a,"_", s)][[1]][2], k = ests[paste0(a,"_", s)][[1]][3]), 
				  col = line_col[get], lty = get, lwd = 2)
			leg = c(leg, paste0(s, ": Linf = ", round(ests[paste0(a,"_", s)][[1]][1], 2),  
								     " L1 = " , round(ests[paste0(a,"_", s)][[1]][2], 2),
								     " k = "  , round(ests[paste0(a,"_", s)][[1]][3], 2) ) )
			#if (length(leg) == 3) {
			#	legend("bottomright", bty = 'n', 
			#			legend = leg, lty = 1:3, col = line_col, lwd = 2)}

		} # line loop
		} # proportion sex loop
		} # sex loop
		if (length(leg) > 0 ) {
		  legend("bottomright", bty = 'n', 
		         legend = leg, lty = 1:get, col = line_col, lwd = 2)
		}
	} # source loop	

dev.off()

file = ifelse(is.null(nm_append), "Length_Age_by_State.png",
			  paste0("Length_Age_by_State_", nm_append, ".png"))

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = c(length(unique(data$State)), 1))	
	get = 0

	for (a in sort(unique(data$State))){
		get = get + 1
		ind = data$State == a

		plot(data$Age[ind], data$Length[ind],  ylab = "Length (cm)", xlab = "Age", main = paste0(a, " Combined"),
			 xaxs = "i", yaxs = "i", ylim = c(0, ymax), xlim = c(0, xmax), pch = 1, col = colors[get])

		if (!is.null(ests) & a  %in% names(ests)) {
			lines(0:ymax, vb_fn(age = 0:ymax, Linf = ests[a][[1]][1], L0 = ests[a][[1]][2], k = ests[a][[1]][3]), col = 1, lwd = 2)
			leg = paste0("Combined : Linf = ", round(ests[a][[1]][1], 2),  
								    " L1 = " , round(ests[a][[1]][2], 2),
								    " k = "  , round(ests[a][[1]][3], 3) ) 

			legend("bottomright", bty = 'n', legend = leg , lty = 1, col = 1, lwd = 2)
		}
	}	
dev.off()

file = ifelse(is.null(nm_append), "Length_Age_by_State_Sex.png",
			  paste0("Length_Age_by_State_Sex_", nm_append, ".png"))

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = c(length(unique(data$State)), 1))	
	
	for (a in sort(unique(data$State))){
		get = 0
		leg = NULL
		for (s in unique(data$Sex)){
			ind = data$State == a & data$Sex == s
			# Only plot  a sex if it is a certain amount
			if ( length(data[ind, "Sex"]) / dim(data[data$State == a, ])[1] > 0.10 ) {
			get = get + 1
			if (get == 1){
			plot(data$Age[ind], data$Length[ind],  ylab = "Length (cm)", xlab = "Age", main = a,
				 xaxs = "i", yaxs = "i", ylim = c(0, ymax), xlim = c(0, xmax), pch = 1, col = sex_col[get]) }

			if (get > 1){
				points(data$Age[ind], data$Length[ind], pch = 1, col = sex_col[get]) }

			if (!is.null(ests) & paste0(a, "_", s)  %in% names(ests)) {
				lines(0:ymax, vb_fn(age = 0:ymax, Linf = ests[paste0(a, "_", s)][[1]][1], L0 = ests[paste0(a, "_", s)][[1]][2], k = ests[paste0(a, "_", s)][[1]][3]), 
					  col = line_col[get], lty = get, lwd = 2)
				leg = c(leg, paste0(s, ": Linf = ", round(ests[paste0(a, "_", s)][[1]][1], 2),  
								         " L1 = " , round(ests[paste0(a, "_", s)][[1]][2], 2),
								         " k = "  , round(ests[paste0(a, "_", s)][[1]][3], 3) ) )

			} #if (ests)
			} # proportion sex loop
		} # sex	
		if (length(leg) > 0 ) { legend("bottomright", bty = 'n', legend = leg, lty = 1:get, col = line_col, lwd = 2) }
	} # state loop
dev.off()

}