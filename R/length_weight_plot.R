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
length_weight_plot <- function(dir, data, splits = NA, nm_append = NULL, ests = NULL){

dir.create(file.path(dir, "plots"), showWarnings = FALSE)

remove = NULL
# Determine if all data sources have lengths & weights
for (s in unique(data$Source)){
	check_len  <- check <- sum( !is.na( data[data$Source == s, "Length"])) == 0
	check_wght <- sum( !is.na( data[data$Source == s, "Weight"])) == 0
	if (check_len | check_wght) {remove <- c(remove, s)}
}

data <- data[!data$Source %in% remove, ]

sources = unique(data$Source)
n = length(sources)
if (n == 1) { panels = c(1, 1)}
if (n == 2) { panels = c(3, 1)}
if (n == 3) { panels = c(2, 2)}
if (n == 4) { panels = c(3, 2)}


colors = rich.colors.short(n + 1, alpha = 0.4)

lens = 1:max(data$Length, na.rm = TRUE)

ymax = max(data$Weight, na.rm = TRUE)
xmax = max(data$Length, na.rm = TRUE)

file = ifelse(is.null(nm_append), "Length_Weight_by_Source.png",
			  paste0("Length_Weight_by_Source_", nm_append, ".png"))

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = panels)	
	plot(data$Length, data$Weight, xlab = "Length (cm)", ylab = "Weight (kg)", main = "All Data", 
		 ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = colors[1])

	if (!is.null(ests) & "all" %in% names(ests)) {
		lines(lens, ests$all[1] * lens ^ ests$all[2],col = 1, lwd = 2)
		legend("topleft", bty = 'n', 
				legend = paste0("Combined: a = ", signif(ests$all[1], digits = 3), " b = ", round(ests$all[2], 2) ), 
				lty = 1, col = 1, lwd = 2)
	}
	

	for (a in sources){
		ind = data$Source == a
		plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = a,
			ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = colors[which(a %in% sources)])

		if (!is.null(ests) & a  %in% names(ests)) {
			lines(lens, ests[a][[1]][1] * lens ^ ests[a][[1]][2], col = 1, lwd = 2)
			legend("topleft", bty = 'n', 
				    paste0("Combined: a = ", signif(ests[a][[1]][1], digits = 3), " b = ", round(ests[a][[1]][2], 2) ),, 
				    lty = 1, col = 1, lwd = 2)}	
	}
		
dev.off()

file = ifelse(is.null(nm_append), "Length_Weight_by_Sex.png",
			  paste0("Length_Weight_by_Sex_", nm_append, ".png"))

line_col = c("red", 'blue', "grey")
sex_col = alpha(line_col, 0.20)

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = c(panels[1], panels[2]) )
		
	ind = 0; leg = NULL
	for(s in unique(data$Sex)) {
		ind = ind + 1	
		if (ind == 1) {
			plot(data[data$Sex == s, "Length"], data[data$Sex == s, "Weight"], 
			     xlab = "Length (cm)", ylab = "Weight (kg)", main = "All Sources", 
			     ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[ind]) 
		}
		if (ind > 1){
			points(data[data$Sex == s, "Length"], data[data$Sex == s, "Weight"], pch = 16, col = sex_col[ind])
		}	
		
		if (!is.null(ests) & paste0("all_", s)  %in% names(ests)) {
			lines(lens, ests[paste0("all_", s)][[1]][1] * lens ^ ests[paste0("all_", s)][[1]][2], col = line_col[ind], lwd = 2, lty = ind)
			leg = c(leg, paste0(s, ": a = ", signif(ests[paste0("all_", s)][[1]][1], digits = 3),  
									" b = ", round(ests[paste0("all_", s)][[1]][2], 2) ) )
			if (length(leg) == 3) {
				legend("topleft", bty = 'n', legend = leg, lty = 1:3, col = line_col, lwd = 2)
			}
		}
	} # close sex loop	

	
	for (a in sources){
		get = 0
		leg = NULL
		for(s in unique(data$Sex)){
		get = get + 1
		ind = data$Source == a & data$Sex == s
		if (get == 1) {
		plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = a,
			ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[get]) }
		if (get > 1){
		points(data$Length[ind], data$Weight[ind], pch = 16, col = sex_col[get]) }	

		if (!is.null(ests) & paste0(a, "_", s)  %in% names(ests)) {
			lines(lens, ests[paste0(a,"_", s)][[1]][1] * lens ^ ests[paste0(a, "_", s)][[1]][2], col = line_col[get], lwd = 2, lty = get)
			leg = c(leg, paste0(s, ": a = ", signif(ests[paste0(a,"_", s)][[1]][1], digits = 3),  
								" b = ", round(ests[paste0(a,"_", s)][[1]][2], 2) ) )
			if (length(leg) == 3) {
				legend("topleft", bty = 'n', 
						legend = leg, lty = 1:3, col = line_col, lwd = 2)}
		}
		} # sex loop
	} # source loop	

dev.off()

file = ifelse(is.null(nm_append), "Length_Weight_by_State.png",
			  paste0("Length_Weight_by_State_", nm_append, ".png"))

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = c(length(unique(data$State)), 1))	
	get = 0
	for (a in unique(data$State)){
		get = get + 1
		ind = data$State == a
		plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = paste0(a, " Combined"),
			ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = colors[get])

		if (!is.null(ests) & a  %in% names(ests)) {
			lines(lens, ests[a][[1]][1] * lens ^ ests[a][[1]][2], col = 1, lwd = 2, lty = 1)
			legend("topleft", bty = 'n', 
				   legend = paste0("Combined : a = ", signif(ests[a][[1]][1], digits = 3)," b = ", round(ests[a][[1]][2], 2) ) , 
				   lty = 1, col = 1, lwd = 2)
		}
	}	
dev.off()

file = ifelse(is.null(nm_append), "Length_Weight_by_State_Sex.png",
			  paste0("Length_Weight_by_State_Sex_", nm_append, ".png"))

pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12)
	par(mfrow = c(length(unique(data$State)), 1))	
	
	for (a in unique(data$State)){
		get = 0
		leg = NULL
		for (s in unique(data$Sex)){
			get = get + 1
			ind = data$State == a & data$Sex == s
			if (get == 1){
			plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = a,
				ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[get]) }

			if (get > 1){
				points(data$Length[ind], data$Weight[ind], pch = 16, col = sex_col[get]) }

			if (!is.null(ests) & paste0(a, "_", s)  %in% names(ests)) {
				lines(lens, ests[paste0(a, "-", s)][[1]][1] * lens ^ ests[paste0(a, "-", s)][[1]][2], col = line_col[get], lwd = 2, lty = get)
				leg = c(leg, paste0(s, ": a = ", signif(ests[paste0(a, "-", s)][[1]][1], digits = 3),  
								        " b = ", round(ests[paste0(a, "-", s)][[1]][2], 2) ) )
				if (length(leg) == 3) {
					legend("topleft", bty = 'n', legend = leg, lty = 1:3, col = line_col, lwd = 2)}
			} #if (ests)
		} # sex	
	} # state loop
dev.off()

}
