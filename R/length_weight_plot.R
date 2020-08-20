#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param dir
#' @param list of data
#' @param splits
#'
#' @return Nothing - plots
#'
#' @author Chantel Wetzel
#' @export
#'
length_weight_plot <- function(dir, data, splits = NA, nm_append = NULL, haveHandy = TRUE){

  dir.create(file.path(dir, "plots"))
  
  sources = unique(data$Source)
  n = length(sources)
  if (n == 1) { panels = c(1, 1)}
  if (n == 2) { panels = c(3, 1)}
  if (n == 3) { panels = c(2, 2)}
  if (n == 4) { panels = c(3, 2)}
  if (n == 5) { panels = c(3, 2)}
  
  rich.colors.short <- function(n, alpha=1){
    x <- seq(0, 1, length = n)
    r <- 1/(1 + exp(20 - 35 * x))
    g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
    b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
    rgb.m <- matrix(c(r, g, b), ncol = 3)
    rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha=alpha))
  }
  
  colors = rich.colors.short(n + 1, alpha = 0.4)
  
  ymax = max(data$Weight, na.rm = TRUE)
  xmax = max(data$Length, na.rm = TRUE)
  
  
  #By data source
  file = ifelse(is.null(nm_append), "Length_Weight_by_Source.png",
  			  paste0("Length_Weight_by_Source_", nm_append, ".png"))
  
  if(haveHandy) { pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12) }
  if(!haveHandy) { png(file.path(dir, "plots", file), width = 7, height = 7, units = "in", res = 300) }
  
  par(mfrow = panels, mar = c(4,4,2,1))	
  plot(data$Length, data$Weight, xlab = "Length (cm)", ylab = "Weight (kg)", main = "All Data", 
  	 ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = colors[1])
  
  #if (!is.null(est) & "all" %in% names(est)) { #Not sure what this is
  
  #}
  
  get = 0
  for (a in sources){
  	get = get + 1
  	ind = data$Source == a
  	plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = a,
  		ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = colors[get])
  }	
  dev.off()
  
  
  #By sex
  file = ifelse(is.null(nm_append), "Length_Weight_by_Sex.png",
  			  paste0("Length_Weight_by_Sex_", nm_append, ".png"))
  
  sex_col = c("red", 'blue', "grey")

  if(haveHandy) { pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12) }
  if(!haveHandy) { png(file.path(dir, "plots", file), width = 7, height = 7, units = "in", res = 300) }
  
  par(mfrow = c(n+1, length(unique(data$Sex))), mar = c(4,4,2,1))
  ind = 0	
  for(s in unique(data$Sex)) {
  ind = ind + 1	
  plot(data[data$Sex == s, "Length"], data[data$Sex == s, "Weight"], 
  	 xlab = "Length (cm)", ylab = "Weight (kg)", main = paste("All", s), 
  	 ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[ind])
  }
  
  for (a in sources){
  	get = 0
  	for(s in unique(data$Sex)){
  	get = get + 1
  	ind = data$Source == a & data$Sex == s
  	plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = paste(a, s),
  		ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[get])
  	}
  }	
  dev.off()
  
  
  #By state
  file = ifelse(is.null(nm_append), "Length_Weight_by_State.png",
  			  paste0("Length_Weight_by_State_", nm_append, ".png"))
  
  if(haveHandy) { pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12) }
  if(!haveHandy) { png(file.path(dir, "plots", file), width = 7, height = 7, units = "in", res = 300) }
  
  par(mfrow = c(length(unique(data$State)), 1), mar = c(4,4,2,1))	
  get = 0
  for (a in unique(data$State)){
  	get = get + 1
  	ind = data$State == a
  	plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = a,
  		ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = colors[get])
  }	
  dev.off()


  #By state and sex
  file = ifelse(is.null(nm_append), "Length_Weight_by_State_Sex.png",
  			  paste0("Length_Weight_by_State_Sex_", nm_append, ".png"))
  
  if(haveHandy) { pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12) }
  if(!haveHandy) { png(file.path(dir, "plots", file), width = 7, height = 7, units = "in", res = 300) }
  
	par(mfrow = c(length(unique(data$State)), length(unique(data$Sex))))	
	for (a in unique(data$State)){
		get = 0
		for (s in unique(data$Sex)){
			get = get + 1
			ind = data$State == a & data$Sex == s
			plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = paste(a, s),
				ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[get])
		}	
	}
	dev.off()
	
	
	#By state and source
	file = ifelse(is.null(nm_append), "Length_Weight_by_State_Source.png",
	              paste0("Length_Weight_by_State_Source_", nm_append, ".png"))
	
	if(haveHandy) { pngfun(wd = file.path(dir, "plots"), file = file, w = 7, h = 7, pt = 12) }
	if(!haveHandy) { png(file.path(dir, "plots", file), width = 7, height = 7, units = "in", res = 300) }
	
	par(mfrow = c(n, length(unique(data$State))), mar = c(4,4,2,1))	
	for (a in sources){
	  get = 0
	  for (s in unique(data$State)){
	    get = get + 1
	    ind = data$Source == a & data$State == s
	    plot(data$Length[ind], data$Weight[ind], xlab = "Length (cm)", ylab = "Weight (kg)", main = paste(a, s),
	         ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[get])
	  }	
	}
  dev.off()
}