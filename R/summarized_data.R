#' Summarize all the data to evaluate the samples by source,
#' year, state, and biological measurement (length, weight, age)
#'
#' @param dir directory location to save output
#' @param data data frame created by the create_data_frame function
#'
#' @return list
#'
#' @author Chantel Wetzel
#' @export
#'
summarize_data <- function(dir = NULL, data){

	summary_list <- list()
	summary_list$sources <- unique(data$Source) 
	summary_list$sample_yrs <- table(data$Year, data$Source)
	summary_list$sample_by_area <- table(data$Year, data$State_Areas)

	
	place = 1
	data_sum <- list()
	for(s in sort(unique(data$Source))){
		yrs = sort(unique(data[data$Source == s, "Year"]))
		mat = matrix(NA, length(yrs), 3)
		rownames(mat) = yrs
		colnames(mat) = c("Length", 'Weight', 'Age')
		ind = 1
		for( y in yrs){
			mat[ind,] = c(sum(!is.na(data[data$Source == s & data$Year == y, "Length"])), 
						  sum(!is.na(data[data$Source == s & data$Year == y, "Weight"])),
						  sum(!is.na(data[data$Source == s & data$Year == y, "Age"])) )	
			ind = ind + 1
		}
		data_sum[[place]] = mat
		place = place + 1
	}
	names(data_sum) = sort(unique(data$Source))
	summary_list$BySource = data_sum

	
	place = 1
	data_sum <- list()
	for(t in sort(unique(data$State))){
	  yrs = sort(unique(data[data$State == t, "Year"]))
	  mat = matrix(NA, length(yrs), 3)
	  rownames(mat) = yrs
	  colnames(mat) = c("Length", 'Weight', 'Age')
	  ind = 1
	  for( y in yrs){
	    mat[ind,] = c(sum(!is.na(data[data$State == t & data$Year == y, "Length"])), 
	                  sum(!is.na(data[data$State == t & data$Year == y, "Weight"])),
	                  sum(!is.na(data[data$State == t & data$Year == y, "Age"])) )	
	    ind = ind + 1
	  }
	  data_sum[[place]] = mat
	  place = place + 1
	}
	names(data_sum) = sort(unique(data$State))
	summary_list$ByState = data_sum


	place = 1; nm = NULL
	data_sum <- list()
	for(t in sort(unique(data$Source))){
	  for(a in sort(unique(data$State))){
	  	yrs = sort(unique(data[data$Source == t & data$State == a, "Year"]))
	  	if (length(yrs) > 0){
	  		mat = matrix(NA, length(yrs), 3)
	  		rownames(mat) = yrs
	  		colnames(mat) = c("Length", 'Weight', 'Age')
	  		ind = 1
	  		for( y in yrs){
	  		  get = which(data$Source == t & data$State == a & data$Year == y)
	  		  mat[ind,] = c(sum(!is.na(data[get, "Length"])), 
	  		                sum(!is.na(data[get, "Weight"])),
	  		                sum(!is.na(data[get, "Age"])) )	
	  		  ind = ind + 1
	  		}
	  		data_sum[[place]] = mat
	  		place = place + 1
	  		nm = c(nm, paste0(t, "_", a))
	  	}
	  }
	}
	names(data_sum) = nm #paste0(sort(unique(data$Source)),"_", sort(unique(data$State)))
	summary_list$BySource_ByState = data_sum	

	place = 1; nm = NULL
	data_sum <- list()
	for(t in sort(unique(data$Source))){
	  for(a in sort(unique(data$State_Areas))){
	  	yrs = sort(unique(data[data$Source == t & data$State_Areas == a, "Year"]))
	  	if (length(yrs) > 0){
	  		mat = matrix(NA, length(yrs), 3)
	  		rownames(mat) = yrs
	  		colnames(mat) = c("Length", 'Weight', 'Age')
	  		ind = 1
	  		for( y in yrs){
	  		  get = which(data$Source == t & data$State_Areas == a & data$Year == y)
	  		  mat[ind,] = c(sum(!is.na(data[get, "Length"])), 
	  		                sum(!is.na(data[get, "Weight"])),
	  		                sum(!is.na(data[get, "Age"])) )	
	  		  ind = ind + 1
	  		}
	  		data_sum[[place]] = mat
	  		place = place + 1
	  		nm = c(nm, paste0(t, "_", a))
	  	}
	  }
	}
	names(data_sum) = nm #paste0(sort(unique(data$Source)),"_", sort(unique(State_Area)))
	summary_list$BySource_ByArea = data_sum	
	
	
	if(!is.null(dir)){ 
		cat(capture.output(print(summary_list), file = file.path(dir, "data_summary.txt"))) }

	return(summary_list)
}