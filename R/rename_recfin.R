#' Create standarized field for RecFIN data
#'
#' @param data read in RecFIN data
#' @param area_grouping list of area names in data source
#' @param area_names user area names
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
rename_recfin <- function(data, area_grouping = NULL, area_names = NULL, column_name = "DIST"){

	col = which( colnames(data) %in% c("STATE_NAME", "State.Name"))
	if (length(col) > 0){
		state <- ifelse( data[,col] %in% c("CALIFORNIA", "CA", "C"), "CA", 
			     ifelse( data[,col] %in% c("OREGON", "OR", "O"), "OR",
			     ifelse( data[,col] %in% c("WASHINGTON", "WA", "W"), "WA", "OTHER")))
	} else { 
		stop("State name column not found. Double check file.")
	}

	col = which( colnames(data) %in% c("FISH_SEX", "Fish.Sex") )
	if (length(col) > 0){
		sex <- ifelse(data[,col] == "F" | data[,col] == 2, "F",
		   	   ifelse(data[,col] == "M" | data[,col] == 1, "M",
		   		  "U"))
	} else {
		message("Sex data column not found. Double check file.")
	}
	
	if(!is.null(area_grouping)){
		State_Areas = recfin_areas(data = data, 
								   area_grouping = area_grouping, 
								   area_names = area_names,
								   column_name = column_name)
	} else {
		data$State_Areas = NA
	}

	col = which( colnames(data) %in% c("RECFIN_YEAR", "RecFIN.Year"))
	if (length(col) > 0) {
		year = data[,col]
	} else {
		stop("Year column not found. Double check file.")
	}	

	col = which( colnames(data) %in% c("RECFIN_LENGTH_MM", "RecFIN.Length.MM"))
	if (length(col) > 0) {
		length = data[,col] / 10
	} else {
		length = NA
		message("Length data column not found. Double check file.")
	}

	col = which(colnames(data) %in% c("AGENCY_WEIGHT", "Agency.Weight"))
	if (length(col) > 0) {
		weight = data[,col]
	} else {
		weight = NA
		message("Weight data column not found. Double check file.")
	}

	col = which(colnames(data) %in% c("IS_RETAINED", "Is.Retained"))
	if (length(col) > 0) {
		retain = data[,col]
	} else {
		retain = NA
	}

	col = which(colnames(data) %in% c("FISH_AGE", "USE_THIS_AGE"))
	if (length(col) > 0) {
		age = data[,col]
	} else {
		age = NA
	}

	data$Year = year
	data$Lat = NA
	data$Lon = NA
	data$State  = state
	data$State_Areas = State_Areas
	data$Areas  = NA
	data$Depth  = NA
	data$Sex    = sex
	data$Length = length
	data$Weight = weight
	data$Age    = age
	data$Data_Type = retain
	data$Source = "RecFIN"	

	return (data)
}