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
rename_recfin <- function(data, area_grouping = NULL, area_names = NULL, column_name = NULL){

	col = which( colnames(data) %in% c("STATE_NAME", "State.Name", "SAMPLING_AGENCY_NAME"))
	if (length(col) > 0){
		state <- ifelse( data[,col] %in% c("CALIFORNIA", "CA", "C", "CDFW"), "CA", 
			     ifelse( data[,col] %in% c("OREGON", "OR", "O", "ODFW"), "OR",
			     ifelse( data[,col] %in% c("WASHINGTON", "WA", "W", "WDFW"), "WA", "OTHER")))
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
	
	State_Areas = NA
	if(!is.null(area_grouping)){
		State_Areas = recfin_areas(data = data, 
								   area_grouping = area_grouping, 
								   area_names = area_names,
								   column_name = column_name)
	} 

	col = which( colnames(data) %in% c("RECFIN_YEAR", "RecFIN.Year"))
	if (length(col) > 0) {
		year = data[,col]
	} else {
		stop("Year column not found. Double check file.")
	}	

	col = which( colnames(data) %in% c("RECFIN_LENGTH_MM", "RecFIN.Length.MM"))
	length = NA
	if (length(col) > 0) {
		length = data[,col] / 10
	} else {
		message("Length data column not found. Double check file.")
	}

	col = which(colnames(data) %in% c("AGENCY_WEIGHT", "Agency.Weight"))
	weight = NA
	if (length(col) > 0) {
		weight = data[,col]
	} else {
		message("Weight data column not found. Double check file.")
	}

	col = which(colnames(data) %in% c("IS_RETAINED", "Is.Retained"))
	retain = NA
	if (length(col) > 0) {
		retain = data[,col]
	} 

	col = which(colnames(data) %in% c("FISH_AGE", "USE_THIS_AGE"))
	age = NA
	if (length(col) > 0) {
		age = data[,col]
	} 

	col = which(colnames(data) %in% c("RecFIN.Mode.Name"))
	modes = NA
	if (length(col) > 0){
		modes[data[,col] == "BEACH/BANK"] = "shore_beachbank"
		modes[data[,col] == "MAN-MADE/JETTY"] = "shore_manmade"
		modes[data[,col] == "PARTY/CHARTER BOATS"] = "charter"
		modes[data[,col] == "PRIVATE/RENTAL BOATS"] = "private"
		modes[data[,col] == "NOT KNOWN"] = "unknown"
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
	data$Fleet  = modes
	data$Data_Type = retain
	data$Source = "RecFIN"	

	return (data)
}