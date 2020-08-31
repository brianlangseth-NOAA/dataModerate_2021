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
rename_recfin <- function(data, area_grouping = NULL, area_names = NULL){

	state <- ifelse( data$STATE_NAME %in% c("CALIFORNIA", "CA", "C"), "CA", 
		     ifelse( data$STATE_NAME %in% c("OREGON", "OR", "O"), "OR",
		     ifelse( data$STATE_NAME %in% c("WASHINGTON", "WA", "W"), "WA", "OTHER")))

	sex <- ifelse(data$FISH_SEX == "F" | data$FISH_SEX == 2, "F",
		   ifelse(data$FISH_SEX == "M" | data$FISH_SEX == 1, "M",
		   		  "U"))
	
	if(!is.null(area_grouping)){
		data$State_Areas = recfin_areas(data = data, 
									    area_grouping = area_grouping, 
										area_names = area_names)
	} else {
		data$State_Areas = NA
	}

	data$Year = data$RECFIN_YEAR
	data$Lat = NA
	data$Lon = NA
	data$State  = state
	data$State_Areas = data$State_Areas
	data$Areas  = NA
	data$Depth  = NA
	data$Sex    = sex
	data$Length = data$RECFIN_LENGTH_MM / 10
	data$Weight = data$AGENCY_WEIGHT
	data$Age    = data$FISH_AGE
	data$Data_Type = data$IS_RETAINED
	data$Source = "RecFIN"	

	return (data)
}