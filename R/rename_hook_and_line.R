#' Create standarized field for hook & line data
#'
#' @param data read in hook and line data
#' @param survey_name 
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
rename_hook_and_line <- function(data, survey_name = NULL){

	state <- ifelse( data$drop_latitude_degrees < 42, "CA", 
		     ifelse( data$drop_latitude_degrees < 46 & data$drop_latitude_degrees >= 42, "OR",
		     ifelse( data$drop_latitude_degrees >= 46, "WA", "OTHER")))
	
	areas <- ifelse( data$site_number >= 500, "CCA", "non_CCA")

	if (is.null(survey_name)){
		survey_name = "survey"
	}

	data$Year = data$year
	data$Lat  = data$drop_latitude_degrees
	data$Lon  = data$drop_longitude_degrees
	data$State  = state
	data$State_Areas = "south_pt_concep"
	data$Areas  = areas
	data$Depth  = data$drop_depth_meters
	data$Sex    = data$sex
	data$Length = data$length_cm
	data$Weight = data$weight_kg
	data$Age    = data$age_years
	data$Fleet  = survey_name
	data$Data_Type = "RETAINED"
	data$Source = "NWFSC_HKL"	

	return (data)
}