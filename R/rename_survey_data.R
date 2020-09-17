#' Create standarized field for survey data
#'
#' @param data read in biological survey data
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
rename_survey_data <- function(data){

	state <- ifelse( data$Latitude_dd < 42, "CA", 
			 ifelse( data$Latitude_dd < 46 & data$Latitude_dd >= 42, "OR",
			 ifelse( data$Latitude_dd >= 46, "WA", "OTHER")))
	
	areas <- NA 

	project <- ifelse(unique(data$Project) == "NWFSC.Combo", "NWFSC_WCGBTS",
		       ifelse(unique(data$Project) == "Triennial", "Triennial",
		       ifelse(unique(data$Project) == "AK.Slope", 'AFSC_Slope',
		       ifelse(unique(data$Project) == "NWFSC.Slope", "NWFSC_Slope",
		       	"Other_Survey"))))

	data$Year = data$Year
	data$Lat  = data$Latitude_dd
	data$Lon  = data$Longitude_dd
	data$State  = state
	data$State_Areas = NA
	data$Areas  = areas
	data$Depth  = data$Depth_m
	data$Sex    = data$Sex
	data$Length = data$Length_cm
	data$Weight = data$Weight
	data$Age    = data$Age
	data$Data_Type = "RETAINED"
	data$Source = project	

	return (data)
}