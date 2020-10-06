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

	state <- ifelse( data$LATDD < 42, "CA", 
		     ifelse( data$LATDD < 46 & data$LATDD >= 42, "OR",
		     ifelse( data$LATDD >= 46, "WA", "OTHER")))
	
	areas <- ifelse( data$SITENAME >= 500, "CCA", "non_CCA")

	if (is.null(survey_name)){
		survey_name = "survey"
	}

	data$Year = data$YEAR
	data$Lat  = data$LATDD
	data$Lon  = data$LONDD
	data$State  = state
	data$State_Areas = "south_pt_concep"
	data$Areas  = areas
	data$Depth  = data$DEPTHM
	data$Sex    = data$SEX
	data$Length = data$LENGTH
	data$Weight = data$WEIGHT
	data$Age    = NA
	data$Fleet  = survey_name
	data$Data_Type = "RETAINED"
	data$Source = "NWFSC_HKL"	

	return (data)
}