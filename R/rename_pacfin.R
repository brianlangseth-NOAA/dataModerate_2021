#' Create standarized cleaning and renaming PacFIN data.
#' This should be folded into the cleanPacFIN function.
#'
#' @param data read in pacfin data file
#' @param area_grouping list of area names in data source
#' @param area_names user area names
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
rename_pacfin <- function(data, area_grouping = NULL, area_names = NULL){

	data <- PacFIN.Utilities::cleanPacFIN(Pdata = data, 
								  keep_length_type = c("", "A", "F", "U", "T", NA),
								  keep_missing_lengths = FALSE,
								  keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT","PS"))

	state <- ifelse( data$SOURCE_AGID == "C", "CA", 
		     ifelse( data$SOURCE_AGID == "O", "OR",
		     ifelse( data$SOURCE_AGID == "W", "WA", "OTHER")))
	
	areas <- NA
	if(!is.null(area_grouping)){
		for (a in 1:length(area_grouping)){
			get <- paste(area_grouping[[a]], collapse = "|")
			find = grep(get, data$PCID, ignore.case = TRUE)
			areas[find] = area_names[a]
		}
	}  

	data$Year = data$SAMPLE_YEAR
	data$Lat  = NA
	data$Lon  = NA
	data$State  = state
	data$Areas  = areas
	data$Depth  = NA
	data$Sex    = data$SEX
	data$Length = data$FISH_LENGTH / 10
	data$Weight = data$FISH_WEIGHT
	data$Age    = data$age1
	data$Data_Type = "RETAINED"
	data$Source = "PacFIN"

	return (data)
}