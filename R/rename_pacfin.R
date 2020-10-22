#' Create standarized cleaning and renaming PacFIN data.
#' This should be folded into the cleanPacFIN function.
#'
#' @param data read in pacfin data file
#' @param area_grouping list of grouped area names in data source
#' @param area_names user area names
#' @param fleet_grouping list of grouped fleet names in data source
#' @param fleet_names user fleet names
#' @param fleet_column_name name of the column to group fleets by in the PacFIN data
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
rename_pacfin <- function(data, area_grouping = NULL, area_names = NULL, fleet_grouping = NULL, fleet_names = NULL, fleet_column_name = "COND"){

	data <- PacFIN.Utilities::cleanPacFIN(Pdata = data, 
								  keep_length_type = c("", "A", "F", "U", "T", NA),
								  keep_missing_lengths = FALSE,
								  keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT","PS"))

	state <- ifelse( data$SOURCE_AGID == "C", "CA", 
		     ifelse( data$SOURCE_AGID == "O", "OR",
		     ifelse( data$SOURCE_AGID == "W", "WA", "OTHER")))

	fleets <- NA
	if(!is.null(fleet_grouping)){
	fleets = pacfin_fleets(data = data, 
						   fleet_grouping = fleet_grouping, 
						   fleet_names = fleet_names, 
						   fleet_column_name = fleet_column_name)
	}
	
	state_areas <- NA
	if(!is.null(area_grouping)){
		for (a in 1:length(area_grouping)){
			get <- paste(area_grouping[[a]], collapse = "|")
			find = grep(get, data$PCID, ignore.case = TRUE)
			state_areas[find] = area_names[a]
		}
	}

	age = data$age1
	age[which(age == -1)] = NA  

	data$Year = data$SAMPLE_YEAR
	data$Lat  = NA
	data$Lon  = NA
	data$State  = state
	data$State_Areas  = state_areas
	data$Areas  = NA
	data$Depth  = NA
	data$Sex    = data$SEX
	data$Length = data$FISH_LENGTH / 10
	data$Weight = data$FISH_WEIGHT / 2.20462
	data$Age    = age
	data$Fleet  = fleets
	data$Data_Type = "RETAINED"
	data$Source = "PacFIN"

	return (data)
}