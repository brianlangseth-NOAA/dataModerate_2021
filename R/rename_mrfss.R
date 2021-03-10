#' Create standarized field for RecFIN data
#'
#' @param data read in RecFIN data
#' @param area_grouping list of area names in data source
#' @param area_names user area names
#' @param len_col name of the length column to use
#' @param len_divide value to convert between mm and cm
#' @param column_name area field that can be used to specify catch area
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
rename_mrfss <- function(data, len_col = "T_LEN", len_divide = 10, 
						 area_grouping = NULL, area_names = NULL, area_column_name = "DIST", 
						 mode_grouping = NULL, mode_names = NULL, mode_column_name = NULL){

	# Check for decimal lengths and remove
	length <- NA
	# Removing for now under the assumption that the conversion between fork and total is appropriate and 
	# does not bias the data.  The practice of which length type was recorded since to change over time by
	# state. Throwing out one or the other would remove periods of recreational data.  This decision should
	# be revisted at some time in the future.
	#keep <- which(data[,len_col] == floor(data[,len_col]))
	#length[keep] <- data[keep, len_col] / len_divide
	length <- data[,len_col] / len_divide

	# Set any computed weights to NA
	weight <- rep(NA, nrow(data))
	find = which(colnames(data) %in% c("WGT", "Weight"))
	keep <- which(data[,find] == round(data[,find], 2))
	weight[keep] <- data[keep, find]


	state <- ifelse( data$STATE_NAME %in% c("CALIFORNIA", "CA", "C"), "CA", 
		     ifelse( data$STATE_NAME %in% c("OREGON", "OR", "O"), "OR",
		     ifelse( data$STATE_NAME %in% c("WASHINGTON", "WA", "W"), "WA", "OTHER")))

	sex <- NA
	if(sum(colnames(data) %in% "F_SEX")){
		sex[data$F_SEX == 2] = "F"
		sex[data$F_SEX == 1] = "M"
		sex[!data$F_SEX %in% c("F", "M")] = "U"
	} else {
		sex <- "U"
	}

	find = which(colnames(data) %in% c("Year", "YEAR"))
	year = data[,find]
	
	if(!is.null(area_grouping)){
		data$State_Areas = recfin_areas(data = data, 
									    area_grouping = area_grouping, 
										area_names = area_names,
										column_name = area_column_name)
	} else {
		data$State_Areas = NA
	}

	# Oregon mode codes correspond as: 1 = shore/manmade, 2 = shore/beach bank, 6 = charter, 7 = private boas
	#find = which(colnames(data) %in% c("MRFSS_MODE_FX", "MODE_FX"))
	#modes = NA
	#if (length(find) > 0){
	#	modes[data[,find] == 1] = "shore_manmade"
	#	modes[data[,find] == 2] = "shore_beachbank"
	#	modes[data[,find] == 6] = "charter"
	#	modes[data[,find] == 7] = "private_boat"
	#}

	mode <- NA
	if(!is.null(mode_grouping)){
		for (a in 1:length(mode_grouping)){
			get <- paste(mode_grouping[[a]], collapse = "|")
			find = grep(get, data[, mode_column_name], ignore.case = TRUE)
			mode[find] = mode_names[a]
		}
	}

	data$Year = year
	data$Lat = NA
	data$Lon = NA
	data$State  = state
	data$State_Areas = data$State_Areas
	data$Areas  = NA
	data$Depth  = NA
	data$Sex    = sex
	data$Length = length
	data$Weight = weight
	data$Age    = NA
	data$Fleet  = mode 
	data$Data_Type = NA
	data$Source = "RecFIN_MRFSS"	

	return (data)
}