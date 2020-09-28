#' Create fleet naming for PacFIN
#'
#' @param data read in PacFIN data
#' @param fleet_grouping list of area names in data source
#' @param fleet_names user area names
#' @param column_name name to do the grouping with (default is based on recfin)
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
#'
#'
pacfin_fleets <- function(data, fleet_grouping, fleet_names, fleet_column_name = ""){

	fleet <- NA
	for (a in 1:length(fleet_grouping)){
		get <- paste(fleet_grouping[[a]], collapse = "|")
		find = grep(get, data[,fleet_column_name], ignore.case = TRUE)
		fleet[find] = area_fleets[a]
	}
	
	return (fleet)
}