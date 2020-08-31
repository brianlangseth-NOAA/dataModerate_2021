#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param list of data sets
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
create_data_frame <- function(data_list){

	all_data = NA
	for (a in 1:length(data_list)){

		tmp  <- data.frame(Year = data_list[[a]]$Year,
							Lat = data_list[[a]]$Lat,
							Lon = data_list[[a]]$Lon,
							State  = data_list[[a]]$State,
							Areas  = data_list[[a]]$Areas,
							Depth  = data_list[[a]]$Depth,
							Sex    = data_list[[a]]$Sex,
							Length = data_list[[a]]$Length,
							Weight = data_list[[a]]$Weight,
							Age    = data_list[[a]]$Age,
							Data_Type = data_list[[a]]$Data_Type,
							Source = data_list[[a]]$Source)

		all_data = rbind(all_data, tmp)			
	}

	all_data = all_data[!is.na(all_data$Year), ]
	return (all_data)
}