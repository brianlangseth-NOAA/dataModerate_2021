#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param list of data sets
#' @param 
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
create_data_frame <- function(data_list, areas = NA){

	all_data = NA
	for (a in 1:length(data_list)){

	  # Grab the Hook and Line Survey data
		if ("HOOKID" %in% colnames(data_list[[a]]) & length(row.names(data_list[[a]])) > 0 ){

			state <- ifelse( data_list[[a]]$LATDD < 42, "CA", 
				     ifelse( data_list[[a]]$LATDD < 46 & data_list[[a]]$LATDD >= 42, "OR",
				     ifelse( data_list[[a]]$LATDD >= 46, "WA", "OTHER")))

			if (!is.na(areas)) { 
				areas = data_list[[a]]$GENLOC}

			hkl    <- data.frame(Year = data_list[[a]]$YEAR,
								 Lat = data_list[[a]]$LATDD,
								 Lon = data_list[[a]]$LONDD,
								 State  = state,
								 Areas  = areas,
								 Depth  = data_list[[a]]$DEPTHM,
								 Sex    = data_list[[a]]$SEX,
								 Length = data_list[[a]]$LENGTH,
								 Weight = data_list[[a]]$WEIGHT,
								 Age    = NA,
								 Source = "nwfsc_hkl")	

			all_data = rbind(all_data, hkl)
		}

		# Grab the Survey data
		if ("Project" %in% colnames(data_list[[a]]) & length(row.names(data_list[[a]])) > 0 ){

			state <- ifelse(data_list[[a]]$Latitude_dd < 42, "CA", 
						    ifelse( data_list[[a]]$Latitude_dd < 46 & data_list[[a]]$Latitude_dd >= 42, "OR",
						    ifelse( data_list[[a]]$Latitude_dd >= 46, "WA", "OTHER")))
			
			if (!is.na(areas)) { areas = NA }

			combo <- data.frame(Year = data_list[[a]]$Year,
								Lat = data_list[[a]]$Latitude_dd,
								Lon = data_list[[a]]$Longitude_dd,
								State  = state,
								Areas  = areas,
								Depth  = data_list[[a]]$Depth_m,
								Sex    = data_list[[a]]$Sex,
								Length = data_list[[a]]$Length_cm,
								Weight = data_list[[a]]$Weight,
								Age    = data_list[[a]]$Age,
								Source = "nwfsc_wcgbts")	

			all_data = rbind(all_data, combo)
		}

		# Grab the PACFIN data
		if ("CLUSTER_NO" %in% colnames(data_list[[a]]) & length(row.names(data_list[[a]])) > 0 ){

			state <- ifelse( data_list[[a]]$SOURCE_AGID == "C", "CA", 
				     ifelse( data_list[[a]]$SOURCE_AGID == "O", "OR",
				     ifelse( data_list[[a]]$SOURCE_AGID == "W", "WA", "OTHER")))
			
			if (!is.na(areas)) { 
				areas = data_list[[a]]$PORT }

			comm  <- data.frame(Year = data_list[[a]]$SAMPLE_YEAR,
								Lat  = NA,
								Lon  = NA,
								State  = state,
								Areas  = areas,
								Depth  = NA,
								Sex    = data_list[[a]]$SEX,
								Length = data_list[[a]]$FISH_LENGTH / 10,
								Weight = data_list[[a]]$FISH_WEIGHT,
								Age    = data_list[[a]]$age1,
								Source = "pacfin")

			all_data = rbind(all_data, comm)	
		}

		# Grab the RECFIN data
		if ("RECFIN_YEAR" %in% colnames(data_list[[a]]) & length(row.names(data_list[[a]])) > 0 ){

			state <- ifelse( data_list[[a]]$STATE_NAME == "CALIFORNIA", "CA", 
				     ifelse( data_list[[a]]$STATE_NAME == "OREGON", "OR",
				     ifelse( data_list[[a]]$STATE_NAME == "WASHINGTON", "WA", "OTHER")))
			
			if (!is.na(areas)) { 
				areas = data_list[[a]]$RECFIN_PORT_NAME }
			
			#Rename factors for Sex ###Will need to revise by state to ensure codes are accurate
			levels(data_list[[a]]$FISH_SEX)[levels(data_list[[a]]$FISH_SEX)=="1"] <- "M"
			levels(data_list[[a]]$FISH_SEX)[levels(data_list[[a]]$FISH_SEX)=="2"] <- "F"
			levels(data_list[[a]]$FISH_SEX)[levels(data_list[[a]]$FISH_SEX)=="3"] <- "U"
			levels(data_list[[a]]$FISH_SEX)[levels(data_list[[a]]$FISH_SEX)=="6"] <- "U"
			levels(data_list[[a]]$FISH_SEX)[levels(data_list[[a]]$FISH_SEX)=="8"] <- "U"
			
			#Used to adjust weight to be kg. Lots of fish dont have units so assume anything under 20 without a unit is kg
			scalar = ifelse(data_list[[a]]$AGENCY_WEIGHT_UNITS == "K" | 
			                  (data_list[[a]]$AGENCY_WEIGHT_UNITS == "" & data_list[[a]]$AGENCY_WEIGHT < 20), 1, 1000)
			
			rec  <- data.frame(Year = data_list[[a]]$RECFIN_YEAR,
								Lat = NA,
								Lon = NA,
								State  = state,
								Areas  = areas,
								Depth  = NA,
								Sex    = data_list[[a]]$FISH_SEX,
								Length = data_list[[a]]$RECFIN_LENGTH_MM / 10,
								Weight = data_list[[a]]$AGENCY_WEIGHT / scalar,
								Age    = NA,
								Source = "recfin")

			all_data <- rbind(all_data, rec)	
		}
	  
	  # Grab triennial data
	  if (names(data_list[[a]])[1] == "Lengths" & length(row.names(data_list[[a]][[1]])) > 0 ){
	    
	    state <- ifelse(data_list[[a]]$Lengths$Latitude_dd < 42, "CA", 
	                    ifelse( data_list[[a]]$Lengths$Latitude_dd < 46 & data_list[[a]]$Lengths$Latitude_dd >= 42, "OR",
	                            ifelse( data_list[[a]]$Lengths$Latitude_dd >= 46, "WA", "OTHER")))
	    
	    if (!is.na(areas)) { areas = NA }
	    
	    rec  <- data.frame(Year = data_list[[a]]$Lengths$Year,
	                       Lat = data_list[[a]]$Lengths$Latitude_dd,
	                       Lon = data_list[[a]]$Lengths$Longitude_dd,
	                       State  = state,
	                       Areas  = areas,
	                       Depth  = data_list[[a]]$Lengths$Depth_m,
	                       Sex    = data_list[[a]]$Lengths$Sex,
	                       Length = data_list[[a]]$Lengths$Length_cm,
	                       Weight = data_list[[a]]$Lengths$Weight,
	                       Age    = data_list[[a]]$Lengths$Age,
	                       Source = "triennial")
	    
	    all_data = rbind(all_data, rec)	
	  }
	  
	}

	all_data = all_data[!is.na(all_data$Year), ]
	return (all_data)
}