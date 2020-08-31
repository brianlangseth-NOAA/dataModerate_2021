#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param list of data
#' @param grouping options: all, source, state
#' @return data frame with estimated length-at-weight
#'
#' @author Chantel Wetzel
#' @export
#'
estimate_length_weight <- function(data, grouping = "all"){

	remove = NULL
	# Determine if all data sources have lengths & weights
	for (s in unique(data$Source)){
		check_len  <- check <- sum( !is.na( data[data$Source == s, "Length"])) == 0
		check_wght <- sum( !is.na( data[data$Source == s, "Weight"])) == 0
		if (check_len | check_wght) {remove <- c(remove, s)}
	}

	data <- data[!data$Source %in% remove, ]
	n_sex <- unique(data$Sex)
	n_state <- unique(data$State)
	n_source <- unique(data$Source)


	len_weight_list <- list()
	nm = NULL
	len_weight_list[[1]] <- c( exp( lm(log(data$Weight) ~ log(data$Length), na.action = na.omit)$coefficients[1]),  	
								   lm(log(data$Weight) ~ log(data$Length), na.action = na.omit)$coefficients[2])	
	nm = "all"

	t = 1
	for (a in unique(data$Sex)){
		if (sum(data$Sex == a) > 0){
		t = t + 1
		len_weight_list[[t]] <- c( exp( lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$Sex == a)$coefficients[1]),
								  lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$Sex == a)$coefficients[2] )  
		nm = c(nm, paste0("all_", a))	

		}
	}
	
	for(a in unique(data$Source)){
		t = t + 1
		len_weight_list[[t]] <- c( exp( lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$Source == a)$coefficients[1]),
										lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$Source == a)$coefficients[2] )  
		nm = c(nm, a)		
	}

	for(a in unique(data$State)){
		
		check = data[data$State == a, c("Length", "Weight")]
		if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Weight)) != dim(check)[1] ){
		t = t +1
		len_weight_list[[t]] <- c( exp( lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$State == a)$coefficients[1]),
									    lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$State == a)$coefficients[2] ) 	
		nm = c(nm, a)
		}		
	}

	for(a in unique(data$State)){
		for (b in unique(data$Source)){
			check = data[data$State == a & data$Source == b, c("Length", "Weight")]
			if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Weight)) != dim(check)[1] ){
				t = t +1
				len_weight_list[[t]] <- c( exp( lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$State == a & data$Source == b)$coefficients[1]),
												lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$State == a & data$Source == b)$coefficients[2])  
				nm = c(nm, paste0(a, "_", b))	
			}		
		}
	}


	for (b in unique(data$Source)){
		for(s in unique(data$Sex)){
		check = data[data$Source == b & data$Sex == s, c("Length", "Weight")]
		if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Weight)) != dim(check)[1] ){
			t = t +1
			len_weight_list[[t]] <- c( exp( lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$Source == b & data$Sex == s)$coefficients[1]),
									        lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$Source == b & data$Sex == s)$coefficients[2] )  
			nm = c(nm, paste0(b, "_", s))	
		}		
		}
	}


	for(a in unique(data$State)){
		for (b in unique(data$Source)){
			for(s in unique(data$Sex)){
			check = data[data$State == a & data$Source == b & data$Sex == s, c("Length", "Weight")]
			if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Weight)) != dim(check)[1] ){
				t = t +1
				len_weight_list[[t]] <- c( exp( lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$State == a & data$Source == b & data$Sex == s)$coefficients[1]),
										        lm(log(data$Weight) ~ log(data$Length), na.action = na.omit, subset = data$State == a & data$Source == b & data$Sex == s)$coefficients[2] )  
				nm = c(nm, paste0(a, "_", b, "_", s))	
			}		
			}
		}
	}

	names(len_weight_list) <- nm
	return(len_weight_list)
}