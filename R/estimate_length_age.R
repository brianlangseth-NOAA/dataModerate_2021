#' Create function to take data from multiple sources and create a 
#' single data frame that can be used for biological comparisons acroos
#' data sources
#'
#' @param list of data
#' @param grouping not implemented (options: all, sex, source, state)
#' @param linf L infinity (L2) for the user to specifiy if desired.
#' @param l0 Initial length (L1) for the user to specifiy if desired.
#' @param k von bert growth rate for the user to specifiy if desired.
#' @return data frame with estimated length-at-age
#'
#' @author Chantel Wetzel
#' @export
#'
estimate_length_age <- function(data, grouping = "all", linf = NULL, l0 = NULL,k = NULL){

	keep <- which(!is.na(data$Age))
	data <- data[keep, ]

	# Check for NA lengths from the aged fish and remove these
	keep <- which(!is.na(data$Length))
	data <- data[keep, ]

	n_sex <- unique(data$Sex)
	n_state <- unique(data$State)
	n_source <- unique(data$Source)

	# if(grouping == "all")    { calc = 1:10 }
	# if(grouping == "sex")    { calc = c(2) }
	# if(grouping == "source") { calc = c() }
	# if(grouping == "state")  { calc = c() }

	# dynamically determine reasonable parameters
	linf <- as.numeric(quantile(data$Length, 0.90))
	l0   <- ifelse(linf > 30, 10, 5)
	k    <- 0.10

	len_age_list <- list()
	nm <- NULL

	len_age_list[[1]] <- optim(c(linf, l0, k), vb_opt_fn, age = data$Age, lengths = data$Length)$par	
	

	nm <- "all"
	t <- 1
	for (a in unique(data$Sex)){
		if (sum(data$Sex == a) > 0){
			t = t + 1
			tmp = data[data$Sex == a, ]
			len_age_list[[t]] <- optim(c(linf, l0, k), vb_opt_fn, age = tmp$Age, lengths = tmp$Length)$par 
			nm = c(nm, paste0("all_", a))	
		}
	}
	
	
	for(a in unique(data$Source)){
		t = t + 1
		tmp = data[data$Source == a, ]
		len_age_list[[t]] <- optim(c(linf, l0, k), vb_opt_fn, age = tmp$Age, lengths = tmp$Length)$par 
		nm = c(nm, a)		
	}

	for(a in unique(data$State)){		
		check = data[data$State == a, c("Length", "Age")]
		if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Age)) != dim(check)[1] ){
			t = t +1
			tmp = data[data$State == a, ]
			len_age_list[[t]] <- optim(c(linf, l0, k), vb_opt_fn, age = tmp$Age, lengths = tmp$Length)$par
			nm = c(nm, a)
		}		
	}

	for(a in unique(data$State)){
		for (b in unique(data$Source)){
			check = data[data$State == a & data$Source == b, c("Length", "Age")]
			if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Age)) != dim(check)[1] ){
				t = t +1
				tmp = data[data$State == a & data$Source == b, ]
				len_age_list[[t]] <- optim(c(linf, l0, k), vb_opt_fn, age = tmp$Age, lengths = tmp$Length)$par				
				nm = c(nm, paste0(a, "_", b))	
			}		
		}
	}


	for (b in unique(data$Source)){
		for(s in unique(data$Sex)){
			check = data[data$Source == b & data$Sex == s, c("Length", "Age")]
			if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Age)) != dim(check)[1] ){
				t = t +1
				tmp = data[data$Source == b & data$Sex == s, ]
				len_age_list[[t]] <- optim(c(linf, l0, k), vb_opt_fn, age = tmp$Age, lengths = tmp$Length)$par				
				nm = c(nm, paste0(b, "_", s))	
			}		
		}
	}

	for(a in unique(data$State)){
		for(s in unique(data$Sex)){
			check = data[data$State == a & data$Sex == s, c("Length", "Age")]
			if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Age)) != dim(check)[1] ){
				t = t +1
				tmp = data[data$State == a & data$Sex == s, ]
				len_age_list[[t]] <- optim(c(linf, l0, k), vb_opt_fn, age = tmp$Age, lengths = tmp$Length)$par					
				nm = c(nm, paste0(a, "_", s))	
			}		
		}
	}

	for(a in unique(data$State)){
		for (b in unique(data$Source)){
			for(s in unique(data$Sex)){
			check = data[data$State == a & data$Source == b & data$Sex == s, c("Length", "Age")]
			if( sum(is.na(check$Length)) != dim(check)[1] & sum(is.na(check$Age)) != dim(check)[1] ){
				t = t +1
				tmp = data[data$State == a & data$Source == b & data$Sex == s, ]
				len_age_list[[t]] <- optim(c(linf, l0, k), vb_opt_fn, age = tmp$Age, lengths = tmp$Length)$par					
				nm = c(nm, paste0(a, "_", b, "_", s))	
			}		
			}
		}
	}

	names(len_age_list) <- nm
	return(len_age_list)
}