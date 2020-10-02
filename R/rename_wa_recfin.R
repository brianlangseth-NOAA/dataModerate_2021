#' Modify Washington recreational data that does not match RecFIN column labels
#' for use by data exploration tools
#'
#' @param data pass read in data provided by John Budrick for CA recreational data
#'
#' @return A data frame 
#'
#' @author Brian Langseth and Chantel Wetzel
#' @export
#'

rename_wa_recfin <- function(data){

  #Rename variables to match recfin pulls
  names(data)[which(names(data) == "species_name")] <- "SPECIES_NAME"
  names(data)[which(names(data) == "mfbds_v_sample.punch_card_area_code")] <- "RECFIN_PORT_NAME"
  names(data)[which(names(data) == "weight_gm")] <- "AGENCY_WEIGHT"
  names(data)[which(names(data) == "sample_year")] <- "RECFIN_YEAR"
  names(data)[which(names(data) == "sex_name")] <- "FISH_SEX"
  names(data)[which(names(data) == "best_age")] <- "FISH_AGE"

  data$STATE_NAME = "WASHINGTON"
  data$RECFIN_LENGTH_MM = data$AGENCY_LENGTH = data$fish_length_cm * 10
  data$IS_RETAINED = "RETAINED"

  # Change weight from gm to kilograms
  data$AGENCY_WEIGHT = data$AGENCY_WEIGHT / 1000

  data$FISH_SEX[data$FISH_SEX == "Female"] = "F"
  data$FISH_SEX[data$FISH_SEX == "Male"] = "M"
  data$FISH_SEX[!data$FISH_SEX %in% c('F', 'M')] = 'U'
  

  return(data)
}