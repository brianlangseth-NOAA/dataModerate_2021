#' Modify California recreational data that does not match RecFIN column labels
#' for use by data exploration tools
#'
#' @param data pass read in data provided by John Budrick for CA recreational data
#'
#' @return A data frame 
#'
#' @author Brian Langseth and Chantel Wetzel
#' @export
#'

rename_budrick_recfin <- function(data){

  #Rename variables to match recfin pulls
  names(data)[which(names(data) == "Species.Name")] <- "SPECIES_NAME"
  names(data)[which(names(data) == "RecFIN.Port.Name")] <- "RECFIN_PORT_NAME"
  names(data)[which(names(data) == "Agency.Length")] <- "AGENCY_LENGTH"
  names(data)[which(names(data) == "Agency.Length.Units")] <- "AGENCY_LENGTH_UNITS"
  names(data)[which(names(data) == "RecFIN.Length.MM")] <- "RECFIN_LENGTH_MM"
  names(data)[which(names(data) == "Agency.Weight")] <- "AGENCY_WEIGHT"
  names(data)[which(names(data) == "Agency.Weight.Units")] <- "AGENCY_WEIGHT_UNITS"
  names(data)[which(names(data) == "RecFIN.Imputed.Weight.KG")] <- "RECFIN_IMPUTED_WEIGHT_KG"
  names(data)[which(names(data) == "RecFIN.Year")] <- "RECFIN_YEAR"
  names(data)[which(names(data) == "State.Name")] <- "STATE_NAME"
  names(data)[which(names(data) == "Fish.Sex")] <- "FISH_SEX"
  names(data)[which(names(data) == "Is.Retained")] <- "IS_RETAINED"
  data$FISH_AGE <- NA

  return(data)
}