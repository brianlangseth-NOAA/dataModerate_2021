
pepr = "Chantel"
pepr = "Brian"

species = "quillback"
species = "squarespot"

if(pepr == "Chantel") { 
  dir = "//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/copper_rockfish" 
  
  devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
  devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/PacFIN.Utilities")
  devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/HandyCode")
  
  source("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021/R/create_data_frame.R")
  source("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021/R/length_weight_plot.R")
  source("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021/R/estimate_length_weight.R")
}

if(pepr == "Brian"){
  if(species == "quillback"){
    dir = "//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/Quillback_Rockfish"
    pacfin_abbr = "QLBK"
    species_name = "QUILLBACK ROCKFISH"
  }
  
  library(nwfscSurvey)
  library(PacFIN.Utilities)
  
  source("U:\\Stock assessments\\dataModerate_2021\\R\\create_data_frame.R")
  source("U:\\Stock assessments\\dataModerate_2021\\R\\length_weight_plot.R")
  source("U:\\Stock assessments\\dataModerate_2021\\R\\estimate_length_weight.R")
}


############################################################################################
#	Load Data
############################################################################################

##PacFIN
if(pepr == "Chantel") load(file.path(dir, "data", "commercial_comps", "PacFIN.COPP.bds.13.Aug.2020.RData"))
if(pepr == "Brian") load(file.path(dir, "data", "PacFIN BDS", "PacFIN.QLBK.bds.13.Aug.2020.RData"))
pacfin 	 = PacFIN.QLBK.bds.13.Aug.2020
Pdata = cleanPacFIN(Pdata = pacfin, 
					keep_length_type = c("", "A", "F", "U", "T", NA),
					keep_missing_lengths = FALSE,
					keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT","PS"))

##RecFIN
if(pepr == "Chantel") recfin = read.csv(file.path(dir, "data", "recreational_comps", "SD001-WASHINGTON-OREGON-CALIFORNIA-1980-2019.csv"))
if(pepr == "Brian") {
  recfin = load(file.path(dir, "data", "RecFIN Sample Data", "wetzel_comp_bio_age_inventory_20200117.RData"))
  recfin = rbind(wa,or,ca)
  recfin = recfin[recfin$SPECIES_NAME == "QUILLBACK ROCKFISH", ]
}
  
##Hook and Line survey
if(pepr == "Chantel") {
  hkl = read.csv(file.path(dir, "data", "survey_comps", "qryGrandUnifiedThru2019_06182020.csv"))
  sub_hkl = hkl[hkl$COMNAME == 'Copper Rockfish', ]
}
if(pepr == "Brian") {
  hkl = read.csv(file.path(dir, "data", "NWFSC Hook & Line", "qryGrandUnifiedThru2019_06182020.csv"))
  sub_hkl = hkl[hkl$COMNAME == 'Quillback Rockfish', ]
}

##Combo Survey
if(pepr == "Chantel") load(file.path(dir, "data", "survey_comps", "Bio_All_NWFSC.Combo_2020-08-14.rda"))
if(pepr == "Brian") {
  load(file.path(dir, "data", "Trawl Survey Bio", "Bio_All_NWFSC.Combo_2020-07-30.rda"))
  combo = Data
  rm(Data)
}

##Triennial Survey
if(pepr == "Brian") {
  load(file.path(dir, "data", "Trawl Survey Bio", "Bio_All_Triennial_2020-07-30.rda"))
  trien = Data
  rm(Data)
}

input = list()
input[[1]] = combo
input[[2]] = Pdata
input[[3]] = recfin
input[[4]] = sub_hkl
input[[5]] = trien

############################################################################################
#	Create data frame with all the input data
############################################################################################
out = create_data_frame(data_list = input, areas = NA)

############################################################################################
#	Plot length-at-weight data by source and year
############################################################################################
if(pepr == "Chantel") length_weight_plot(dir = file.path(dir, "data", "biology"), data = out)
if(pepr == "Brian") length_weight_plot(dir = file.path(dir, "data"), data = out, haveHandy = FALSE)



############################################################################################
# Estimate Growth Using only Survey data
############################################################################################
survey_dat <- out[out$Source %in% c("nwfsc_wcgbts", "nwfsc_hkl"),]
est_growth <- estimate_length_weight(data = survey_dat)
save(est_growth, file = file.path(dir, "data", "biology", "growth_estimates_survey.Rdat"))

length_weight_plot(dir = file.path(dir, "data", "biology"),
				   nm_append = "Survey", data = survey_dat)


