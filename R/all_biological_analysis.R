

devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/PacFIN.Utilities")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/HandyCode")

source("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021/R/create_data_frame.R")
source("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021/R/length_weight_plot.R")
source("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021/R/estimate_length_weight.R")

dir = "//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/copper_rockfish"


############################################################################################
#	Load Data
############################################################################################
load(file.path(dir, "data", "commercial_comps", "PacFIN.COPP.bds.13.Aug.2020.RData"))
pacfin 	 = PacFIN.COPP.bds.13.Aug.2020
Pdata = cleanPacFIN(Pdata = pacfin, 
					keep_length_type = c("", "A", "F", "U", "T", NA),
					keep_missing_lengths = FALSE,
					keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT","PS"))


recfin = read.csv(file.path(dir, "data", "recreational_comps", "SD001-WASHINGTON-OREGON-CALIFORNIA-1980-2019.csv"))

hkl = read.csv(file.path(dir, "data", "survey_comps", "qryGrandUnifiedThru2019_06182020.csv"))
sub_hkl = hkl[hkl$COMNAME == 'Copper Rockfish', ]

load(file.path(dir, "data", "survey_comps", "Bio_All_NWFSC.Combo_2020-08-14.rda"))

input = list()
input[[1]] = bio
input[[2]] = Pdata
input[[3]] = recfin
input[[4]] = sub_hkl

############################################################################################
#	Create data frame with all the input data
############################################################################################
out = create_data_frame(data_list = input, areas = NA)

############################################################################################
#	Plot length-at-weight data by source and year
############################################################################################
length_weight_plot(dir = file.path(dir, "data", "biology"), data = out)


############################################################################################
# Estimate Growth Using only Survey data
############################################################################################
survey_dat <- out[out$Source %in% c("nwfsc_wcgbts", "nwfsc_hkl"),]
est_growth <- estimate_length_weight(data = survey_dat)
save(est_growth, file = file.path(dir, "data", "biology", "growth_estimates_survey.Rdat"))

length_weight_plot(dir = file.path(dir, "data", "biology"),
				   nm_append = "Survey", data = survey_dat)


