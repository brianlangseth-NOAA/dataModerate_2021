########################################
#Script to assess biological data patterns 
#for length, age, and weight. Also to output
#tables for number of data entries
#
#Author: Brian Langseth, Chantel Wetzel
#
#Updated: 9/18/2020
#########################################

#devtools::install_github("brianlangseth-NOAA/dataModerate_2021")
library(dataModerate2021)
library(nwfscSurvey)
library(PacFIN.Utilities)
library(ggplot2)

species = "quillback"
species = "squarespot"

if(species == "quillback"){
  dir = "//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/Quillback_Rockfish"
  pacfin_abbr = "QLBK"
  hkl_name = "Quillback Rockfish"
  recfin_name = "QUILLBACK ROCKFISH"
}
if(species == "squarespot"){
  dir = "//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/Squarespot_Rockfish"
  pacfin_abbr = "SQRS"
  hkl_name = "Squarespot Rockfish"
  recfin_name = "SQUARESPOT ROCKFISH"
}

source("U:\\Stock assessments\\dataModerate_2021\\R\\pngfun.R")
source("U:\\Stock assessments\\dataModerate_2021\\R\\rich_colors_short.R")
source("U:\\Stock assessments\\dataModerate_2021\\R\\VB_functions.R")
source("U:\\Stock assessments\\dataModerate_2021\\R\\estimate_length_age.R")
source("U:\\Stock assessments\\dataModerate_2021\\R\\length_by_depth_plot.R")
source("U:\\Stock assessments\\dataModerate_2021\\R\\length_age_plot.R")
source("U:\\Stock assessments\\dataModerate_2021\\R\\compare_length_cca.R")

############################################################################################
#	Load Data
############################################################################################

##PacFIN
load(file.path(dir, "data", "PacFIN BDS", paste0("PacFIN.",pacfin_abbr,".bds.13.Aug.2020.RData")))
pacfin 	 = eval(as.name(paste0("PacFIN.",pacfin_abbr,".bds.13.Aug.2020")))

##RecFIN
#California
ca_recfin = rename_budrick_recfin(read.csv("//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/Data_From_States/ca/ca_rec_lengths_2004_2020_updated.csv", header=T, na.strings = "-"))
#Washignton
if(species == "quillback"){
  wa_recfin = rename_wa_recfin(read.csv(paste0("//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/Data_From_States/wa/wa_rec_bds_",species,".csv"), header = T, na.strings = "-"))
}
if(species == "squarespot") { wa_recfin = NULL }
#Oregon
or_recfin = NULL
#Combine
rec_fields = c("RECFIN_YEAR", "STATE_NAME", "FISH_SEX", "RECFIN_LENGTH_MM", "AGENCY_WEIGHT", "FISH_AGE", "IS_RETAINED", "SPECIES_NAME", "RECFIN_PORT_NAME")
recfin = rbind(wa_recfin[,which(names(wa_recfin) %in% rec_fields)], or_recfin[,which(names(or_recfin) %in% rec_fields)], ca_recfin[,which(names(ca_recfin) %in% rec_fields)])
#Use only desired species
recfin = recfin[recfin$SPECIES_NAME == recfin_name, ]

# ##Research - CONTINUE
# if(species == "quillback"){
#   wa_research = read.csv(paste0("//nwcfile/FRAM/Assessments/CurrentAssessments/DataModerate_2021/Data_From_States/wa/wa_research_bds_",species,".csv"),header=T)
#   
# }
# if(species == "squarespot") { wa_research = NULL }
# 
# 
# ##MRFSS - CONTINUE


  
##Hook and Line survey
hkl = read.csv(file.path(dir, "data", "Hook Line Data", "qryGrandUnifiedThru2019_06182020.csv"))
sub_hkl = hkl[hkl$COMNAME == hkl_name, ]


##Combo Survey
load(file.path(dir, "data", "Trawl Survey Bio", "Bio_All_NWFSC.Combo_2020-07-30.rda"))
combo = Data
rm(Data)


##Triennial Survey
load(file.path(dir, "data", "Trawl Survey Bio", "Bio_All_Triennial_2020-07-30.rda"))
trien = Data
rm(Data)

input = list()
input[[1]] = rename_survey_data(combo)
input[[2]] = rename_pacfin(pacfin)
input[[3]] = rename_recfin(recfin, area_grouping = list(c("CHANNEL", "SOUTH"), c("BAY AREA", "WINE", "CENTRAL", "REDWOOD", "NOT KNOWN")),
                           area_names = c("south_pt_concep", "north_pt_concep"))
input[[4]] = rename_hook_and_line(sub_hkl)
input[[5]] = rename_survey_data(trien[[1]])

############################################################################################
#	Create data frame with all the input data
############################################################################################
out = create_data_frame(data_list = input)


############################################################################################
#	Summarize all of the input data
############################################################################################
summarize_data(dir = paste0(dir,"/data/plots"), data = out)


############################################################################################
#	Plot length-at-weight data by source and year
############################################################################################
lw_ests <- estimate_length_weight(data = out, grouping = "all")
length_weight_plot(dir = file.path(dir, "data"), splits = NA, data = out, nm_append = NULL, ests = lw_ests)


############################################################################################
#	Plot length-at-age data by source and year
############################################################################################
la_ests <- estimate_length_age(data = out, grouping = "all")
length_age_plot(dir = file.path(dir, "data"), splits = NA, data = out, nm_append = NULL, ests = la_ests)


############################################################################################
#	Plot length by depth plot by source and year
############################################################################################
length_by_depth_plot(dir = file.path(dir, "data"), data = out, xlim = NULL, ylim = NULL)


############################################################################################
#	Plot length frequency plots by source and year
############################################################################################
length_freq_plot(dir = file.path(dir, "data"), data = out, xlim = NULL, ylim = NULL)


############################################################################################
#	Plot length frequency plots in and out of Cowcod Conservation Areas from Hook and Line survey data only
############################################################################################
compare_length_cca(dir = file.path(dir, "data"), data = out, file = "hkl_cca_comparison", xlim = NULL, ylim = NULL)


