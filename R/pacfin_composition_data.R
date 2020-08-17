##################################################################################################
#
#	PacFIN Data Exploration for Copper Rockfish
# 		
#		Written by Chantel Wetzel
#
##################################################################################################

devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/PacFIN.Utilities")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/HandyCode")
library(ggplot2)
options(stringsAsFactors = TRUE)


dir = "N://Assessments/CurrentAssessments/DataModerate_2021/copper_rockfish/data/commercial_comps"
setwd(dir)
load(paste0(dir, "/PacFIN.COPP.bds.24.Jul.2020.RData"))

MasterDat = data = PacFIN.COPP.bds.24.Jul.2020

# Add default age data column in order to make code work 
# Must remove once data are re-pulled!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MasterDat$FISH_AGE_YEARS_FINAL = NA

table(data$FISH_LENGTH_TYPE)

# May want to evaluat the data by state and the sample quality code to identify special projects data
table(data$SOURCE_AGID, data$SAMPLE_QUALITY)

# Below command does not work with the July 24 pull which does not have an age data column
plotRawData(MasterDat)

# For some stupid reason the new pull has FISH_LENGTH_TYPE as TRUE or FALSE not the expected
# c("", "A", "F", "U", "T", NA).  Need to check with Wallace why these are different.

MasterDat$FISH_LENGTH_TYPE = NA # Need to remove when fixed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Pdata = cleanPacFIN(Pdata = MasterDat, 
					keep_length_type = c("", "A", "F", "U", "T", NA),
					keep_missing_lengths = FALSE,
					keep_INPFC = c("VUS","CL","VN","COL","NC","SC","EU","CP","EK","MT","PS"))

#  Removal Report
#  
#  Records in input:                  8798 
#  Records not in USINPFC             0 
#  Records not in INPFC_AREA:         351 
#  Records in bad INPFC_AREA:         0 
#  Records in badRecords list:        0 
#  Records with bad SAMPLE_TYPE       0 
#  Records with bad SAMPLE_METHOD     0 
#  Records with no SAMPLE_NO          0 
#  Records with no usable length      225 
#  Records remaining:                 8222 

checkGrade(Pdata)


# Need to determine how best to split north and south of conception !!!!!!!!!!!!!!!!!!!!!



############################################################################################################
# Length-at-age checks
#############################################################################################################
# If ages appear in the dataset - will need to evaluate length-age combinations
# Check lengths by sex
# Females first
# k = 
# Linf = 
# L0 = 
# CV1 = 
# CV2 = 
# Par = 
# 
# Pdata = checkLenAge(Pdata = Pdata, Par = Par, keepAll = TRUE, Optim = TRUE)
# remove = which(Pdata[,'length'] > Pdata[,'Lhat_high'] | Pdata[,'FISH_LENGTH'] < Pdata[,'Lhat_low'])
# badRecords = Pdata[remove,]

# Look at the flagged records relative to length-age relationship
# par(mfrow = c(1,1))
# plot(badRecords[badRecords$SEX == "F",   "age"], badRecords[badRecords$SEX == "F", "length"], type = 'p', col = 'red', ylim = c(0, 900))
# points(badRecords[badRecords$SEX == "F", "age"], badRecords[badRecords$SEX == "F", "Lhat_pred"], pch = 16, col = 'red')
# points(badRecords[badRecords$SEX == "M", "age"], badRecords[badRecords$SEX == "M", "length"], col = 'blue')
# points(badRecords[badRecords$SEX == "M", "age"], badRecords[badRecords$SEX == "M", "Lhat_pred"], pch = 16, col = 'blue')
# points(badRecords[badRecords$SEX == "U", "age"], badRecords[badRecords$SEX == "U", "length"], col = 'darkgrey')
# points(badRecords[badRecords$SEX == "U", "age"], badRecords[badRecords$SEX == "U", "Lhat_pred"], pch = 16, col = 'darkgrey')
# 
# # Look at the bad records relative to all the other data
# par(mfrow=c(3,1))
# plot(Pdata[Pdata$SEX == "F", "age"], Pdata[Pdata$SEX == "F", "length"], type = 'p', col = 'red', ylim = c(0, 900))
# points(Pdata[Pdata$SEX == "F", "age"], Pdata[Pdata$SEX == "F", "Lhat_pred"], pch = 16, col = 1)
# points(badRecords[badRecords$SEX == "F", "age"], badRecords[badRecords$SEX == "F", "length"], pch = 17, col = 1)
# 
# plot(Pdata[Pdata$SEX == "M", "age"], Pdata[Pdata$SEX == "M", "length"], type = 'p', col = 'blue',  ylim = c(0, 900))
# points(Pdata[Pdata$SEX == "M", "age"], Pdata[Pdata$SEX == "M", "Lhat_pred"], pch = 16, col = 1)
# points(badRecords[badRecords$SEX == "M", "age"], badRecords[badRecords$SEX == "M", "length"], pch = 17, col = 1)
# 
# plot(Pdata[Pdata$SEX == "U", "age"], Pdata[Pdata$SEX == "U", "length"], type = 'p', col = 'grey',  ylim = c(0, 900))
# points(Pdata[Pdata$SEX == "U", "age"], Pdata[Pdata$SEX == "U", "Lhat_pred"], pch = 16, col = 1)
# points(badRecords[badRecords$SEX == "U", "age"], badRecords[badRecords$SEX == "U", "length"], pch = 17, col = 1)
# 
# # Look at the lengths  where there are not ages 
# par(mfrow =c (1,2))
# check = Pdata$SEX == "F" & Pdata$age == -1
# plot(Pdata[check, "length"], type = 'p', col = 'red', pch = 21, ylim = c(0, max(Pdata$length, na.rm = T)))
# check =  Pdata$SEX == "M" & Pdata$age == -1
# plot(Pdata[check, "length"], type = 'p', col = 'blue', pch = 21, ylim = c(0, max(Pdata$length, na.rm = T)))
#
#################################################################################################################################

# Save the filtered data
save(Pdata, file = "Cleaned_PacFIN.PTRL.bds.26.Jun.2019.Rda")

Pdata = getGearGroup(Pdata)

# Plot the length data
plotCleaned(Pdata)




#####################################################################################################
# Early visualization of the data:
#####################################################################################################

# Evaluate the available length samples by state
pngfun(wd = getwd(), file = paste0('copper_samples_ca_pacfin.png'), h = 12, w = 12)
find = data$SOURCE_AGID == "C"
ggplot(data[find,], aes(x = length_cm)) + 
		geom_histogram() + #facet_grid(~ RECFIN_YEAR) + 
		facet_wrap(facets = c("SAMPLE_YEAR", "SOURCE_AGID"), nrow = 12, ncol = 5) +
		theme_bw() + stat_bin(bins = 60, binwidth = 2)
dev.off()

pngfun(wd = getwd(), file = paste0('copper_samples_or_pacfin.png'), h = 12, w = 12)
find = data$SOURCE_AGID == "O"
ggplot(data[find,], aes(x = length_cm)) + 
		geom_histogram() + #facet_grid(~ RECFIN_YEAR) + 
		facet_wrap(facets = c("SAMPLE_YEAR", "SOURCE_AGID"), nrow = 12, ncol = 5) +
		theme_bw() + stat_bin(bins = 60, binwidth = 2)
dev.off()

pngfun(wd = getwd(), file = paste0('copper_samples_wa_pacfin.png'), h = 12, w = 12)
find = data$SOURCE_AGID == "W"
ggplot(data[find,], aes(x = length_cm)) + 
		geom_histogram() + #facet_grid(~ RECFIN_YEAR) + 
		facet_wrap(facets = c("SAMPLE_YEAR", "SOURCE_AGID"), nrow = 12, ncol = 5) +
		theme_bw() + stat_bin(bins = 60, binwidth = 2)
dev.off()

###########################################################################################################
# Create table of samples by area and year
###########################################################################################################

temp = data[!is.na(data$length_cm) & data$SAMPLE_YEAR < 2021,]

# Once I figure out how to parse by finer area this next line should be replaced
temp$strat = temp$SOURCE_AGID

Nfish = table(temp$SAMPLE_YEAR, temp$strat)

aa = unique(temp$strat)
yy = sort(unique(temp$SAMPLE_YEAR))
Ntows = matrix(0, length(yy), length(aa))
for(y in 1:length(yy)){
	for(a in 1:length(aa)){
		ind = which(temp$SAMPLE_YEAR == yy[y] & temp$strat == aa[a])
		if(length(ind) > 0) {Ntows[y, a] = length(unique(temp$SAMPLE_NO[ind])) }
	}
}
colnames(Ntows) = aa
rownames(Ntows) = yy

samples = NULL
for (a in 1:length(aa)){
	samples = cbind(samples, Ntows[,aa[a]], Nfish[,aa[a]])
}

samples = cbind(Ntows[,"C"], Nfish[,"C"], 
			    Ntows[,"O"], Nfish[,"O"],
				Ntows[,"W"], Nfish[,"W"])

colnames(samples) = c("C_NTows", "C_Nfish", 
	 				  "O_NTows", "O_Nfish", 
	 				  "W_NTows", "W_Nfish")

write.csv(samples, file = file.path(getwd(),"PacFIN_Length_Samples.csv"), row.names = TRUE)

# Now lets do ages in PacFIN

temp = data[!is.na(data$FISH_AGE_YEARS_FINAL) & data$SAMPLE_YEAR < 2021,]
# There are no ages under the FISH AGE YEAR FINAL column