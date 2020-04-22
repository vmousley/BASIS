library(tidyverse)
library(praise)
library(ggplot2)
library(readxl)

# manually recoded A016, A017, and A031 to erase zeros
# load data & trials
ccdata <- read.csv("/Volumes/leap/EVELYNE DATA/EM_Bilingual_CleanData/BB_cogcontrol_20190719T125418.csv")#; View(ccdata)
cctrials <- read.csv("/Volumes/leap/EVELYNE DATA/EM_Bilingual_CleanData/BB_cogcontrol_trials_20190719T125418.csv")#; View(ccdata_v)

# load participant information data & clean up
bkgrddata <- read.csv("/Volumes/leap/EVELYNE DATA/EM_Bilingual_CleanData/Age_Group_Gender.csv")#; View(bkgrddata)
names(bkgrddata)[1] <- "id"
names(bkgrddata)[2] <- "group"
names(bkgrddata)[3] <- "gender"
names(bkgrddata)[4] <- "days"
names(bkgrddata)[5] <- "months"
bkgrddata <- bkgrddata[-c(1),] # erase top row with strings
rownames(bkgrddata) <- seq(length=nrow(bkgrddata)) # rename rows

# find excluded p's in ccdata file to merge tables
# this function doesn't work perfectly, not sure why. check all values manually

# ccdata missing data from A35, A38

# merge ccdata and relevant background data columns
  # add group
ccdata$group <- bkgrddatacc$group
  # add gender 
ccdata$gender <- bkgrddatacc$gender
  # add age (day)
ccdata$age <- bkgrddatacc$days

# now have clean ccdf with bkgrd data! 
praise()

# chunk by group for analyses
ccdatab <- filter(ccdata, group == "B") 
ccdatam <- filter(ccdata, group == "M")

### GROUP ANALYSES
t.test(ccdatab$NumTrialsToCrit_Learn, ccdatam$NumTrialsToCrit_Learn) # NULL
t.test(ccdatab$NumTrialsToCrit_Rev, ccdatam$NumTrialsToCrit_Rev) # NULL
t.test(ccdatab$NumAntiSac_Learn, ccdatam$NumAntiSac_Learn) # NULL
t.test(ccdatab$NumAntiSac_Rev, ccdatam$NumAntiSac_Rev) # NULL
t.test(ccdatab$PropCorrectAntiSac_Learn, ccdatam$PropCorrectAntiSac_Learn) # NULL
t.test(ccdatab$PropCorrectAntiSac_Rev, ccdatam$PropCorrectAntiSac_Rev) # NULL
t.test(ccdatab$RTCorrectAntiSacMean_Learn, ccdatam$RTCorrectAntiSacMean_Learn) # NULL
t.test(ccdatab$RTCorrectAntiSacSD_Learn, ccdatam$RTCorrectAntiSacSD_Learn) # p = 0.06
t.test(ccdatab$RTCorrectAntiSacMean_Rev, ccdatam$RTCorrectAntiSacMean_Rev) # p = 0.06
t.test(ccdatab$RTCorrectAntiSacSD_Rev, ccdatam$RTCorrectAntiSacSD_Rev) # p = 0.08
t.test(ccdatab$RTSlope_Learn, ccdatam$RTSlope_Learn) # NULL
t.test(ccdatab$RTSlope_Rev, ccdatam$RTSlope_Rev) # NULL
t.test(ccdatab$PropLost, ccdatam$PropLost) # NULL
t.test(ccdatab$Accuracy, ccdatam$Accuracy) # NULL
t.test(ccdata$NumAntiSac_All, ccdatam$NumAntiSac_All) # NULL

# NumCorrectAntiSac_Rev of interest
  # investigate after removing bad trials

# RTCorrectAntiSacMean_Rev of interest
  # predict that bilinguals would have shorter RT in reversal (quicker adaptation)
  # borderline significant, would be with cleaning data more
  # supports hyp present in literature

### Interpreting significant group differences ###
# t.test(ccdatab$RTCorrectAntiSacSD_Rev, ccdatam$RTCorrectAntiSacMean_Rev)
