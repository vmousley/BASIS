require('tidyverse')
require('praise')
require('ggplot2')
require('read_xls')


# manually recoded A016, A017, and A031 to erase zeros
# load data & trials csv and view
gapdata <- read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/gap/gap_results_20200318_15_57_31.xlsx")#; View(gapdata)
gaptrials <- read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/gap/gap_trials_20200318_15_57_31.xlsx")#; View(gaptrials)

# load participant information data & clean up
bkgrddata <- read.csv("Age_Group_Gender.csv")#; View(bkgrddata)
names(bkgrddata)[1] <- "id"
names(bkgrddata)[2] <- "group"
names(bkgrddata)[3] <- "gender"
names(bkgrddata)[4] <- "days"
names(bkgrddata)[5] <- "months"
bkgrddata <- bkgrddata[-c(1),] # erase top row with strings
rownames(bkgrddata) <- seq(length=nrow(bkgrddata)) # rename rows

# find excluded p's in ff1/2/3data_raw files to merge tables
# this function doesn't work perfectly, not sure why. check all values manually
bkgrddata$id[!(bkgrddata$id %in% gapdata$ID)]
bkgrddatagap <- bkgrddata[-c(35, 38), ] 
  # gapdata missing data from A35, A38

# merge ffdata and relevant background data columns
  # add group
gapdata$group <- bkgrddatagap$group
  # add gender
gapdata$gender <- bkgrddatagap$gender
  # add age (days)
gapdata$age <- bkgrddatagap$days

# now have clean df w/ bkgrd variables attached!
praise()

# chunk by group for analyses
gapdatab <- filter(gapdata, group == "B")#; View(gapdatab)
gapdatam <- filter(gapdata, group == "M")#; View(gapdatam)

# group analyses
t.test(gapdatab$BASELINE_NumValid, gapdatam$BASELINE_NumValid) # NULL
t.test(gapdata$GAP_NumValid, gapdatam$GAP_NumValid) # NULL
t.test(gapdatab$OVERLAP_NumValid, gapdatam$OVERLAP_NumValid) # NULL
t.test(gapdatab$BASELINE_Valid, gapdatam$BASELINE_Valid) # NULL
t.test(gapdatab$GAP_Valid, gapdatam$GAP_Valid) # NULL
t.test(gapdatab$OVERLAP_Valid, gapdatam$OVERLAP_Valid) # NULL
t.test(gapdatab$BASELINE_SRT_mean, gapdatam$BASELINE_SRT_mean) # NULL
t.test(gapdatab$GAP_SRT_mean, gapdatam$GAP_SRT_mean) # NULL
t.test(gapdatab$OVERLAP_SRT_mean, gapdatam$OVERLAP_SRT_mean) # NULL
t.test(gapdatab$FAC_SRT, gapdatam$FAC_SRT) # NULL
t.test(gapdatab$DIS_SRT, gapdatam$DIS_SRT) # NULL
t.test(gapdatab$BASELINE_SRT_sd, gapdatam$BASELINE_SRT_sd) # NULL
t.test(gapdatab$GAP_SRT_sd, gapdatam$GAP_SRT_sd) # NULL
t.test(gapdatab$OVERLAP_SRT_sd, gapdatam$OVERLAP_SRT_sd) # NULL
t.test(gapdatab$FAC_SRT_sd, gapdatam$FAC_SRT_sd) # NULL
t.test(gapdatab$DIS_SRT_sd, gapdatam$DIS_SRT_sd) # NULL 
t.test(gapdatab$BASELINE_sac_gain, gapdatam$BASELINE_sac_gain) # NULL
t.test(gapdatab$GAP_sac_gain, gapdatam$GAP_sac_gain) # NULL
t.test(gapdatab$OVERLAP_sac_gain, gapdatam$OVERLAP_sac_gain) # NULL
t.test(gapdata$BASELINE_sac_gain_sd, gapdatam$BASELINE_sac_gain_sd) # NULL
t.test(gapdatab$GAP_sac_gain_sd, gapdatam$GAP_sac_gain_sd) # NULL
t.test(gapdatab$OVERLAP_sac_gain_sd, gapdatam$OVERLAP_sac_gain_sd) # NULL
t.test(gapdatab$BASELINE_sac_gain_rsd, gapdatam$BASELINE_sac_gain_rsd) # NULL
t.test(gapdatab$GAP_sac_gain_rsd, gapdatam$GAP_sac_gain_rsd) # NULL
t.test(gapdata$OVERLAP_sac_gain_rsd, gapdatam$OVERLAP_sac_gain_rsd) # NULL

# need to calculate cost of an overlap meaning overlap minus baseline 
# facilitation of the gap - the gap effect is baseline minus gap
  # if baby has good control, will have small cost of overlap
    # when there's something in the middle they don't get stuck on it 
    # gap gain will be smaller for baby who has good control of their attention
  # would predict bilinguals have a smaller gap and overlap effect (BOTH)
    # from literature, one is a lot more reliable (think overlap cost is more reliable)


