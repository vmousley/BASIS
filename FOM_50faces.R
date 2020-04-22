### Analysis code for 50 faces
# Last updated 26 March 2020
# "Face scanning amongst mono & bilinguals"

require("praise")
require("ggplot2")
require("readxl")
require("reshape2")
require("tidyverse")
require("dplyr")
require("lme4")
require("lmerTest")
require("cowplot")
require("papaja")
require("coda")
require("MCMCvis")
require("ggpubr")
require("naniar")
require("e1071")
require("broom")
require("cowplot")
require("gridExtra")
require("purrr")
require("extrafont")
require("showtext")

# VICTORIA DATA -----------------------------------------------------------
vm1 <- data.frame(read_excel("/Users/victoriamousley/Desktop/MATLAB/lm_analysis/50faces/50faces_wide_faces 1.m4v_20200228T214125.xlsx"))
vm2 <- data.frame(read_excel("/Users/victoriamousley/Desktop/MATLAB/lm_analysis/50faces/50faces_wide_faces 2.m4v_20200228T214341.xlsx"))
vm3 <- data.frame(read_excel("/Users/victoriamousley/Desktop/MATLAB/lm_analysis/50faces/50faces_wide_faces 3.m4v_20200228T214558.xlsx"))

vmdata <- rbind(vm1, vm2, vm3) # merge all 3 files' rows
demodata <- data.frame(read_excel("/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx"))
names(demodata)[1] <- "id"
vmdata <- arrange(vmdata, id) # sort rows so each p has 3 rows in a row 

# EXCLUSION #1 - erase trials with < 2 seconds total looking 
# 1) TRIALS > 2 SECONDS
vmdata$time_total <- NA # creating column of total trial length (per trial)
for (row in 1:nrow(vmdata)){ # for a range of numbers between 1 and # of rows in df, iterate through each number 
  vmdata$time_total[row] = sum(vmdata$body_timeInAOI[row], vmdata$outer_face_timeInAOI[row],  # sum the values of each AOI in each row
                               vmdata$face_timeInAOI[row],vmdata$bg_people_timeInAOI[row])}

vmdata2sec <- subset(vmdata, vmdata$time_total >= 2) # data frame of participants looking at least 2 seconds

# EXCLUSION #2 - erase participants with 0 or 1 trials
# seeing which participants have 1 or fewer trials
idFreq <- data.frame(table(vmdata2sec$id)) # take list of df1 id, count distinct ids, show how many trials each participant has
oneTrialList <- vector() # make empty vector called oneTrialList 
onePlusTrialList <- vector() # make empty vector called onePlusTrialList 
for (row in 1:nrow(idFreq)) { # for a range of numbers between 1 and # of rows in a, iterate through each number
  id <- toString(idFreq[row, "Var1"]) # look @ Var1 values for each row in object a and assign them to object id as a string 
  freq  <- idFreq[row, "Freq"] # look @ freq values for each row in object a and assign them to object freq 
  if(freq <= 1 ) { # if the freq < 1
    oneTrialList <- c(oneTrialList, id) # then put ID number in oneTrialList
    print(paste(id, "is less than 1")) # then tell us which ID < 1
  } else { # if freq > 1
    onePlusTrialList <- c(onePlusTrialList, id) # then put ID number in onePlusTrialList
    next # then go to next one
  }
}

length(oneTrialList) # number of IDs with 1 or fewer trials
length(onePlusTrialList) # number of IDs with more than 1 trials

finalDf <- vmdata2sec[!vmdata2sec$id %in% oneTrialList, ] # if df1 IDs are in oneTrialList, do not include those rows in mydf
finalDf <- finalDf %>% arrange(id) # group rows by ID 

test <- data.frame(finalDf[,c(1, 64)])
test$firstSampSec <- test$face_firstSamp/300

# CALCULATING FOM -------------------------


