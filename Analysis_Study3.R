### Analysis code for 50 faces
# Last updated 17 Jan 2020
# "Face scanning amongst mono & bilinguals"

# EVELYNE DATA ------------------------------------------------------------
require("tidyverse")
require("praise")
require("ggplot2")
require("readxl")
require("dplyr")
require("lme4")
require("lmerTest")

# load 50 faces data sets and fix ordering issues, view files manually
em1 <- data.frame(read_excel("/Volumes/leap/EVELYNE DATA/EM_Bilingual_RawData/10 months/lm_analysis/50faces/50faces_wide_faces 1.m4v_20200124T221154.xlsx"))
em2 <- data.frame(read_excel("/Volumes/leap/EVELYNE DATA/EM_Bilingual_RawData/10 months/lm_analysis/50faces/50faces_wide_faces 2.m4v_20200124T221421.xlsx"))
em3 <- data.frame(read_excel("/Volumes/leap/EVELYNE DATA/EM_Bilingual_RawData/10 months/lm_analysis/50faces/50faces_wide_faces 3.m4v_20200124T221700.xlsx"))

# merge all 3 csvs to 1 big table 
emdata <- rbind(em1, em2, em3) # merge all 3 files' rows
emdata <- arrange(emdata, id) # sort rows so each p has 3 rows in a row 

## explanation of variables
# "samplesInAOI: number samples in each AOI"
# "propInAOI: proportion of samples in each AOI out of total valid samples"
# "timeInAOI: time (secs) in AOI"
# "firstSamp: index of first sample in AOI (not useful)"
# "firstTimeS: time (secs) of first sample in AOI"
# "numLooks: number of looks to each AOI"
# "meanLook: mean look duration of looks to AOI"
# "peakLook: peak look"
# "minLook: minimum look"
# "ratioInAOI: prop of samples in each AOI out of all samples in any AOI"

# load participant information data & clean up
bgdata <- read.csv("/Volumes/leap/EVELYNE DATA/EM_Bilingual_CleanData/Age_Group_Gender.csv")#; View(bkgrddata)
names(bgdata)[1] <- "id"
names(bgdata)[2] <- "group"
names(bgdata)[3] <- "gender"
names(bgdata)[4] <- "days"
names(bgdata)[5] <- "months"
rownames(bgdata) <- seq(length=nrow(bgdata)) 
bgdata$id <- gsub('\\s+', '', bgdata$id) # removes trailing white space in ID values
emdataMerge <- merge(emdata, bgdata, by.x = 'id', by.y = 'id')

# # visualise EM's participants' ages
# plot(bkgrddata$months)
# bkgrddata$days <- as.numeric(as.character(bkgrddata$days))
# bkgrddata$agem <- floor(bkgrddata$days*0.0328767); bkgrddata$agem
# ages <- table(bkgrddata$agem); ages 

# EXCLUSION #1 - erase trials with < 2 seconds total looking 

# 1) TRIALS > 2 SECONDS
emdataMerge$time_total <- NA # creating column of total trial length (per trial)
for (row in 1:nrow(emdataMerge)){ # for a range of numbers between 1 and # of rows in df, iterate through each number 
  emdataMerge$time_total[row] = sum(emdataMerge$body_timeInAOI[row], emdataMerge$outer_face_timeInAOI[row],
                                    emdataMerge$face_timeInAOI[row],emdataMerge$bg_people_timeInAOI[row])}
                                  #  sum the values of each AOI in each row

emdataMerge2sec <- subset(emdataMerge, emdataMerge$time_total >= 2) # data frame of participants looking at least 2 seconds

# EXCLUSION #2 - erase participants with 0 or 1 trials (0 are auto excluded tho)
# seeing which participants have 1 or fewer trials
idFreq <- data.frame(table(emdataMerge2sec$id)) # take list of df1 id, count distinct ids, show how many trials each participant has
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

finalDf <- emdataMerge2sec[!emdataMerge2sec$id %in% oneTrialList, ] # if df1 IDs are in oneTrialList, do not include those rows in mydf
finalDf <- finalDf %>% arrange(id) # group rows by ID 

# EM: N = 32

# Group Analyses ----------------------------------------------------------
# bilinguals will look at the mouth more than monolinguals
finalIds = unique(finalDf$id) # IDs in the finalDf
mouthPropAvList <- vector() # empty vector to hold 'mouth_propInAOI' average
mouthLTList <- vector() # empty vector to hold 'mouth_timeInAOI' average
faceLTAvList <- vector() # empty vector to hold 'face_timeInAOI' average
eyesLTAvList <- vector() # empty vector to hold 'eyes_timeInAOI' average
e2fList <- vector() # empty vector to hold eyes-to-face ratio
m2fList <- vector() # empty vector to hold mouth-to-face ratio

for (x in finalIds){
  dfLoop <- finalDf[which(finalDf$id %in% c(x)),]
  
  mouthPropAv <- mean(dfLoop$mouth_propInAOI, na.rm = TRUE) # mean of 'mouth_propInAOI', ignore empty
  mouthPropAvList <- c(mouthPropAvList, mouthPropAv) # put mean value in an ordered list
  
  mouthLTAv <- mean(dfLoop$mouth_timeInAOI, na.rm = TRUE) # mean of 'mouth_timeInAOI', ignore empty
  mouthLTList <- c(mouthLTList, mouthLTAv) # put mean value in an ordered list
  
  faceLTAv <- mean(dfLoop$face_timeInAOI, na.rm = TRUE) # mean of 'face_timeInAOI', ignore empty
  faceLTAvList <- c(faceLTAvList, faceLTAv) # put mean value in an ordered list
  
  eyesLTAv <- mean(dfLoop$eyes_timeInAOI, na.rm = TRUE) # mean of 'eyes_timeInAOI', ignore empty
  eyesLTAvList <- c(eyesLTAvList, eyesLTAv) # put mean value in an ordered list
  
  e2f <- eyesLTAv/faceLTAv
  e2fList <- c(e2fList, e2f) # put ratio in an ordered list
    
  m2f <- mouthLTAv/faceLTAv
  m2fList <- c(m2fList, m2f) # put ratio in an ordered list
  
}

analysisDf <- data.frame(finalIds, mouthPropAvList, mouthLTList, faceLTAvList, eyesLTAvList, e2fList, m2fList) # create df with average and ID
analysisDf <- merge(analysisDf, bgdata, by.x = 'finalIds', by.y = 'id') # merge to include demographic data

bMouthProp <- analysisDf[which(analysisDf$group %in% c('B')),]$mouthPropAvList
mMouthProp <- analysisDf[which(analysisDf$group %in% c('M')),]$mouthPropAvList

t.test(bMouthProp, mMouthProp) # 7 - 10 month old monolingual and bilingual infants do not spend a significantly
# different proportion of the time looking at the mouth

bMouthLT <- analysisDf[which(analysisDf$group %in% c('B')),]$mouthLTList
mMouthLT <- analysisDf[which(analysisDf$group %in% c('M')),]$mouthLTList

t.test(bMouthLT, mMouthLT) # 15 - 18 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

# A 3 (AOI: mouth vs eyes vs other) x 2 (Group: monolingual vs bilingual)

##########################

# Exploratory Analyses ----------------------------------------------------
## monolinguals & bilinguals e2f ratio - group diffS?
be2f <- analysisDf[which(analysisDf$group %in% c('B')),]$e2fList
me2f <- analysisDf[which(analysisDf$group %in% c('M')),]$e2fList
t.test(be2f, me2f)

## monolinguals & bilinguals m2f ratio - group diffS?
bm2f <- analysisDf[which(analysisDf$group %in% c('B')),]$m2fList
mm2f <- analysisDf[which(analysisDf$group %in% c('M')),]$m2fList
t.test(be2f, me2f)

## trajectories over time
  # mouth-to-face ratio with age
analysisDf$days <- as.numeric(as.character(analysisDf$days))
mouth2faceplot <- ggplot(analysisDf, aes(days, m2fList, colour = group)) +
  geom_smooth(method="lm", se = F, size = 0.5) +
  geom_point() +
  theme_bw() +
  ggtitle('Mouth-to-face ratio among monolingual and bilingual 7- to 10-month-old infants') +
  xlab('Age (days)') +
  ylab('Mouth-to-face ratio'); mouth2faceplot


  # mouth-to-face ratio with age
eyes2faceplot <- ggplot(analysisDf, aes(days, e2fList, colour = group)) +
  geom_smooth(method="lm", se = F, size = 0.5) +
  geom_point() +
  theme_bw() +
  ggtitle('Eyes-to-face ratio among monolingual and bilingual 7- to 10-month-old infants') +
  xlab('Age (days)') +
  ylab('Eyes-to-face ratio'); eyes2faceplot







# VICTORIA DATA -----------------------------------------------------------
vm1 <- data.frame(read_excel("/Volumes/leap/MATLAB data/lm_preproc/lm_analysis/50faces/50faces_wide_faces 1.m4v_20200124T212421.xlsx"))
vm2 <- data.frame(read_excel("/Volumes/leap/MATLAB data/lm_preproc/lm_analysis/50faces/50faces_wide_faces 2.m4v_20200124T212624.xlsx"))
vm3 <- data.frame(read_excel("/Volumes/leap/MATLAB data/lm_preproc/lm_analysis/50faces/50faces_wide_faces 3.m4v_20200124T212849.xlsx"))

vmdata <- rbind(vm1, vm2, vm3) # merge all 3 files' rows
demodata <- data.frame(read_excel("/Volumes/leap/Behavioural/BehaviouralData.xlsx"))
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

# VM: N = 44

# Group Analyses ----------------------------------------------------------
# bilinguals will look at the mouth more than monolinguals
finalIds = unique(finalDf$id) # IDs in the finalDf
mouthPropAvList <- vector() # empty vector to hold 'mouth_propInAOI' average
mouthLTList <- vector() # empty vector to hold 'mouth_timeInAOI' average

for (x in finalIds){
  dfLoop <- finalDf[which(finalDf$id %in% c(x)),]
  
  mouthPropAv <- mean(dfLoop$mouth_propInAOI, na.rm = TRUE) # mean of 'mouth_propInAOI', ignore empty
  mouthPropAvList <- c(mouthPropAvList, mouthPropAv) # put mean value in an ordered list
  
  mouthLTAv <- mean(dfLoop$mouth_timeInAOI, na.rm = TRUE) # mean of 'mouth_timeInAOI', ignore empty
  mouthLTList <- c(mouthLTList, mouthLTAv) # put mean value in an ordered list
  m2f <- 
  e2f <- 
}

analysisDf <- data.frame(finalIds, mouthPropAvList, mouthLTList) # create df with average and ID
analysisDf <- merge(analysisDf, demodata, by.x = 'finalIds', by.y = 'id') # merge to include demographic data

analysisDf <- arrange(analysisDf, group)

bMouthProp <- analysisDf[which(analysisDf$group %in% c('B')),]$mouthPropAvList
mMouthProp <- analysisDf[which(analysisDf$group %in% c('M')),]$mouthPropAvList

t.test(bMouthProp, mMouthProp) # not sig

bMouthLT <- analysisDf[which(analysisDf$group %in% c('B')),]$mouthLTList
mMouthLT <- analysisDf[which(analysisDf$group %in% c('M')),]$mouthLTList

t.test(bMouthLT, mMouthLT) # not sig

# AOI-to-face ratio (e.g. total mouth looking-time / total face looking-time)

  # eyes-to-face ratio in monolinguals with age
  # eyes-to-face ratio in bilinguals with age

  # mouth-to-face ratio in monolinguals with age
  # mouth-to-face ratio in bilinguals with age












# Graveyard ---------------------------------------------------------------
# create eyes data frame for each AOI
# eyes

eyesdf <- dplyr::select(finalDf, id, group, days, 
                        eyes_triggered, 
                        eyes_samplesInAOI,      
                        eyes_propInAOI,     
                        eyes_timeInAOI,       
                        eyes_firstSamp,         
                        eyes_firstTimeS,        
                        eyes_numLooks,          
                        eyes_meanLook,          
                        eyes_peakLook,          
                        eyes_minLook,          
                        eyes_ratioInAOI)
eyesdf <- na.omit(finalDf) # omit empty trials

# nose
nosedf <- dplyr::select(finalDf, id, group, days,
                        nose_triggered,       
                        nose_samplesInAOI,      
                        nose_propInAOI,         
                        nose_timeInAOI,        
                        nose_firstSamp,        
                        nose_firstTimeS,        
                        nose_numLooks,          
                        nose_meanLook,          
                        nose_peakLook,          
                        nose_minLook,           
                        nose_ratioInAOI)
nosedf <- na.omit(finalDf) # omit empty trials

# mouth
mouthdf <- dplyr::select(finalDf, id, group, days,
                         mouth_triggered,        
                         mouth_samplesInAOI,    
                         mouth_propInAOI,     
                         mouth_timeInAOI,        
                         mouth_firstSamp,        
                         mouth_firstTimeS,       
                         mouth_numLooks,       
                         mouth_meanLook,         
                         mouth_peakLook,         
                         mouth_minLook,         
                         mouth_ratioInAOI)
mouthdf <- na.omit(finalDf) # omit empty trials

# faces (general)
facedf <- dplyr::select(finalDf, id, group, days,
                        face_triggered,         
                        face_samplesInAOI,    
                        face_propInAOI,     
                        face_timeInAOI,        
                        face_firstSamp,         
                        face_firstTimeS,        
                        face_numLooks,        
                        face_meanLook,          
                        face_peakLook,          
                        face_minLook, 
                        face_ratioInAOI)
facedf <- na.omit(finalDf)

# body
bodydf <- dplyr::select(finalDf, id, group, days,
                        body_triggered,      
                        body_samplesInAOI,      
                        body_propInAOI,      
                        body_timeInAOI,         
                        body_firstSamp,         
                        body_firstTimeS,        
                        body_numLooks,        
                        body_meanLook,          
                        body_peakLook,          
                        body_minLook,          
                        body_ratioInAOI)
bodydf <- na.omit(finalDf) # omit empty trials

# now have clean dfs body, eyes, face, mouth, & nose!
praise()

# chunk by group for analyses & combine for averages
#  body
bbody1 <- filter(bd1, group == "B") 
bbody2 <- filter(bd2, group == "B") 
bbody3 <- filter(bd3, group == "B")
bbody <- bind_rows(bbody1, bbody2)
bbody <- bind_rows(bbody, bbody3)
bbody <- arrange(bbody, id)#; View(bbody)

mbody1 <- filter(bd1, group == "M")
mbody2 <- filter(bd2, group == "M")
mbody3 <- filter(bd3, group == "M")
mbody <- bind_rows(mbody1, mbody2)
mbody <- bind_rows(mbody, mbody3)
mbody <- arrange(mbody, id)#; View(mbody)

# eyes
eyesdf <- filter(eyesdf, group == "B") 
beyes <- bind_rows(beyes1, beyes2)
beyes <- bind_rows(beyes, beyes3)
beyes <- arrange(beyes, id)#; View(beyes)

meyes1 <- filter(ed1, group == "M")
meyes2 <- filter(ed2, group == "M")
meyes3 <- filter(ed3, group == "M")
meyes <- bind_rows(meyes1, meyes2)
meyes <- bind_rows(meyes, meyes3)
meyes <- arrange(meyes, id)#; View(meyes)

# face
bface1 <- filter(fd1, group == "B")
bface2 <- filter(fd2, group == "B")
bface3 <- filter(fd3, group == "B")
bface <- bind_rows(bface1, bface2)
bface <- bind_rows(bface, bface3)
bface <- arrange(bface, id)#; View(bface)

mface1 <- filter(fd1, group == "M")
mface2 <- filter(fd2, group == "M")
mface3 <- filter(fd3, group == "M")
mface <- bind_rows(mface1, mface2)
mface <- bind_rows(mface, mface3)
mface <- arrange(mface, id)#; View(mface)

# mouth
bmouth1 <- filter(md1, group == "B")
bmouth2 <- filter(md2, group == "B")
bmouth3 <- filter(md3, group == "B")
bmouth <- bind_rows(bmouth1, bmouth2)
bmouth <- bind_rows(bmouth, bmouth3)
bmouth <- arrange(bmouth, id)#; View(bmouth)

mmouth1 <- filter(md1, group == "M")
mmouth2 <- filter(md2, group == "M")
mmouth3 <- filter(md3, group == "M")
mmouth <- bind_rows(mmouth1, mmouth2)
mmouth <- bind_rows(mmouth, mmouth3)
mmouth <- arrange(mmouth, id)#; View(mmouth)

# nose
bnose1 <- filter(nd1, group == "B")
bnose2 <- filter(nd2, group == "B")
bnose3 <- filter(nd3, group == "B")
bnose <- bind_rows(bnose1, bnose2)
bnose <- bind_rows(bnose, bnose3)
bnose <- arrange(bnose, id)#; View(bnose)

mnose1 <- filter(nd1, group == "M")
mnose2 <- filter(nd2, group == "M")
mnose3 <- filter(nd3, group == "M")
mnose <- bind_rows(mnose1, mnose2)
mnose <- bind_rows(mnose, mnose3)
mnose <- arrange(mnose, id)#; View(mnose)

# now you have group-sorted data frames for body, eyes, face, mouth, & nose! 
praise()

### GROUP ANALYSES
# group diffs -- BODY (ALL NULL)
t.test(bbody$body_samplesInAOI, mbody$body_samplesInAOI) # null
t.test(bbody$body_propInAOI, mbody$body_propInAOI) # null
t.test(bbody$body_timeInAOI, mbody$body_timeInAOI) # null
t.test(bbody$body_firstSamp, mbody$body_firstSamp) # null
t.test(bbody$body_firstTimeS, mbody$body_firstTimeS) # null
t.test(bbody$body_numLooks, mbody$body_numLooks) # null
t.test(bbody$body_meanLook, mbody$body_meanLook) # null
t.test(bbody$body_peakLook, mbody$body_peakLook) # null
t.test(bbody$body_minLook, mbody$body_minLook) # null
t.test(bbody$body_ratioInAOI, mbody$body_ratioInAOI) # null

# group diffs -- EYES (ALL NULL)
t.test(beyes$eyes_samplesInAOI, meyes$eyes_samplesInAOI) # null
t.test(beyes$eyes_propInAOI, meyes$eyes_propInAOI) # null
t.test(beyes$eyes_timeInAOI, meyes$eyes_timeInAOI) # null
t.test(beyes$eyes_firstSamp, meyes$eyes_firstSamp) # null
t.test(beyes$eyes_firstTimeS, meyes$eyes_firstTimeS) # null
t.test(beyes$eyes_numLooks, meyes$eyes_numLooks) # null
t.test(beyes$eyes_meanLook, meyes$eyes_meanLook) # null
t.test(beyes$eyes_peakLook, meyes$eyes_peakLook) # null
t.test(beyes$eyes_minLook, meyes$eyes_minLook) # null
t.test(beyes$eyes_ratioInAOI, meyes$eyes_ratioInAOI) # null

# group diffs -- FACES (GENERAL)
t.test(bface$face_samplesInAOI, mface$face_samplesInAOI) # null
t.test(bface$face_propInAOI, mface$face_propInAOI) # null
t.test(bface$face_timeInAOI, mface$face_timeInAOI) # null
t.test(bface$face_firstSamp, mface$face_firstSamp) # null
t.test(bface$face_firstTimeS, mface$face_firstTimeS) # null
t.test(bface$face_numLooks, mface$face_numLooks) # null
t.test(bface$face_meanLook, mface$face_meanLook) # null
t.test(bface$face_peakLook, mface$face_peakLook) # null
t.test(bface$face_minLook, mface$face_minLook) # null
t.test(bface$face_ratioInAOI, mface$face_ratioInAOI) # null

# group diffs -- MOUTH
t.test(bmouth$mouth_samplesInAOI, mmouth$mouth_samplesInAOI) # null
t.test(bmouth$mouth_propInAOI, mmouth$mouth_propInAOI) # null
t.test(bmouth$mouth_timeInAOI, mmouth$mouth_timeInAOI) # null
t.test(bmouth$mouth_firstSamp, mmouth$mouth_firstSamp) # null
t.test(bmouth$mouth_firstTimeS, mmouth$mouth_firstTimeS) # null
t.test(bmouth$mouth_numLooks, mmouth$mouth_numLooks) # p = 0.058
t.test(bmouth$mouth_meanLook, mmouth$mouth_meanLook) # P = 0.062
t.test(bmouth$mouth_peakLook, mmouth$mouth_peakLook) # null
t.test(bmouth$mouth_minLook, mmouth$mouth_minLook) # p = 0.03
t.test(bmouth$mouth_ratioInAOI, mmouth$mouth_ratioInAOI) # null

# group diff -- NOSE (ALL NULL)
t.test(bnose$nose_samplesInAOI, mnose$nose_samplesInAOI) # null
t.test(bnose$nose_propInAOI, mnose$nose_propInAOI) # null
t.test(bnose$nose_timeInAOI, mnose$nose_timeInAOI) # null
t.test(bnose$nose_firstSamp, mnose$nose_firstSamp) # null
t.test(bnose$nose_firstTimeS, mnose$nose_firstTimeS) # null
t.test(bnose$nose_numLooks, mnose$nose_numLooks) # null
t.test(bnose$nose_meanLook, mnose$nose_meanLook) # null
t.test(bnose$nose_peakLook, mnose$nose_peakLook) # null
t.test(bnose$nose_minLook, mnose$nose_minLook) # null
t.test(bnose$nose_ratioInAOI, mnose$nose_ratioInAOI) # null