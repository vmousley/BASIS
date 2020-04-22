### Analysis code for 50 faces
# Last updated 2 April 2020
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

# EVELYNE DATA ------------------------------------------------------------
# load 50 faces data sets and fix ordering issues, view files manually
em1 <- data.frame(read_excel("/Users/victoriamousley/Desktop/MATLAB/evelynedata/lm_analysis/50faces/50faces_wide_faces 1.m4v_20200124T221154.xlsx"))
em2 <- data.frame(read_excel("/Users/victoriamousley/Desktop/MATLAB/evelynedata/lm_analysis/50faces/50faces_wide_faces 2.m4v_20200124T221421.xlsx"))
em3 <- data.frame(read_excel("/Users/victoriamousley/Desktop/MATLAB/evelynedata/lm_analysis/50faces/50faces_wide_faces 3.m4v_20200124T221700.xlsx"))

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
bgdata <- read.csv("/Users/victoriamousley/Desktop/MATLAB/evelynedata/lm_analysis/Age_Group_Gender.csv")#; View(bkgrddata)
names(bgdata)[1] <- "id"
names(bgdata)[2] <- "group"
names(bgdata)[3] <- "gender"
names(bgdata)[4] <- "days"
names(bgdata)[5] <- "months"
rownames(bgdata) <- seq(length=nrow(bgdata)) 
bgdata$id <- gsub('\\s+', '', bgdata$id) # removes trailing white space in ID values

idSplitter = function(x) unlist(str_split(x,"#"))[1]
emdata$id = sapply(emdata$id, idSplitter)
emdata$ids[!emdata$id %in% bgdata$id] # see what got missed
emdataMerge = merge(emdata, bgdata, by = 'id')

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

# EM: N = 51

finalDf <- emdataMerge2sec[!emdataMerge2sec$id %in% oneTrialList, ] # if df1 IDs are in oneTrialList, do not include those rows in finalDf
finalDf <- finalDf %>% arrange(id) # group rows by ID 

### Pre-processing ----------------------------------------------------------
# bilinguals will look at the mouth more than monolinguals
finalIds = unique(finalDf$id) # IDs in the finalDf
mouthPropAvList <- vector() # empty vector to hold 'mouth_propInAOI' average
mouthLTList <- vector() # empty vector to hold 'mouth_timeInAOI' average
faceLTAvList <- vector() # empty vector to hold 'face_timeInAOI' average
eyesLTAvList <- vector() # empty vector to hold 'eyes_timeInAOI' average
e2fList <- vector() # empty vector to hold eyes-to-face ratio
m2fList <- vector() # empty vector to hold mouth-to-face ratio
facelat <- vector()
flatlist <- vector()

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
  
  facelat <- flat <- mean(dfLoop$face_firstTimeS, na.rm = TRUE) # latency face measure, ignore empty
  flatlist <- c(flatlist, flat) # put mean value in an ordered list
  
}

analysisDf <- data.frame(finalIds, mouthPropAvList, mouthLTList, faceLTAvList, eyesLTAvList, e2fList, m2fList, flatlist) # create df with average and ID
analysisDf <- merge(analysisDf, bgdata, by.x = 'finalIds', by.y = 'id') # merge to include demographic data
analysisDf$months <- as.numeric(as.character(analysisDf$months)) 
analysisDf$age <- round(analysisDf$month/0.032855, 0) # make age var

### Descriptives (EVELYNE'S DATA) ----------------
descriptive <- data.frame(analysisDf$age, analysisDf$group, analysisDf$gender)
names(descriptive)[1] <- "age"
names(descriptive)[2] <- "group"
names(descriptive)[3] <- "gender"
levels(descriptive$gender)[levels(descriptive$gender)==1] <- "Male"
levels(descriptive$gender)[levels(descriptive$gender)==2] <- "Female"

# EXCLUDING OLDER BABIES FOR NOW
test <- data.frame(descriptive)
test$agem <- floor(test$age*0.0328767); test$agem
ptable <- table(test$agem, test$group); ptable

descriptive <- descriptive[-c(17, 35, 37),]
test <- data.frame(descriptive)
test$agem <- floor(test$age*0.0328767); test$agem

ptable <- table(test$agem, test$group); ptable

analysisDf <- analysisDf[-c(17, 35, 37), ] # excluding random older monolinguals

analysisDf %>%
  count(group, sort = TRUE)

female_bi_em <- as.numeric(count(descriptive %>%
                                filter(group == 'B', gender == "Female")))
female_mono_em <- as.numeric(count(descriptive %>% 
                                  filter(group == "M", gender == "Female")))
mono_total_em <- as.numeric(count(descriptive %>% filter(group == "M")))
mono_mean_days_em <- descriptive %>% 
  filter(group == "M", descriptive$age)
mono_mean_days_em <- round(mean(mono_mean_days_em$age), 2)
mono_mean_months_em <- round(mono_mean_days_em*0.0328767, 2)

bi_total_em <- as.numeric(count(descriptive %>% filter(group == "B")))
bi_mean_days_em <- descriptive %>% 
  filter(group == "B", descriptive$age)
bi_mean_days_em <- round(mean(bi_mean_days_em$age), 2)
bi_mean_months_em <- round(bi_mean_days_em*0.0328767, 2)
## All vars in analysisDf so far

# different between groups' ages?
age_m1_em <- aov(age ~ group, data = analysisDf)
apa_age_m1_em <- apa_print(age_m1_em)

# mouthPropAvList
mean(analysisDf$mouthPropAvList) # 0.09615939
median(analysisDf$mouthPropAvList) # 0.07322165
sd(analysisDf$mouthPropAvList) # 0.08788685

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# mouthPropAvList: 
quantile(analysisDf$mouthPropAvList, 0.16) # 
quantile(analysisDf$mouthPropAvList, 0.84) # 

mouthPropAvListHist <- hist(analysisDf$mouthPropAvList, col = 'steelblue')
abline(v=mean(analysisDf$mouthPropAvList), lty=2, lwd= 2)

boxplot(analysisDf$mouthPropAvList, col = 'steelblue')
outliers <- boxplot(analysisDf$mouthPropAvList, col = 'steelblue')$out
analysisDf[which(analysisDf$mouthPropAvList %in% outliers),] # 
# analysisDf <- analysisDf[-which(analysisDf$mouthPropAvList %in% outliers),]

# mouthLTList
mean(analysisDf$mouthLTList) # 
median(analysisDf$mouthLTList) # 
sd(analysisDf$mouthLTList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# mouthLTList: 
quantile(analysisDf$mouthLTList, 0.16) #   
quantile(analysisDf$mouthLTList, 0.84) # 

hist(analysisDf$mouthLTList, col = 'steelblue')
abline(v=mean(analysisDf$mouthLTList), lty=2, lwd= 2)

boxplot(analysisDf$mouthLTList, col = 'steelblue')
outliers <- boxplot(analysisDf$mouthLTList, col = 'steelblue')$out
analysisDf[which(analysisDf$mouthLTList %in% outliers),] # 
# analysisDf <- analysisDf[-which(analysisDf$mouthLTList %in% outliers),]

# faceLTAvList
mean(analysisDf$faceLTAvList) # 
median(analysisDf$faceLTAvList) # 
sd(analysisDf$faceLTAvList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# faceLTAvList: 
quantile(analysisDf$faceLTAvList, 0.16) # 
quantile(analysisDf$faceLTAvList, 0.84) # 

hist(analysisDf$faceLTAvList, col = 'steelblue')
abline(v=mean(analysisDf$faceLTAvList), lty=2, lwd= 2)

boxplot(analysisDf$faceLTAvList, col = 'steelblue')
outliers <- boxplot(analysisDf$faceLTAvList, col = 'steelblue')$out
# analysisDf[which(analysisDf$faceLTAvList %in% outliers),] 

# eyesLTAvList
mean(analysisDf$eyesLTAvList) # 
median(analysisDf$eyesLTAvList) # 
sd(analysisDf$eyesLTAvList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# eyesLTAvList: 
quantile(analysisDf$eyesLTAvList, 0.16) #   
quantile(analysisDf$eyesLTAvList, 0.84) # 

hist(analysisDf$eyesLTAvList, col = 'steelblue')
abline(v=mean(analysisDf$eyesLTAvList), lty=2, lwd= 2)

boxplot(analysisDf$eyesLTAvList, col = 'steelblue')
outliers <- boxplot(analysisDf$eyesLTAvList, col = 'steelblue')$out
analysisDf[which(analysisDf$eyesLTAvList %in% outliers),] # NO OUTLIERS

# e2fList
mean(analysisDf$e2fList) # 
median(analysisDf$e2fList) # 
sd(analysisDf$e2fList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# e2fList: 
quantile(analysisDf$e2fList, 0.16) #   
quantile(analysisDf$e2fList, 0.84) # 

hist(analysisDf$e2fList, col = 'steelblue')
abline(v=mean(analysisDf$e2fList), lty=2, lwd= 2)

boxplot(analysisDf$e2fList, col = 'steelblue')
outliers <- boxplot(analysisDf$e2fList, col = 'steelblue')$out
analysisDf[which(analysisDf$e2fList %in% outliers),] # NO OUTLIERS

# m2fList
mean(analysisDf$mouthPropAvList) # 
median(analysisDf$mouthPropAvList) # 
sd(analysisDf$mouthPropAvList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# m2fList: 
quantile(analysisDf$mouthPropAvList, 0.16) #   
quantile(analysisDf$mouthPropAvList, 0.84) # 

hist(analysisDf$m2fList, col = 'steelblue')
abline(v=mean(analysisDf$m2fList), lty=2, lwd= 2)

boxplot(analysisDf$m2fList, col = 'steelblue')
outliers <- boxplot(analysisDf$m2fList, col = 'steelblue')$out
analysisDf[which(analysisDf$m2fList %in% outliers),] # 
# analysisDf <- analysisDf[-which(analysisDf$m2fList %in% outliers),]

### Analysis (EVELYNE'S DATA) ----------------
### face latency measure
m1_facelat_em <- glm(flatlist ~ age*group, data = analysisDf)
summary(m1_facelat_em)
flatplot_em <- analysisDf %>% ggplot(aes(x = age, y = flatlist, colour = group)) +
  geom_point() +
  labs(y='Face latency', x = 'Age (days)', title = 'Face latency', subtitle = 'Monolingual and bilingual 7- to 10-month-olds', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') +
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); flatplot_em

# maybe try excluding outliers?

### mouthPropAvList
bMouthProp_em <- analysisDf[which(analysisDf$group %in% c('B')),]$mouthPropAvList
mMouthProp_em <- analysisDf[which(analysisDf$group %in% c('M')),]$mouthPropAvList
t.test(bMouthProp_em, mMouthProp_em) # non-significant
# 7 - 10 month old monolingual and bilingual infants do not spend a significantly
# different proportion of the time looking at the mouth

m1_mouthPropAvList_em <- glm(mouthPropAvList ~ age*group, data = analysisDf)
summary(m1_mouthPropAvList_em)
em_m1_mouthProp <- apa_print(m1_mouthPropAvList_em) # no effect of age, group, or interaction

mouthpropplot_em <- analysisDf %>% ggplot(aes(x = age, y = mouthPropAvList, colour = group)) +
  geom_point() +
  labs(y='Mouth-to-Face Ratio', x = 'Age (days)', title = 'Mouth-to-Face Ratio', subtitle = 'Monolingual and bilingual 7- to 10-month-olds', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') +
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); mouthpropplot_em

### total looking to mouth
bMouthLT_em <- analysisDf[which(analysisDf$group %in% c('B')),]$mouthLTList
mMouthLT_em <- analysisDf[which(analysisDf$group %in% c('M')),]$mouthLTList
t.test(bMouthLT_em, mMouthLT_em) # non-significant
# 7 - 10 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_mouthLTList_em <- glm(mouthLTList ~ age*group, data = analysisDf)
summary(m1_mouthLTList_em)
em_m1_mouthLT <- apa_print(m1_mouthLTList_em) # no effect of age, group, or interaction

em_mouthLTplot <- analysisDf %>% ggplot(aes(x = age, y = mouthLTList, colour = group)) +
  geom_point() + 
  labs(y='Looking Time to the Mouth AOI', x = 'Age (days)', title = 'Total Looking Time to Mouth with Age', subtitle = 'Monolingual vs bilingual 7- to 10-month-olds', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); em_mouthLTplot

### faceLTAvList
bfaceLTAvList_em <- analysisDf[which(analysisDf$group %in% c('B')),]$faceLTAvList
mfaceLTAvList_em <- analysisDf[which(analysisDf$group %in% c('M')),]$faceLTAvList
t.test(bfaceLTAvList_em, mfaceLTAvList_em) # non-significant
# 7 - 10 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_faceLTAvList_em <- glm(faceLTAvList ~ age*group, data = analysisDf)
summary(m1_faceLTAvList_em)
em_m1_faceLT <- apa_print(m1_faceLTAvList_em)

em_faceLTAvplot <- analysisDf %>% ggplot(aes(x = age, y = faceLTAvList, colour = group)) +
  geom_point() + 
  labs(y='Looking Time to Mouth AOI', x = 'Age (days)', title = 'Total Looking Time to Face with Age', subtitle =
         'Monolingual vs bilingual 7- to 10-month-olds', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); em_faceLTAvplot

### e2fList
be2fList_em <- analysisDf[which(analysisDf$group %in% c('B')),]$e2fList
me2fList_em <- analysisDf[which(analysisDf$group %in% c('M')),]$e2fList
t.test(be2fList_em, me2fList_em) # non-significant
# 7 - 10 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_e2fList_em <- glm(e2fList ~ age*group, data = analysisDf)
summary(m1_e2fList_em)
m1_e2fList_em <- apa_print(m1_e2fList_em)

em_e2fplot <- analysisDf %>% ggplot(aes(x = age, y = e2fList, colour = group)) +
  geom_point() + 
  labs(y='Eyes-to-Face Ratio', x = 'Age (days)', title = 'Eyes-to-Face Ratio with Age', 
       subtitle = 'Monolingual vs bilingual 7- to 10-month-olds', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); em_e2fplot

### m2fList
bm2fList_em <- analysisDf[which(analysisDf$group %in% c('B')),]$m2fList
mm2fList_em <- analysisDf[which(analysisDf$group %in% c('M')),]$m2fList
t.test(bm2fList_em, mm2fList_em) # non-significant
# 7 - 10 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_m2fList_em <- glm(m2fList ~ age*group, data = analysisDf)
summary(m1_m2fList_em)
m1_m2fList_em <- apa_print(m1_m2fList_em)

em_m2fplot <- analysisDf %>% ggplot(aes(x = age, y = m2fList, colour = group)) +
  geom_point() + 
  labs(y='Mouth-to-Face Ratio', x = 'Age (days)', title = 'Mouth-to-Face Ratio with Age', 
       subtitle = 'Monolingual vs bilingual 7- to 10-month-olds', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); em_m2fplot

### Exploratory Analyses (EVELYNE'S DATA) ----------------------------------------------------
## monolinguals & bilinguals e2f ratio - group diffS?
be2f_em <- analysisDf[which(analysisDf$group %in% c('B')),]$e2fList
me2f_em <- analysisDf[which(analysisDf$group %in% c('M')),]$e2fList
t.test(be2f_em, me2f_em)
# non-significant

em_e2fplot <- analysisDf %>% ggplot(aes(x = age, y = e2fList, colour = group)) +
  geom_point() + 
  labs(y='Eyes-to-Face Ratio', x = 'Age (days)', title = 'Eyes-to-Face Ratio with Age',
       subtitle = 'Monolingual vs bilingual 7- to 10-month-olds', colour = 'Group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') +
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
geom_smooth(method = 'lm', se = T, size = 0.5); em_e2fplot

## monolinguals & bilinguals m2f ratio - group diffS?
bm2f_em <- analysisDf[which(analysisDf$group %in% c('B')),]$m2fList
mm2f_em <- analysisDf[which(analysisDf$group %in% c('M')),]$m2fList
t.test(bm2f_em, mm2f_em)
# non-significant

## trajectories over time
  # mouth-to-face ratio with age
analysisDf$days <- as.numeric(as.character(analysisDf$days))
em_m2fplot <- ggplot(analysisDf, aes(days, m2fList, colour = group)) +
  geom_smooth(method="lm", se = T, size = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y='Mouth-to-Face Ratio', x = 'Age (days)', title = 'Mouth-to-Face Ratio with Age', 
       subtitle = 'Monolingual vs bilingual 7- to 10-month-olds') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')); em_m2fplot

m1_m2fList_em <- lm(m2fList ~ group*age, data = analysisDf)
em_m1_m2f <- summary(m1_m2fList_em)
em_m1_m2f <- apa_print(em_m1_m2f)
# non-significant

  # eyes-to-face ratio with age
em_e2fplot <- ggplot(analysisDf, aes(days, e2fList, colour = group)) +
  geom_smooth(method="lm", se = T, size = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y='Eyes-to-Face Ratio', x = 'Age (days)', title = 'Eyes-to-Face Ratio with Age',
       subtitle = 'Monolingual vs bilingual 7- to 10-month-olds') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')); em_e2fplot

m1 <- lm(e2fList ~ group*age, data = analysisDf)
em_m1_e2f <- summary(m1)
# non-significant

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

# VM ATM: N = 62

### Pre-processing ----------------------------------------------------------
# bilinguals will look at the mouth more than monolinguals
finalIds = unique(finalDf$id) # IDs in the finalDf
mouthPropAvList <- vector() # empty vector to hold 'mouth_propInAOI' average
mouthLTList <- vector() # empty vector to hold 'mouth_timeInAOI' average
faceLTAvList <- vector()
eyesLTAvList <- vector()
e2fList <- vector()
m2fList <- vector()
facelat <- vector()
flatlist <- vector()

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

  facelat <- flat <- mean(dfLoop$face_firstTimeS, na.rm = TRUE) # latency face measure, ignore empty
  flatlist <- c(flatlist, flat) # put mean value in an ordered list
}

vmanalysis <- data.frame(finalIds, mouthPropAvList, mouthLTList, faceLTAvList, eyesLTAvList, e2fList, m2fList, flatlist) # create df with average and ID
# merge vmanalysis and demodata so i have all the behavioural and ET results

idSplitter = function(x) unlist(str_split(x,"#"))[1]
vmanalysis$finalIds = sapply(vmanalysis$finalIds, idSplitter)
vmanalysis$finalIds[!vmanalysis$finalIds %in% demodata$id] # see what got missed

vmanalysis[vmanalysis == 'P11'] <- 'P011' # rename what got missed
vmanalysis[vmanalysis == 'B01'] <- 'B001' # rename what got missed
vmanalysis[vmanalysis == 'A012'] <- 'P012' # rename what got missed

names(vmanalysis)[1] <- "id"
vmanalysis = merge(vmanalysis, demodata, by = 'id')

# descriptive table
descriptive <- data.frame(vmanalysis$id, vmanalysis$Age, vmanalysis$Group, vmanalysis$gender)
names(descriptive)[1] <- "id"
names(descriptive)[2] <- "age"
names(descriptive)[3] <- "group"
names(descriptive)[4] <- "gender"

# EXCLUDING OLDER BABIES FOR NOW
test <- data.frame(descriptive)
test$agem <- floor(test$age*0.0328767); test$agem
ptable <- table(test$agem, test$group); ptable

descriptive <- descriptive[-c(4, 10, 11, 20, 24, 26, 33, 43),]

test <- data.frame(descriptive)
test$agem <- floor(test$age*0.0328767); test$agem
ptable <- table(test$agem, test$group); ptable

vmanalysis <- vmanalysis[-c(4, 10, 11, 20, 24, 26, 33, 43), ] # excluding a lot
# of monolinguals (randomly) to balance age groups :( UGH 

vmanalysis %>%
  count(Group, sort = TRUE)

### Descriptives (VICTORIA'S DATA) -----------------------
female_bi <- as.numeric(count(descriptive %>%
                                filter(group == 'B', gender == 2)))
female_mono <- as.numeric(count(descriptive %>% 
                                  filter(group == "M", gender == 2)))
mono_total <- as.numeric(count(descriptive %>% filter(group == "M")))
mono_mean_days <- descriptive %>% 
  filter(group == "M", descriptive$age)
mono_mean_days <- round(mean(mono_mean_days$age), 2)
mono_mean_months <- round(mono_mean_days*0.0328767, 2)

bi_total <- as.numeric(count(descriptive %>% filter(group == "B")))
bi_mean_days <- descriptive %>% 
  filter(group == "B", descriptive$age)
bi_mean_days <- round(mean(bi_mean_days$age), 2)
bi_mean_months <- round(bi_mean_days*0.0328767, 2)

# different between groups' ages?
age_m1 <- aov(Age ~ Group, data = vmanalysis)
apa_age_m1_vm <- apa_print(age_m1)

# face latency 
m1_facelat <- glm(flatlist ~ Age*Group, data = vmanalysis)
summary(m1)
vm_flatplot <- vmanalysis %>% ggplot(aes(x = Age, y = flatlist, colour = Group)) +
  geom_point() +
  labs(y='Face latency', x = 'Age (days)', title = 'Face latency', subtitle = '15- to 18-month-olds') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') +
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T); vm_flatplot

vm_flatplot <- vmanalysis %>% ggplot(aes(x = Age, y = flatlist)) +
  geom_point() +
  labs(y='Face latency', x = 'Age (days)', title = 'Face latency', subtitle = 'All 15- to 18-month-olds') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') +
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T); vm_flatplot

# try excluding outliers, explore more

## All vars in analysisDf so far

# mouthPropAvList
mean(vmanalysis$mouthPropAvList) # 
median(analysisDf$mouthPropAvList) # 
sd(analysisDf$mouthPropAvList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# mouthPropAvList: 
quantile(vmanalysis$mouthPropAvList, 0.16) #   
quantile(vmanalysis$mouthPropAvList, 0.84) # 

mouthPropAvListHist <- hist(vmanalysis$mouthPropAvList, col = 'steelblue')
abline(v=mean(vmanalysis$mouthPropAvList), lty=2, lwd= 2)

boxplot(vmanalysis$mouthPropAvList, col = 'steelblue')
outliers <- boxplot(vmanalysis$mouthPropAvList, col = 'steelblue')$out
vmanalysis[which(vmanalysis$mouthPropAvList %in% outliers),] # NO OUTLIERS

# mouthLTList
mean(vmanalysis$mouthLTList) # 
median(vmanalysis$mouthLTList) # 
sd(vmanalysis$mouthLTList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# mouthLTList: 
quantile(vmanalysis$mouthLTList, 0.16) #   
quantile(vmanalysis$mouthLTList, 0.84) # 

hist(vmanalysis$mouthLTList, col = 'steelblue')
abline(v=mean(vmanalysis$mouthLTList), lty=2, lwd= 2)

boxplot(vmanalysis$mouthLTList, col = 'steelblue')
outliers <- boxplot(vmanalysis$mouthLTList, col = 'steelblue')$out
vmanalysis[which(vmanalysis$mouthLTList %in% outliers),] # NO OUTLIERS

# faceLTAvList
mean(vmanalysis$faceLTAvList) # 6.907829
median(vmanalysis$faceLTAvList) # 6.938105
sd(vmanalysis$faceLTAvList) # 1.646949

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# faceLTAvList: 
quantile(vmanalysis$faceLTAvList, 0.16) # 
quantile(vmanalysis$faceLTAvList, 0.84) # 

hist(vmanalysis$faceLTAvList, col = 'steelblue')
abline(v=mean(vmanalysis$faceLTAvList), lty=2, lwd= 2)

boxplot(vmanalysis$faceLTAvList, col = 'steelblue')
outliers <- boxplot(vmanalysis$faceLTAvList, col = 'steelblue')$out
vmanalysis[which(vmanalysis$faceLTAvList %in% outliers),] # NO OUTLIERS

# eyesLTAvList
mean(vmanalysis$eyesLTAvList) # 
median(vmanalysis$eyesLTAvList) # 
sd(vmanalysis$eyesLTAvList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# eyesLTAvList: 
quantile(vmanalysis$eyesLTAvList, 0.16) #   
quantile(vmanalysis$eyesLTAvList, 0.84) # 

hist(vmanalysis$eyesLTAvList, col = 'steelblue')
abline(v=mean(vmanalysis$eyesLTAvList), lty=2, lwd= 2)

boxplot(vmanalysis$eyesLTAvList, col = 'steelblue')
outliers <- boxplot(vmanalysis$eyesLTAvList, col = 'steelblue')$out
vmanalysis[which(vmanalysis$eyesLTAvList %in% outliers),] # NO OUTLIERS

# e2fList
mean(vmanalysis$e2fList) # 
median(vmanalysis$e2fList) # 
sd(vmanalysis$e2fList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# e2fList: 
quantile(vmanalysis$e2fList, 0.16) #   
quantile(vmanalysis$e2fList, 0.84) # 

hist(vmanalysis$e2fList, col = 'steelblue')
abline(v=mean(vmanalysis$e2fList), lty=2, lwd= 2)

boxplot(vmanalysis$e2fList, col = 'steelblue')
outliers <- boxplot(vmanalysis$e2fList, col = 'steelblue')$out
analysisDf[which(vmanalysis$e2fList %in% outliers),] # NO OUTLIERS

# m2fList
mean(vmanalysis$mouthPropAvList) # 
median(vmanalysis$mouthPropAvList) # 
sd(vmanalysis$mouthPropAvList) # 

# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# m2fList: 
quantile(vmanalysis$mouthPropAvList, 0.16) #   
quantile(vmanalysis$mouthPropAvList, 0.84) # 

hist(vmanalysis$m2fList, col = 'steelblue')
abline(v=mean(vmanalysis$m2fList), lty=2, lwd= 2)

boxplot(vmanalysis$m2fList, col = 'steelblue')
outliers <- boxplot(vmanalysis$m2fList, col = 'steelblue')$out
analysisDf[which(analysisDf$m2fList %in% outliers),] # NO OUTLIERS

### Analysis (VICTORIA'S DATA) -----------------------

### Proportion of Looking to the Mouth AOI
bMouthProp <- vmanalysis[which(vmanalysis$Group %in% c('B')),]$mouthPropAvList
mMouthProp <- vmanalysis[which(vmanalysis$Group %in% c('M')),]$mouthPropAvList
t.test(bMouthProp, mMouthProp) # NOPE

m1_vmmouthPropAvList <- glm(mouthPropAvList ~ Age*Group, data = vmanalysis)
summary(m1_vmmouthPropAvList)
m1_vmmouthPropAvList <- apa_print(m1_vmmouthPropAvList) #

vm_mouthpropplot <- vmanalysis %>% ggplot(aes(x = Age, y = mouthPropAvList, colour = Group)) +
  geom_point() + 
  labs(y='Mouth-to-Face Ratio', x = 'Age (days)', title = 'Mouth-to-Face Ratio with Age', 
       subtitle = '15- to 18-month-olds by group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); vm_mouthpropplot

### Total Looking Time to the Mouth AOI
bMouthLT <- vmanalysis[which(vmanalysis$Group %in% c('B')),]$mouthLTList
mMouthLT <- vmanalysis[which(vmanalysis$Group %in% c('M')),]$mouthLTList
t.test(bMouthLT, mMouthLT) # NOPE
# 15 - 18 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_vmmouthLT <- glm(mouthLTList ~ Age*Group, data = vmanalysis)
summary(m1_vmmouthLT)
m1_vmmouthLT <- apa_print(m1_vmmouthLT)

vm_mouthLT <- vmanalysis %>% ggplot(aes(x = Age, y = mouthLTList, colour = Group)) +
  geom_point() + 
  labs(y='Total LT to Mouth AOI', x = 'Age (days)', title = 'Total Looking to Mouth with Age', 
       subtitle = '15- to 18-month-olds by group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); vm_mouthLT

### Total Looking Time to the Face AOI
bfaceLTAvList <- vmanalysis[which(vmanalysis$Group %in% c('B')),]$faceLTAvList
mfaceLTAvList <- vmanalysis[which(vmanalysis$Group %in% c('M')),]$faceLTAvList
groupfaceLT <- apa_print(t.test(bfaceLTAvList, mfaceLTAvList)) # NOPE
# 15 - 18 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_faceLTAvList <- glm(faceLTAvList ~ Age*Group, data = vmanalysis)
summary(m1_faceLTAvList)
m1_faceLTAvList <- apa_print(m1_faceLTAvList)

vm_faceLTAvListplot <- vmanalysis %>% ggplot(aes(x = Age, y = faceLTAvList, colour = Group)) +
  geom_point() + 
  labs(y='Total Looking Time to Face AOI', x = 'Age (days)', title = 'Total Looking Time to Face', 
       subtitle = '15- to 18-month-olds by group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); vm_faceLTAvListplot

### Exploratory Analyses (VICTORIA DATA) ----------------------------------------------------
### Eyes-to-Face Ratio
be2fList <- vmanalysis[which(vmanalysis$Group %in% c('B')),]$e2fList
me2fList <- vmanalysis[which(vmanalysis$Group %in% c('M')),]$e2fList
t.test(be2fList, me2fList) # NOPE
# 15 - 18 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_vme2fList <- glm(e2fList ~ Age*Group, data = vmanalysis)
summary(m1_vme2fList)
m1_vme2fList <- apa_print(m1_vme2fList) # TREND

vm_e2fplot <- vmanalysis %>% ggplot(aes(x = Age, y = e2fList, colour = Group)) +
  geom_point() + 
  labs(y='Eyes-to-Face Ratio', x = 'Age (days)', title = 'Eyes-to-Face Ratio with Age', 
       subtitle = '15- to 18-month-olds by group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); vm_e2fplot

### Mouth-to-Face Ratio
bm2fList <- vmanalysis[which(vmanalysis$Group %in% c('B')),]$m2fList
mm2fList <- vmanalysis[which(vmanalysis$Group %in% c('M')),]$m2fList
t.test(bm2fList, mm2fList) # NOPE
# 15 - 18 month old monolingual and bilingual infants do not spend a significantly
# different amounts of time from one another looking at the mouth

m1_vmm2fList <- glm(m2fList ~ Age*Group, data = vmanalysis)
summary(m1_vmm2fList)
m1_vmm2fList <- apa_print(m1_vmm2fList) # nope

vm_m2fplot <- vmanalysis %>% ggplot(aes(x = Age, y = m2fList, colour = Group)) +
  geom_point() + 
  labs(y='Eyes-to-Face Ratio', x = 'Age (days)', title = 'Eyes-to-Face Ratio with Age', 
       subtitle = '15- to 18-month-olds by group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); vm_m2fplot

### EM & VM Data Together -------------------
# mouth to face ratio in younger & older bilinguals

plot1 <- em_m2fplot
plot2 <- vm_m2fplot
plot_grid(plot1, plot2, labels = "AUTO")

# eyes to face ratio in younger and older bilniguals
plot1 <- em_e2fplot
plot2 <- vm_e2fplot
plot_grid(plot1, plot2, labels = "AUTO")

### FOI ----------------
df1 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid1_1.txt', header = FALSE)
df2 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid1_2.txt', header = FALSE)
df3 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid1_3.txt', header = FALSE)
df4 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid1_4.txt', header = FALSE)
df5 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid1_5.txt', header = FALSE)
df6 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid1_6.txt', header = FALSE)
df7 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid2_7.txt', header = FALSE)
df8 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid2_8.txt', header = FALSE)
df9 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid2_9.txt', header = FALSE)
df10 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid2_10.txt', header = FALSE)
df11 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid3_11.txt', header = FALSE)
df12 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid3_12.txt', header = FALSE)
df13 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid3_13.txt', header = FALSE)
df14 <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/vid3_14.txt', header = FALSE)

gaze1 <- data.frame(df1, df2$V2, df3$V2, df4$V2, df5$V2, df6$V2)
gaze2 <- data.frame(df7, df8$V2, df9$V2, df10$V2)
gaze3 <- data.frame(df11, df12$V2, df13$V2, df14$V2)

names(gaze1)[names(gaze1)=="V1"] <- "id" # change annoying name to nice one
names(gaze1)[names(gaze1)=="V2"] <- "face1" # change annoying name to nice one
names(gaze1)[names(gaze1)=="df2.V2"] <- "face2" # change annoying name to nice one
names(gaze1)[names(gaze1)=="df3.V2"] <- "face3" # change annoying name to nice one
names(gaze1)[names(gaze1)=="df4.V2"] <- "face4" # change annoying name to nice one
names(gaze1)[names(gaze1)=="df5.V2"] <- "face5" # change annoying name to nice one
names(gaze1)[names(gaze1)=="df6.V2"] <- "face6" # change annoying name to nice one
gaze1 <- gaze1[-1,]

names(gaze2)[names(gaze2)=="V1"] <- "id" # change annoying name to nice one
names(gaze2)[names(gaze2)=="V2"] <- "face1" # change annoying name to nice one
names(gaze2)[names(gaze2)=="df8.V2"] <- "face2" # change annoying name to nice one
names(gaze2)[names(gaze2)=="df9.V2"] <- "face3" # change annoying name to nice one
names(gaze2)[names(gaze2)=="df10.V2"] <- "face4" # change annoying name to nice one
gaze2 <- gaze2[-1,]

names(gaze3)[names(gaze3)=="V1"] <- "id" # change annoying name to nice one
names(gaze3)[names(gaze3)=="V2"] <- "face1" # change annoying name to nice one
names(gaze3)[names(gaze3)=="df12.V2"] <- "face2" # change annoying name to nice one
names(gaze3)[names(gaze3)=="df13.V2"] <- "face3" # change annoying name to nice one
names(gaze3)[names(gaze3)=="df14.V2"] <- "face4" # change annoying name to nice one
gaze3 <- gaze3[-1,]

### Calculate mean latencies per baby by video
gaze1$id <- as.character(gaze1$id)
gaze1$face1 <- as.numeric(as.character(gaze1$face1), na.rm = TRUE)
gaze1$face2 <- as.numeric(as.character(gaze1$face2), na.rm = TRUE)
gaze1$face3 <- as.numeric(as.character(gaze1$face3), na.rm = TRUE)
gaze1$face4 <- as.numeric(as.character(gaze1$face4), na.rm = TRUE)
gaze1$face5 <- as.numeric(as.character(gaze1$face5), na.rm = TRUE)
gaze1$face6 <- as.numeric(as.character(gaze1$face6), na.rm = TRUE)

gaze2$id <- as.character(gaze2$id)
gaze2$face1 <- as.numeric(as.character(gaze2$face1), na.rm = TRUE)
gaze2$face2 <- as.numeric(as.character(gaze2$face2), na.rm = TRUE)
gaze2$face3 <- as.numeric(as.character(gaze2$face3), na.rm = TRUE)
gaze2$face4 <- as.numeric(as.character(gaze2$face4), na.rm = TRUE)

gaze3$id <- as.character(gaze3$id)
gaze3$face1 <- as.numeric(as.character(gaze3$face1), na.rm = TRUE)
gaze3$face2 <- as.numeric(as.character(gaze3$face2), na.rm = TRUE)
gaze3$face3 <- as.numeric(as.character(gaze3$face3), na.rm = TRUE)
gaze3$face4 <- as.numeric(as.character(gaze3$face4), na.rm = TRUE)

gaze1$mean_total <- rowMeans(gaze1[sapply(gaze1, is.numeric)], na.rm = TRUE)
gaze2$mean_total <- rowMeans(gaze2[sapply(gaze2, is.numeric)], na.rm = TRUE)
gaze3$mean_total <- rowMeans(gaze3[sapply(gaze3, is.numeric)], na.rm = TRUE)

gaze1[gaze1 == 'A012#NONE'] <- 'A012'
gaze1[gaze1 == 'B002#NONE'] <- 'B002'
gaze1[gaze1 == 'B003#NONE'] <- 'B003'
gaze1[gaze1 == 'B004#NONE'] <- 'B004'
gaze1[gaze1 == 'B005#NONE'] <- 'B005'
gaze1[gaze1 == 'B006#NONE'] <- 'B006'
gaze1[gaze1 == 'B007#NONE'] <- 'B007'
gaze1[gaze1 == 'B008#NONE'] <- 'B008'
gaze1[gaze1 == 'B009#NONE'] <- 'B009'
gaze1[gaze1 == 'B010#NONE'] <- 'B010'
gaze1[gaze1 == 'B011#NONE'] <- 'B011'
gaze1[gaze1 == 'B012#NONE'] <- 'B012'
gaze1[gaze1 == 'B013#NONE'] <- 'B013'
gaze1[gaze1 == 'B014#NONE'] <- 'B014'
gaze1[gaze1 == 'B015#NONE'] <- 'B015'
gaze1[gaze1 == 'B016#NONE'] <- 'B016'
gaze1[gaze1 == 'B017#NONE'] <- 'B017'
gaze1[gaze1 == 'B018#NONE'] <- 'B018'
gaze1[gaze1 == 'B019#NONE'] <- 'B019'
gaze1[gaze1 == 'B01#NONE'] <- 'B001'
gaze1[gaze1 == 'B020#NONE'] <- 'B020'
gaze1[gaze1 == 'B021#NONE'] <- 'B021'
gaze1[gaze1 == 'B022#NONE'] <- 'B022'
gaze1[gaze1 == 'B023#NONE'] <- 'B023'
gaze1[gaze1 == 'B024#NONE'] <- 'B024'
gaze1[gaze1 == 'B025#NONE'] <- 'B025'
gaze1[gaze1 == 'B026#NONE'] <- 'B026'
gaze1[gaze1 == 'B027#NONE'] <- 'B027'
gaze1[gaze1 == 'B028#NONE'] <- 'B028'
gaze1[gaze1 == 'B029#NONE'] <- 'B029'
gaze1[gaze1 == 'B030#NONE'] <- 'B030'
gaze1[gaze1 == 'B031#NONE'] <- 'B031'
gaze1[gaze1 == 'B032#NONE'] <- 'B032'
gaze1[gaze1 == 'B033#NONE'] <- 'B033'
gaze1[gaze1 == 'B034#NONE'] <- 'B034'
gaze1[gaze1 == 'B035#NONE'] <- 'B035'
gaze1[gaze1 == 'B036#NONE'] <- 'B036'
gaze1[gaze1 == 'B037#NONE'] <- 'B037'
gaze1[gaze1 == 'B038#NONE'] <- 'B038'
gaze1[gaze1 == 'B039#NONE'] <- 'B039'
gaze1[gaze1 == 'B040#NONE'] <- 'B040'
gaze1[gaze1 == 'B041#NONE'] <- 'B041'
gaze1[gaze1 == 'B042#NONE'] <- 'B042'
gaze1[gaze1 == 'B043#NONE'] <- 'B043'
gaze1[gaze1 == 'B044#NONE'] <- 'B044'
gaze1[gaze1 == 'B045#NONE'] <- 'B045'
gaze1[gaze1 == 'B046#NONE'] <- 'B046'
gaze1[gaze1 == 'B047#NONE'] <- 'B047'
gaze1[gaze1 == 'B048#NONE'] <- 'B048'
gaze1[gaze1 == 'B049#NONE'] <- 'B049'
gaze1[gaze1 == 'B050#NONE'] <- 'B050'
gaze1[gaze1 == 'B051#NONE'] <- 'B051'
gaze1[gaze1 == 'B052#NONE'] <- 'B052'
gaze1[gaze1 == 'B053#NONE'] <- 'B053'
gaze1[gaze1 == 'B054#NONE'] <- 'B054'
gaze1[gaze1 == 'B055#NONE'] <- 'B055'
gaze1[gaze1 == 'B056#NONE'] <- 'B056'
gaze1[gaze1 == 'B057#NONE'] <- 'B057'
gaze1[gaze1 == 'B058#NONE'] <- 'B058'
gaze1[gaze1 == 'B059#NONE'] <- 'B059'
gaze1[gaze1 == 'B060#NONE'] <- 'B060'
gaze1[gaze1 == 'B061#NONE'] <- 'B061'
gaze1[gaze1 == 'B062#NONE'] <- 'B062'
gaze1[gaze1 == 'B063#NONE'] <- 'B063'
gaze1[gaze1 == 'B064#NONE'] <- 'B064'
gaze1[gaze1 == 'B065#NONE'] <- 'B065'
gaze1[gaze1 == 'B066#NONE'] <- 'B066'
gaze1[gaze1 == 'B067#NONE'] <- 'B067'
gaze1[gaze1 == 'B068#NONE'] <- 'B068'
gaze1[gaze1 == 'B069#NONE'] <- 'B069'
gaze1[gaze1 == 'B070#NONE'] <- 'B070'
gaze1[gaze1 == 'B071#NONE'] <- 'B071'
gaze1[gaze1 == 'B072#NONE'] <- 'B072'
gaze1[gaze1 == 'B073#NONE'] <- 'B073'
gaze1[gaze1 == 'B074#NONE'] <- 'B074'
gaze1[gaze1 == 'P11#NONE'] <- 'P011'

gaze2[gaze2 == 'A012#NONE'] <- 'A012'
gaze2[gaze2 == 'B002#NONE'] <- 'B002'
gaze2[gaze2 == 'B003#NONE'] <- 'B003'
gaze2[gaze2 == 'B004#NONE'] <- 'B004'
gaze2[gaze2 == 'B005#NONE'] <- 'B005'
gaze2[gaze2 == 'B007#NONE'] <- 'B007'
gaze2[gaze2 == 'B008#NONE'] <- 'B008'
gaze2[gaze2 == 'B009#NONE'] <- 'B009'
gaze2[gaze2 == 'B010#NONE'] <- 'B010'
gaze2[gaze2 == 'B011#NONE'] <- 'B011'
gaze2[gaze2 == 'B012#NONE'] <- 'B012'
gaze2[gaze2 == 'B013#NONE'] <- 'B013'
gaze2[gaze2 == 'B014#NONE'] <- 'B014'
gaze2[gaze2 == 'B015#NONE'] <- 'B015'
gaze2[gaze2 == 'B016#NONE'] <- 'B016'
gaze2[gaze2 == 'B017#NONE'] <- 'B017'
gaze2[gaze2 == 'B018#NONE'] <- 'B018'
gaze2[gaze2 == 'B019#NONE'] <- 'B019'
gaze2[gaze2 == 'B01#NONE'] <- 'B001'
gaze2[gaze2 == 'B020#NONE'] <- 'B020'
gaze2[gaze2 == 'B021#NONE'] <- 'B021'
gaze2[gaze2 == 'B022#NONE'] <- 'B022'
gaze2[gaze2 == 'B023#NONE'] <- 'B023'
gaze2[gaze2 == 'B024#NONE'] <- 'B024'
gaze2[gaze2 == 'B025#NONE'] <- 'B025'
gaze2[gaze2 == 'B026#NONE'] <- 'B026'
gaze2[gaze2 == 'B027#NONE'] <- 'B027'
gaze2[gaze2 == 'B028#NONE'] <- 'B028'
gaze2[gaze2 == 'B029#NONE'] <- 'B029'
gaze2[gaze2 == 'B030#NONE'] <- 'B030'
gaze2[gaze2 == 'B031#NONE'] <- 'B031'
gaze2[gaze2 == 'B032#NONE'] <- 'B032'
gaze2[gaze2 == 'B033#NONE'] <- 'B033'
gaze2[gaze2 == 'B034#NONE'] <- 'B034'
gaze2[gaze2 == 'B035#NONE'] <- 'B035'
gaze2[gaze2 == 'B036#NONE'] <- 'B036'
gaze2[gaze2 == 'B037#NONE'] <- 'B037'
gaze2[gaze2 == 'B038#NONE'] <- 'B038'
gaze2[gaze2 == 'B039#NONE'] <- 'B039'
gaze2[gaze2 == 'B040#NONE'] <- 'B040'
gaze2[gaze2 == 'B041#NONE'] <- 'B041'
gaze2[gaze2 == 'B042#NONE'] <- 'B042'
gaze2[gaze2 == 'B043#NONE'] <- 'B043'
gaze2[gaze2 == 'B044#NONE'] <- 'B044'
gaze2[gaze2 == 'B045#NONE'] <- 'B045'
gaze2[gaze2 == 'B046#NONE'] <- 'B046'
gaze2[gaze2 == 'B047#NONE'] <- 'B047'
gaze2[gaze2 == 'B048#NONE'] <- 'B048'
gaze2[gaze2 == 'B049#NONE'] <- 'B049'
gaze2[gaze2 == 'B050#NONE'] <- 'B050'
gaze2[gaze2 == 'B051#NONE'] <- 'B051'
gaze2[gaze2 == 'B052#NONE'] <- 'B052'
gaze2[gaze2 == 'B053#NONE'] <- 'B053'
gaze2[gaze2 == 'B054#NONE'] <- 'B054'
gaze2[gaze2 == 'B055#NONE'] <- 'B055'
gaze2[gaze2 == 'B056#NONE'] <- 'B056'
gaze2[gaze2 == 'B057#NONE'] <- 'B057'
gaze2[gaze2 == 'B058#NONE'] <- 'B058'
gaze2[gaze2 == 'B059#NONE'] <- 'B059'
gaze2[gaze2 == 'B060#NONE'] <- 'B060'
gaze2[gaze2 == 'B061#NONE'] <- 'B061'
gaze2[gaze2 == 'B062#NONE'] <- 'B062'
gaze2[gaze2 == 'B063#NONE'] <- 'B063'
gaze2[gaze2 == 'B064#NONE'] <- 'B064'
gaze2[gaze2 == 'B065#NONE'] <- 'B065'
gaze2[gaze2 == 'B066#NONE'] <- 'B066'
gaze2[gaze2 == 'B067#NONE'] <- 'B067'
gaze2[gaze2 == 'B068#NONE'] <- 'B068'
gaze2[gaze2 == 'B069#NONE'] <- 'B069'
gaze2[gaze2 == 'B070#NONE'] <- 'B070'
gaze2[gaze2 == 'B071#NONE'] <- 'B071'
gaze2[gaze2 == 'B072#NONE'] <- 'B072'
gaze2[gaze2 == 'B073#NONE'] <- 'B073'
gaze2[gaze2 == 'B074#NONE'] <- 'B074'
gaze2[gaze2 == 'P11#NONE'] <- 'P011'

gaze3[gaze3 == 'A012#NONE'] <- 'A012'
gaze3[gaze3 == 'B002#NONE'] <- 'B002'
gaze3[gaze3 == 'B003#NONE'] <- 'B003'
gaze3[gaze3 == 'B004#NONE'] <- 'B004'
gaze3[gaze3 == 'B005#NONE'] <- 'B005'
gaze3[gaze3 == 'B006#NONE'] <- 'B006'
gaze3[gaze3 == 'B007#NONE'] <- 'B007'
gaze3[gaze3 == 'B008#NONE'] <- 'B008'
gaze3[gaze3 == 'B009#NONE'] <- 'B009'
gaze3[gaze3 == 'B010#NONE'] <- 'B010'
gaze3[gaze3 == 'B011#NONE'] <- 'B011'
gaze3[gaze3 == 'B012#NONE'] <- 'B012'
gaze3[gaze3 == 'B013#NONE'] <- 'B013'
gaze3[gaze3 == 'B014#NONE'] <- 'B014'
gaze3[gaze3 == 'B015#NONE'] <- 'B015'
gaze3[gaze3 == 'B016#NONE'] <- 'B016'
gaze3[gaze3 == 'B017#NONE'] <- 'B017'
gaze3[gaze3 == 'B018#NONE'] <- 'B018'
gaze3[gaze3 == 'B019#NONE'] <- 'B019'
gaze3[gaze3 == 'B01#NONE'] <- 'B001'
gaze3[gaze3 == 'B020#NONE'] <- 'B020'
gaze3[gaze3 == 'B021#NONE'] <- 'B021'
gaze3[gaze3 == 'B022#NONE'] <- 'B022'
gaze3[gaze3 == 'B023#NONE'] <- 'B023'
gaze3[gaze3 == 'B024#NONE'] <- 'B024'
gaze3[gaze3 == 'B025#NONE'] <- 'B025'
gaze3[gaze3 == 'B026#NONE'] <- 'B026'
gaze3[gaze3 == 'B027#NONE'] <- 'B027'
gaze3[gaze3 == 'B028#NONE'] <- 'B028'
gaze3[gaze3 == 'B029#NONE'] <- 'B029'
gaze3[gaze3 == 'B030#NONE'] <- 'B030'
gaze3[gaze3 == 'B031#NONE'] <- 'B031'
gaze3[gaze3 == 'B032#NONE'] <- 'B032'
gaze3[gaze3 == 'B033#NONE'] <- 'B033'
gaze3[gaze3 == 'B034#NONE'] <- 'B034'
gaze3[gaze3 == 'B035#NONE'] <- 'B035'
gaze3[gaze3 == 'B036#NONE'] <- 'B036'
gaze3[gaze3 == 'B037#NONE'] <- 'B037'
gaze3[gaze3 == 'B038#NONE'] <- 'B038'
gaze3[gaze3 == 'B039#NONE'] <- 'B039'
gaze3[gaze3 == 'B040#NONE'] <- 'B040'
gaze3[gaze3 == 'B041#NONE'] <- 'B041'
gaze3[gaze3 == 'B042#NONE'] <- 'B042'
gaze3[gaze3 == 'B043#NONE'] <- 'B043'
gaze3[gaze3 == 'B044#NONE'] <- 'B044'
gaze3[gaze3 == 'B045#NONE'] <- 'B045'
gaze3[gaze3 == 'B046#NONE'] <- 'B046'
gaze3[gaze3 == 'B047#NONE'] <- 'B047'
gaze3[gaze3 == 'B048#NONE'] <- 'B048'
gaze3[gaze3 == 'B049#NONE'] <- 'B049'
gaze3[gaze3 == 'B050#NONE'] <- 'B050'
gaze3[gaze3 == 'B051#NONE'] <- 'B051'
gaze3[gaze3 == 'B052#NONE'] <- 'B052'
gaze3[gaze3 == 'B053#NONE'] <- 'B053'
gaze3[gaze3 == 'B054#NONE'] <- 'B054'
gaze3[gaze3 == 'B055#NONE'] <- 'B055'
gaze3[gaze3 == 'B056#NONE'] <- 'B056'
gaze3[gaze3 == 'B057#NONE'] <- 'B057'
gaze3[gaze3 == 'B058#NONE'] <- 'B058'
gaze3[gaze3 == 'B059#NONE'] <- 'B059'
gaze3[gaze3 == 'B060#NONE'] <- 'B060'
gaze3[gaze3 == 'B061#NONE'] <- 'B061'
gaze3[gaze3 == 'B062#NONE'] <- 'B062'
gaze3[gaze3 == 'B063#NONE'] <- 'B063'
gaze3[gaze3 == 'B064#NONE'] <- 'B064'
gaze3[gaze3 == 'B065#NONE'] <- 'B065'
gaze3[gaze3 == 'B066#NONE'] <- 'B066'
gaze3[gaze3 == 'B067#NONE'] <- 'B067'
gaze3[gaze3 == 'B068#NONE'] <- 'B068'
gaze3[gaze3 == 'B069#NONE'] <- 'B069'
gaze3[gaze3 == 'B070#NONE'] <- 'B070'
gaze3[gaze3 == 'B071#NONE'] <- 'B071'
gaze3[gaze3 == 'B072#NONE'] <- 'B072'
gaze3[gaze3 == 'B073#NONE'] <- 'B073'
gaze3[gaze3 == 'B074#NONE'] <- 'B074'
gaze3[gaze3 == 'P11#NONE'] <- 'P011'

# now merge all 3 data frames together
test <- merge(gaze1, merge(gaze2, gaze3, by='id', all.x = TRUE, all.y = TRUE), by = 'id', all.x = TRUE, all.y = TRUE)
final <- data.frame(id=test[,1], means=rowMeans(test[,-1], na.rm = TRUE))

x <- final[final$id %in% descriptive$id, ]; x
lat_means <- merge(x, descriptive, by='id')

lat_means_plot <- lat_means %>% ggplot(aes(x = age, y = means, colour = group)) +
  geom_point() + 
  labs(y='Latency means', x = 'Age (days)', title = '50 Faces Latency Means', 
       subtitle = '15- to 18-month-olds by group') +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
  geom_smooth(method = 'lm', se = T, size = 0.5); lat_means_plot

# t-test latency
lat_means_m1 <- glm(means ~ age*group, data = lat_means)
summary(lat_means_m1)
lat_means_m1 <- apa_print(lat_means_m1) # nope

### GRAVEYARD----------
# FOM_em <- analysisDf %>% ggplot(aes(x = age, y = flatlist, colour = group)) +
#   geom_point() + 
#   labs(y='Face latency (sec)', x = 'Age (days)', title = 'Face latency in 7- to 10-month olds', 
#        subtitle = '7- to 10-month-olds') +
#   theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
#         plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
#         axis.title.x = element_text(size = 10),
#         axis.text.x = element_text(family = 'Helvetica', size = (10)),
#         axis.title.y = element_text(family = 'Helvetica', size = (10)),
#         legend.position = 'right') + 
#   scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
#   geom_smooth(method = 'lm', se = T); FOM_em
# 
# FOM_aov_em <- aov(flatlist ~ age*group, data = analysisDf)
# summary(FOM_aov_em) #non-significant
# 
# FOM_vm <- vmanalysis %>% ggplot(aes(x = Age, y = flatlist, colour = Group)) +
#   geom_point() + 
#   labs(y='Face latency (sec)', x = 'Age (days)', title = 'Face latency in 7- to 10-month olds', 
#        subtitle = '15- to 18-month-olds') +
#   theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
#         plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
#         axis.title.x = element_text(size = 10),
#         axis.text.x = element_text(family = 'Helvetica', size = (10)),
#         axis.title.y = element_text(family = 'Helvetica', size = (10)),
#         legend.position = 'right') + 
#   scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')) +
#   geom_smooth(method = 'lm', se = T); FOM_vm
# 
# FOM_aov_vm <- aov(flatlist ~ Age*Group, data = vmanalysis)
# summary(FOM_aov_vm)
# 
# plot1 <- em_m2fplot
# plot2 <- vm_m2fplot
# plot_grid(plot1, plot2, labels = "AUTO")
# 
# # face orientation
# plot1 <- FOM_em
# plot2 <- FOM_vm
# plot_grid(plot1, plot2, labels = "AUTO")

### Loading data frame of face onset times (manually) ----
# onsettimes1 <- data.frame(c(0.4, 1.92, 3.40, 4.64, 7.52, 10.52) , c('Face1', 'Face2', 'Face3', 'Face4', 'Face5', 'Face6'))
# names(onsettimes1)[names(onsettimes1)=="c.0.4..1.92..3.4..4.64..7.52..10.52."] <- "time" # change annoying name to nice one
# names(onsettimes1)[names(onsettimes1)=="c..Face1....Face2....Face3....Face4....Face5....Face6.."] <- "face" # change annoying name to nice one
# 
# onsettimes2 <- data.frame(c(0.04, 3.72, 5.88, 9.20), c('Face1', 'Face2', 'Face3', 'Face4'))
# names(onsettimes2)[names(onsettimes2)=="c.0.04..3.72..5.88..9.2."] <- "time" # change annoying name to nice one
# names(onsettimes2)[names(onsettimes2)=="c..Face1....Face2....Face3....Face4.."] <- "face" # change annoying name to nice one
# 
# onsettimes3 <- data.frame(c(0.04, 1.96, 7.52, 9.72) , c('Face1', 'Face2', 'Face3', 'Face4'))
# names(onsettimes3)[names(onsettimes3)=="c.0.04..1.96..7.52..9.72."] <- "time" # change annoying name to nice one
# names(onsettimes3)[names(onsettimes3)=="c..Face1....Face2....Face3....Face4.."] <- "face" # change annoying name to nice one
