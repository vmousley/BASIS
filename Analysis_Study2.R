### Analysis code for face pop-out
# Last updated 26 March 2019
# "Bilingual and Monolingual Infants’ Attention Capture and Maintenance For Faces"

# Dependencies ------------------------------------------------------------
# load all packages you need 
require("praise") 
require("lme4")
require ("ggplot2")
require("readxl")
require("reshape2")
require("tidyverse")
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
require("papaja")
# require("rjags")
# Pre-processing 1 ----------------------------------------------------------
df <- read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/popout/popout_wide_20200318T155557.xlsx') # read in tall excel file from lm_analysis folder 
df[df == 65535] <- NA # replace all 65535s with NA - 65535 is the automatic number used for missing data with lm scripts
# df[df == 'A012'] <- 'P012' # fix ID numbers
# df[df == 'P11'] <- 'P011'
# df[df == 'B01'] <- 'B001'

# Exclusion:
## 1) trials = more than 1 second of looking
## 2) babies who have fewer than 5 trials

# 1) TRIALS > 1 SECOND
df$time_total <- NA # creating column of total trial length (per trial)
for (row in 1:nrow(df)){ # for a range of numbers between 1 and # of rows in df, iterate through each number 
  df$time_total[row] = sum(df$face_timeInAOI[row], df$car_timeInAOI[row], # sum the values of each AOI in each row
                           df$phone_timeInAOI[row], df$noise_timeInAOI[row],
                           df$bird_timeInAOI[row])
}
df1 <- subset(df, df$time_total > 1) # for rows where time_total > 1, keep a subset of it and put it in df1

# 2) 5 TRIALS PER BABY
a <- data.frame(table(df1$id)) # take list of df1 id, count distinct ids, show how many trials each participant has
mylist <- vector() # make empty vector called mylist 
for (row in 1:nrow(a)) { # for a range of numbers between 1 and # of rows in a, iterate through each number
  id <- toString(a[row, "Var1"]) # look @ Var1 values for each row in object a and assign them to object id as a string 
  freq  <- a[row, "Freq"] # look @ freq values for each row in object a and assign them to object freq 
  if(freq <= 5) { # if the freq < 5
    mylist <- c(mylist, id) # then put ID number in mylist
    print(paste(id, "is less than 5")) # then tell us which ID < 5
  } else { # if freq > 5
    next # then go to next one
  }
}

mydf <- df1[!df1$id %in% mylist, ] # if df1 IDs are in mylist, do not include those rows in mydf
mydf <- mydf %>% arrange(id) # group rows by ID 

# Pre-processing 2 ----------------------------------------------------------------

# make empty vectors to calculate LATENCY measures 
flatlist <- vector() # face latency measure
clatlist <- vector() # car latency measure
platlist <- vector() # phone latency measure
nlatlist <- vector() # noise latency measure
blatlist <- vector() # bird latency measure
nonflatlist <- vector() # global 'non-face' latency measure

# make empty vectors to calculate FIXATION COUNT measures
fcountlist <- vector() # face fixation count
ccountlist <- vector() # car fixation count
pcountlist <- vector() # phone fixation count
ncountlist <- vector() # noise fixation count
bcountlist <- vector() # bird fixation count
nonfcountlist <- vector() # global 'non-face' count measure

# make empty vectors to calculate FIXATION DURATION measures
fdurlist <- vector()
cdurlist <- vector()
pdurlist <- vector()
ndurlist <- vector()
bdurlist <- vector()
nonfdurlist <- vector()

ids <- unique(mydf$id) # make a list of unique ids in mydf
idList <- vector() # make empty vector of idList

for (x in ids){ # for every id in object ids
  dfLoop <- mydf[which(mydf$id %in% c(x)),] # make a dataframe of trials for each id in mydf
  idList <- c(idList,x) # add id values to list to make column of ids
  
  flat <- mean(dfLoop$face_firstTimeS, na.rm = TRUE) # latency face measure, ignore empty
  flatlist <- c(flatlist, flat) # put mean value in an ordered list
  
  clat <- mean(dfLoop$car_firstTimeS, na.rm = TRUE) # latency car measure, ignore empty
  clatlist <- c(clatlist, clat) # put mean value in an ordered list
  
  plat <- mean(dfLoop$phone_firstTimeS, na.rm = TRUE) # latency phone measure, ignore empty
  platlist <- c(platlist, plat) # put mean value in an ordered list
  
  nlat <- mean(dfLoop$noise_firstTimeS, na.rm = TRUE) # latency noise measure, ignore empty
  nlatlist <- c(nlatlist, nlat) # put mean value in an ordered list
  
  blat <- mean(dfLoop$bird_firstTimeS, na.rm = TRUE) # latency bird measure, ignore empty
  blatlist <- c(blatlist, blat) # put mean value in an ordered list
  
  nonflat <- mean(clat, plat, nlat, blat, na.rm = TRUE) # global non-face latency measure, ignore empty
  nonflatlist <- c(nonflatlist, nonflat) # put mean value in an ordered list
  
  fcount <- mean(dfLoop$face_numLooks, na.rm = TRUE) # fixation count face measure, ignore empty
  fcountlist <- c(fcountlist, fcount) # put mean value in an ordered list
  
  ccount <- mean(dfLoop$car_numLooks, na.rm = TRUE) # fixation count car measure, ignore empty
  ccountlist <- c(ccountlist, ccount) # put mean value in an ordered list
  
  pcount <- mean(dfLoop$phone_numLooks, na.rm = TRUE) # fixation count phone measure, ignore empty
  pcountlist <- c(pcountlist, pcount) # put mean value in an ordered list
  
  ncount <- mean(dfLoop$noise_numLooks, na.rm = TRUE) # fixation count noise measure, ignore empty
  ncountlist <- c(ncountlist, ncount) # put mean value in an ordered list
  
  bcount <- mean(dfLoop$bird_numLooks, na.rm = TRUE) # fixation count bird measure, ignore empty
  bcountlist <- c(bcountlist, bcount) # put mean value in an ordered list
  
  nonfcount <- mean(ccount, pcount, ncount, bcount, na.rm = TRUE) # fixation count non-face measure, ignore empty
  nonfcountlist <- c(nonfcountlist, nonfcount) # # put mean value in an ordered list

  fdur <- mean(dfLoop$face_timeInAOI, na.rm = TRUE)
  fdurlist <- c(fdurlist, fdur)
  
  cdur <- mean(dfLoop$car_timeInAOI, na.rm = TRUE)
  cdurlist <- c(cdurlist, cdur)
    
  pdur <- mean(dfLoop$phone_timeInAOI, na.rm = TRUE)
  pdurlist <- c(pdurlist, pdur)
    
  ndur <- mean(dfLoop$noise_timeInAOI, na.rm = TRUE)
  ndurlist <- c(ndurlist, ndur)
    
  bdur <- mean(dfLoop$bird_timeInAOI, na.rm = TRUE)
  bdurlist <- c(bdurlist, bdur)

  nonfdur <- mean(cdur, pdur, ndur, bdur, na.rm = TRUE)
  nonfdurlist <- c(nonfdurlist, nonfdur)
}

s <- data.frame(ids, flatlist, clatlist, platlist, nlatlist, blatlist, nonflatlist, 
                fcountlist, ccountlist, pcountlist, ncountlist, bcountlist, nonfcountlist,
                fdurlist, cdurlist, pdurlist, ndurlist, bdurlist, nonfdurlist) # make a data frame of all ordered lists

behavedata <- read_xlsx('/Users/victoriamousley/Documents/MATLAB/IDs.xlsx') # load metadata about each participant from server
gender <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx'))

meta <- data.frame(behavedata$'ID no', behavedata$Age, behavedata$Group) # make new data frame with ids, age, and group
names(meta)[names(meta)=="behavedata..ID.no."] <- "ids" # change annoying name to nice one
names(meta)[names(meta)=="behavedata.Age"] <- "age" # change annoying name to nice one
names(meta)[names(meta)=="behavedata.Group"] <- "group" # change annoying name to nice one

# merge s and meta so i have all the behavioural and ET results
idSplitter = function(x) unlist(str_split(x,"_"))[1]
s$ids = sapply(s$ids, idSplitter)

s$ids[!s$ids %in% meta$ids] # see what got missed
s[s == 'P11'] <- 'P011' # rename what got missed
s[s == 'B01'] <- 'B001' # rename what got missed
s[s == 'A012'] <- 'P012' # rename what got missed

sMeta = merge(s, meta, by = 'ids')
gender <- gender[(as.character(gender$ID) %in% as.character(sMeta$ids)),]
sMeta$gender <- as.factor(gender$gender) # 2 = f, 1 = m

### Descriptives ----------------------------------------------------
# descriptive table
age <- list(sMeta$age)
group <- list(sMeta$group)
gender <- list(sMeta$gender)
descriptives <- data.frame(age, group, gender)
names(descriptives)[1] <- "age"
names(descriptives)[2] <- "group"
names(descriptives)[3] <- "gender"
levels(descriptives$gender)[levels(descriptives$gender)==1] <- "Male"
levels(descriptives$gender)[levels(descriptives$gender)==2] <- "Female"

test <- data.frame(descriptives)
test$agem <- floor(test$age*0.0328767); test$agem
ptable <- table(test$agem, test$group); ptable 

# EXCLUDING OLDER BABIES FOR NOW
test$id <- sMeta$ids
test$group <- sMeta$group

descriptives <- descriptives[-c(7, 11, 16, 19, 29, 34, 39),] # these
# are 18 and 16mo monolinguals randomly excluded 
# to make more balanced groups bc of RONA ugh

# redo descriptives
test <- data.frame(descriptives)
test$agem <- floor(test$age*0.0328767); test$agem
ptable <- table(test$agem, test$group); ptable

sMeta <- sMeta[-c(7, 11, 16, 19, 29, 34, 39), ]
sMeta %>%
  count(group, sort = TRUE)

# M = 18, B = 18

female_bi_fp <- as.numeric(count(descriptives %>%
                                filter(group == 'B', gender == "Female")))
female_mono_fp <- as.numeric(count(descriptives %>% 
                                  filter(group == "M", gender == "Female")))
mono_total_fp <- as.numeric(count(descriptives %>% filter(group == "M")))
mono_mean_days_fp <- descriptives %>% 
  filter(group == "M", descriptives$age)
mono_mean_days_fp <- round(mean(mono_mean_days_fp$age), 2)
mono_mean_months_fp <- round(mono_mean_days_fp*0.0328767, 2)

bi_total_fp <- as.numeric(count(descriptives %>% filter(group == "B")))
bi_mean_days_fp <- descriptives %>% 
  filter(group == "B", descriptives$age)
bi_mean_days_fp <- round(mean(bi_mean_days_fp$age), 2)
bi_mean_months_fp <- round(bi_mean_days_fp*0.0328767, 2)

# different between groups' ages?
age_m1 <- aov(age ~ group, data = sMeta)
apa_age_m1_fp <- apa_print(age_m1)

# Exploring Variables -------------------------------------------------
mean(sMeta$flatlist) # 1.212128
median(sMeta$flatlist) # 1.085282
sd(sMeta$flatlist) # 0.669969
# 68% rule of thumb is that resulting numbers should be fairly
# close to the interval covered by M - SD and M + SD

# face latency: 
quantile(sMeta$flatlist, 0.16) #  
quantile(sMeta$flatlist, 0.84) # 

hist(sMeta$flatlist, col = 'steelblue')
abline(v=mean(sMeta$flatlist), lty=2, lwd= 2)

boxplot(sMeta$flatlist, col = 'steelblue')
outliers <- boxplot(sMeta$flatlist, col = 'steelblue')$out
sMeta[which(sMeta$flatlist %in% outliers),] # 

# car latency: 
mean(sMeta$clatlist) # 
median(sMeta$clatlist) # 
sd(sMeta$clatlist) #  
quantile(sMeta$clatlist, 0.16) # 
quantile(sMeta$clatlist, 0.84) # 

hist(sMeta$clatlist, col = 'steelblue')
abline(v=mean(sMeta$clatlist), lty=2, lwd= 2)

boxplot(sMeta$clatlist, col = 'steelblue')
outliers <- boxplot(sMeta$clatlist, col = 'steelblue')$out
sMeta[which(sMeta$clatlist %in% outliers),] # 

# phone latency: 
mean(sMeta$platlist) # 
median(sMeta$platlist) # 
sd(sMeta$platlist) # 
quantile(sMeta$platlist, 0.16) # 
quantile(sMeta$platlist, 0.84) # 

hist(sMeta$platlist, col = 'steelblue')
abline(v=mean(sMeta$platlist), lty=2, lwd= 2)

boxplot(sMeta$platlist, col = 'steelblue')
outliers <- boxplot(sMeta$platlist, col = 'steelblue')$out
sMeta[which(sMeta$platlist %in% outliers),] # NO OUTLIERS

# noise latency:
mean(sMeta$nlatlist) # 
median(sMeta$nlatlist) # 
sd(sMeta$nlatlist) # 
quantile(sMeta$nlatlist, 0.16) # 
quantile(sMeta$nlatlist, 0.84) # 

hist(sMeta$nlatlist, col = 'steelblue')
abline(v=mean(sMeta$nlatlist), lty=2, lwd= 2)

boxplot(sMeta$nlatlist, col = 'steelblue')
outliers <- boxplot(sMeta$nlatlist, col = 'steelblue')$out
sMeta[which(sMeta$nlatlist %in% outliers),] # B037

# bird latency: 
mean(sMeta$blatlist, na.rm = TRUE) # 
sd(sMeta$blatlist, na.rm = TRUE) # 
quantile(sMeta$blatlist, 0.16, na.rm = TRUE) # 
quantile(sMeta$blatlist, 0.84, na.rm = TRUE) # 

hist(sMeta$blatlist, col = 'steelblue')
abline(v=mean(sMeta$blatlist), lty=2, lwd= 2)

boxplot(sMeta$blatlist, col = 'steelblue')
outliers <- boxplot(sMeta$blatlist, col = 'steelblue')$out
sMeta[which(sMeta$blatlist %in% outliers),] # NO OUTLIERS

mean(sMeta$fcountlist)
sd(sMeta$fcountlist)
quantile(sMeta$fcountlist, 0.16) #  
quantile(sMeta$fcountlist, 0.84) # 

hist(sMeta$fcountlist, col = 'steelblue')
abline(v=mean(sMeta$fcountlist), lty=2, lwd= 2)

boxplot(sMeta$fcountlist, col = 'steelblue')
outliers <- boxplot(sMeta$fcountlist, col = 'steelblue')$out
sMeta[which(sMeta$fcountlist %in% outliers),] # B048

mean(sMeta$ccountlist)
sd(sMeta$ccountlist)
quantile(sMeta$ccountlist, 0.16) #  
quantile(sMeta$ccountlist, 0.84) # 

hist(sMeta$ccountlist, col = 'steelblue')
abline(v=mean(sMeta$ccountlist), lty=2, lwd= 2)

boxplot(sMeta$ccountlist, col = 'steelblue')
outliers <- boxplot(sMeta$ccountlist, col = 'steelblue')$out
sMeta[which(sMeta$ccountlist %in% outliers),] # B013

mean(sMeta$pcountlist)
sd(sMeta$pcountlist)
quantile(sMeta$pcountlist, 0.16) # 
quantile(sMeta$pcountlist, 0.84) # 

hist(sMeta$pcountlist, col = 'steelblue')
abline(v=mean(sMeta$pcountlist), lty=2, lwd= 2)

boxplot(sMeta$pcountlist, col = 'steelblue')
outliers <- boxplot(sMeta$pcountlist, col = 'steelblue')$out
sMeta[which(sMeta$pcountlist %in% outliers),] # NO OUTLIERS

mean(sMeta$ncountlist)
sd(sMeta$ncountlist)
quantile(sMeta$ncountlist, 0.16) # 
quantile(sMeta$ncountlist, 0.84) # 

hist(sMeta$ncountlist, col = 'steelblue')
abline(v=mean(sMeta$ncountlist), lty=2, lwd= 2)

boxplot(sMeta$ncountlist, col = 'steelblue')
outliers <- boxplot(sMeta$ncountlist, col = 'steelblue')$out
sMeta[which(sMeta$ncountlist %in% outliers),] # NO OUTLIERS

mean(sMeta$bcountlist)
sd(sMeta$bcountlist)
quantile(sMeta$bcountlist, 0.16) # 
quantile(sMeta$bcountlist, 0.84) # 

hist(sMeta$bcountlist, col = 'steelblue')
abline(v=mean(sMeta$bcountlist), lty=2, lwd= 2)

boxplot(sMeta$bcountlist, col = 'steelblue')
outliers <- boxplot(sMeta$bcountlist, col = 'steelblue')$out
sMeta[which(sMeta$bcountlist %in% outliers),] # B028 and B029

# Assumptions of ANOVA ----------------------------------------------------
# 1: Random sampling
# 2: Equal variance
# 3: Independence of errors
# 4: Normal distribution of errors
# 5: Additivity of treatment effects

## NTS: check all assumptions

# Pre-registered hypotheses, latency -----------------------------------------------

# BY GROUP ----
lat2 <- data.frame(sMeta$ids, sMeta$group, sMeta$age, sMeta$flatlist, sMeta$nonflatlist)
names(lat2)[names(lat2)=="sMeta.ids"] <- "id" # change annoying name to nice one
names(lat2)[names(lat2)=="sMeta.group"] <- "group" # change annoying name to nice one
names(lat2)[names(lat2)=="sMeta.age"] <- "age" # change annoying name to nice one
names(lat2)[names(lat2)=="sMeta.flatlist"] <- "face" # change annoying name to nice one
names(lat2)[names(lat2)=="sMeta.nonflatlist"] <- "nonface" # change annoying name to nice one

lat2 <- arrange(melt(lat2,id=c('id', 'group', 'age')), by.x=id) # format data for aov
names(lat2)[names(lat2)=="value"] <- "lat" # change annoying name to nice one

## Plot with just face & non-face by group
latencyplot <- lat2 %>% ggplot(aes(x = variable, y = lat, fill = group))+
  geom_boxplot() + 
  labs(y='Fixation Latency (sec)', x = 'Areas of Interest', title = 'Fixation Latency to Face vs Non-Face AOIs', subtitle = 'Bilingual and monolingual 15- to 18-month-olds', fill = 'Group') +
  scale_x_discrete(labels=c('Face', 'Non-Face')) +
  scale_fill_manual(values=c('lightblue', 'lightslategrey'), labels=c('Bilingual', 'Monolingual')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right'); latencyplot

lat2aov2 <- aov(lat ~ variable*group, data = lat2)
summary(lat2aov2)
lat2aov2 <- apa_print(lat2aov2) # EVELYNE: this command is for RMarkdown, so you can ignore it :) 

# planned comparisons
t_car <- t.test(sMeta$flatlist, sMeta$clatlist) # sig
t_phone <- t.test(sMeta$flatlist, sMeta$platlist) # sig
t_noise <- t.test(sMeta$flatlist, sMeta$nlatlist) # sig
t_bird <- t.test(sMeta$flatlist, sMeta$blatlist) # sig

lat_t_car <- apa_print(t_car)
lat_t_phone <- apa_print(t_phone)
lat_t_noise <- apa_print(t_noise)
lat_t_bird <- apa_print(t_bird)
lat_t_tests <- map_df(list(t_car, t_phone, t_noise, t_bird), tidy)
names(lat_t_tests)[names(lat_t_tests)=="statistic"] <- "T-Statistic"
names(lat_t_tests)[names(lat_t_tests)=="p.value"] <- "P-Value"
names(lat_t_tests)[names(lat_t_tests)=="conf.low"] <- "CI (lower bound)"
names(lat_t_tests)[names(lat_t_tests)=="conf.high"] <- "CI (upper bound)"
lat_t_tests$AOI <- c('Car', 'Phone', 'Noise', 'Bird')

# plot all AOIs
lat5 <- data.frame(sMeta$ids, sMeta$group, sMeta$age, sMeta$flatlist, sMeta$clatlist, sMeta$platlist,
                   sMeta$nlatlist, sMeta$blatlist)

names(lat5)[names(lat5)=="sMeta.ids"] <- "ids" # change annoying name to nice one
names(lat5)[names(lat5)=="sMeta.age"] <- "age" # change annoying name to nice one
names(lat5)[names(lat5)=="sMeta.group"] <- "group" # change annoying name to nice one
names(lat5)[names(lat5)=="sMeta.flatlist"] <- "face" # change annoying name to nice one
names(lat5)[names(lat5)=="sMeta.clatlist"] <- "car" # change annoying name to nice one
names(lat5)[names(lat5)=="sMeta.nlatlist"] <- "noise" # change annoying name to nice one
names(lat5)[names(lat5)=="sMeta.platlist"] <- "phone" # change annoying name to nice one
names(lat5)[names(lat5)=="sMeta.blatlist"] <- "bird" # change annoying name to nice one

lat5 <- arrange(melt(lat5, id=c('ids', 'group', 'age'))) # format data for aov
names(lat5)[names(lat5)=="value"] <- "lat" # change annoying name to nice one

lat5plot <- lat5 %>% ggplot(aes(x = variable, y = lat, fill = variable))+
  geom_boxplot() +
  labs(y='Fixation Latency (sec)', x = 'Areas of Interest', title = 'Fixation Latency to AOIs', subtitle = 'All Participants') +
  scale_x_discrete(labels=c('Face', 'Car', 'Phone', 'Noise', 'Bird')) +
  scale_fill_manual(values=c('lightblue', 'lightcoral', 'lightseagreen', 'lightgoldenrod', 'lightpink')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'none'); lat5plot

# Pre-registered hypotheses, maintenance -----------------------------------------------
  # MODEL: anova -- fixation count measures ~ stimulus * group (stim = 5 levels)

### MODEL: stimulus is 2 levels (face vs non-face)
count2 <- data.frame(sMeta$ids, sMeta$group, sMeta$age, sMeta$fcountlist, sMeta$nonfcountlist)
names(count2)[names(count2)=="sMeta.ids"] <- "id" # change annoying name to nice one
names(count2)[names(count2)=="sMeta.group"] <- "group" # change annoying name to nice one
names(count2)[names(count2)=="sMeta.age"] <- "age" # change annoying name to nice one
names(count2)[names(count2)=="sMeta.fcountlist"] <- "face" # change annoying name to nice one
names(count2)[names(count2)=="sMeta.nonfcountlist"] <- "nonface" # change annoying name to nice one

count2 <- arrange(melt(count2,id=c('id','group','age')), by.x=id)
names(count2)[names(count2)=='value'] <- 'count'
count2aov2 <- aov(count ~ variable*group, data = count2)
summary(count2aov2)
count2aov2 <- apa_print(count2aov2)

count5 <- data.frame(sMeta$ids, sMeta$group, sMeta$age, sMeta$fcountlist, sMeta$ccountlist,
                     sMeta$pcountlist, sMeta$ncountlist, sMeta$bcountlist)
names(count5)[names(count5)=="sMeta.ids"] <- "id" # change annoying name to nice one
names(count5)[names(count5)=="sMeta.group"] <- "group" # change annoying name to nice one
names(count5)[names(count5)=="sMeta.age"] <- "age" # change annoying name to nice one
names(count5)[names(count5)=="sMeta.fcountlist"] <- "face" # change annoying name to nice one
names(count5)[names(count5)=="sMeta.ccountlist"] <- "car" # change annoying name to nice one
names(count5)[names(count5)=="sMeta.pcountlist"] <- "phone" # change annoying name to nice one
names(count5)[names(count5)=="sMeta.ncountlist"] <- "noise" # change annoying name to nice one
names(count5)[names(count5)=="sMeta.bcountlist"] <- "bird" # change annoying name to nice one

count5b <- arrange(melt(count5,id=c('id', 'group', 'age')), by.x=id)
names(count5b)[names(count5b)=="value"] <- "count"
count5b <- arrange(melt(count5,id=c('id', 'group', 'age')), by.x=id)
names(count5b)[names(count5b)=="value"] <- "count"

## Table with fixation count to face vs non-face AOIs
count2b <- tidyr::pivot_wider(count2, names_from = variable, values_from = count)
count2table <- as.data.frame(matrix(ncol = 3, nrow = 2))
names(count2table)[names(count2table)=="V1"] <- "Areas of Interest" # change annoying name to nice one
names(count2table)[names(count2table)=="V2"] <- "Mean" # change annoying name to nice one
names(count2table)[names(count2table)=="V3"] <- "SD" # change annoying name to nice one
count2table$`Areas of Interest` <- (c('Face', 'Non-Face'))
count2table$Mean <- round(c(mean(count2b$face), mean(count2b$nonface)), digits = 2)
count2table$SD <- round(c(sd(count2b$face), sd(count2b$nonface)), digits = 2)
count2table <- as.data.frame(count2table) # bc RMarkdown is being grumpy

## Plot with face & non-face for all participants
count2plot <- count2 %>% ggplot(aes(x = variable, y = count, fill = group)) +
  geom_boxplot() + 
  labs(y='Number of Visits', x = 'Areas of Interest', title = 'Fixation Count to Areas of Interest', subtitle = 'All Participants', fill = 'Group') +
  scale_x_discrete(labels=c('Face', 'Non-Face')) +
  scale_fill_manual(values=c('lightblue', 'gray'), labels=c('Bilingual', 'Monolingual')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right'); count2plot

t_car_fix <- t.test(sMeta$fcountlist, sMeta$ccountlist)
t_phone_fix <- t.test(sMeta$fcountlist, sMeta$pcountlist) 
t_noise_fix <- t.test(sMeta$fcountlist, sMeta$ncountlist) 
t_bird_fix <- t.test(sMeta$fcountlist, sMeta$bcountlist) 

t_car_fix <- apa_print(t_car_fix)
t_phone_fix <- apa_print(t_phone_fix)
t_noise_fix <- apa_print(t_noise_fix)
t_bird_fix <- apa_print(t_bird_fix)
lat_t_tests_fix <- map_df(list(t_car, t_phone, t_noise, t_bird), tidy)
names(lat_t_tests_fix)[names(lat_t_tests_fix)=="statistic"] <- "T-Statistic" 
names(lat_t_tests_fix)[names(lat_t_tests_fix)=="p.value"] <- "P-Value" 
names(lat_t_tests_fix)[names(lat_t_tests_fix)=="conf.low"] <- "CI (lower bound)"
names(lat_t_tests_fix)[names(lat_t_tests_fix)=="conf.high"] <- "CI (upper bound)" 
lat_t_tests_fix$AOI <- c('Car', 'Phone', 'Noise', 'Bird')

## Graphical representation of count t-tests
allcountplot <- count5b %>% ggplot(aes(x = variable, y = count, fill = variable)) +
  geom_boxplot() + 
  labs(y='Number of Visits', x = 'Areas of Interest', title = 'Fixation Count to Areas of Interest', subtitle = 'All Participants') +
  scale_x_discrete(labels=c('Face', 'Car', 'Phone', 'Noise', 'Bird')) +
  scale_fill_manual(values=c('lightblue', 'lightcoral', 'lightseagreen', 'lightgoldenrod', 'lightpink')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'none'); allcountplot

## Fixation Duration ------------------------
dur2 <- data.frame(sMeta$ids, sMeta$group, sMeta$age, sMeta$fdurlist, sMeta$nonfdurlist)
names(dur2)[names(dur2)=="sMeta.ids"] <- "id" # change annoying name to nice one
names(dur2)[names(dur2)=="sMeta.group"] <- "group" # change annoying name to nice one
names(dur2)[names(dur2)=="sMeta.age"] <- "age" # change annoying name to nice one
names(dur2)[names(dur2)=="sMeta.fdurlist"] <- "face" # change annoying name to nice one
names(dur2)[names(dur2)=="sMeta.nonfdurlist"] <- "nonface" # change annoying name to nice one

dur2 <- arrange(melt(dur2,id=c('id', 'group', 'age')), by.x=id) # format data for aov
names(dur2)[names(dur2)=="value"] <- "dur" # change annoying name to nice one

dur2aov2 <- aov(dur ~ variable*group, data = dur2) # aov with 2 variable levels
summary(dur2aov2)
dur2aov2 <- apa_print(dur2aov2)

durface <- dur2 %>% ggplot(aes(x = variable, y = dur, fill = group)) +
  geom_boxplot() + 
  labs(y='Fixation Duration (sec)', x = 'Areas of Interest', title = 'Fixation Duration to Face vs Non-Face AOIs by Group', fill = 'Group') + 
  scale_fill_manual(values=c('lightblue', 'gray'), labels=c('Bilingual', 'Monolingual')) +
  scale_x_discrete(labels=c('Face', 'Non-Face')) + 
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(family = 'Helvetica', size = (10)), 
        legend.title = element_text(family = 'Helvetica', face = 'bold', size = (10)),
        legend.text = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right'); durface

dur5 <- data.frame(sMeta$ids, sMeta$group, sMeta$age, sMeta$fdurlist, sMeta$cdurlist, sMeta$pdurlist,
                   sMeta$ndurlist, sMeta$bdurlist)

names(dur5)[names(dur5)=="sMeta.ids"] <- "ids" # change annoying name to nice one
names(dur5)[names(dur5)=="sMeta.age"] <- "age" # change annoying name to nice one
names(dur5)[names(dur5)=="sMeta.group"] <- "group" # change annoying name to nice one
names(dur5)[names(dur5)=="sMeta.fdurlist"] <- "face" # change annoying name to nice one
names(dur5)[names(dur5)=="sMeta.cdurlist"] <- "car" # change annoying name to nice one
names(dur5)[names(dur5)=="sMeta.ndurlist"] <- "noise" # change annoying name to nice one
names(dur5)[names(dur5)=="sMeta.pdurlist"] <- "phone" # change annoying name to nice one
names(dur5)[names(dur5)=="sMeta.bdurlist"] <- "bird" # change annoying name to nice one

dur5 <- arrange(melt(dur5, id=c('ids', 'group', 'age'))) # format data for aov
names(dur5)[names(dur5)=="value"] <- "dur" # change annoying name to nice one

alldurplot <- dur5 %>% ggplot(aes(x = variable, y = dur, fill = variable)) +
  geom_boxplot() +
  labs(y='Length of Looking (sec)', x = 'Areas of Interest', title = 'Fixation Duration to Areas of Interest', subtitle = 'All Participants') +
  scale_x_discrete(labels=c('Face', 'Car', 'Phone', 'Noise', 'Bird')) +
  scale_fill_manual(values=c('lightblue', 'lightcoral', 'lightseagreen', 'lightgoldenrod', 'lightpink')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'none'); alldurplot

t_car <- t.test(sMeta$fdurlist, sMeta$cdurlist) # sig
t_phone <- t.test(sMeta$fdurlist, sMeta$pdurlist) # sig
t_noise <- t.test(sMeta$fdurlist, sMeta$ndurlist) # sig
t_bird <- t.test(sMeta$fdurlist, sMeta$bdurlist) # sig

dur_t_car <- apa_print(t_car)
dur_t_phone <- apa_print(t_phone)
dur_t_noise <- apa_print(t_noise)
dur_t_bird <- apa_print(t_bird)
dur_t_tests <- map_df(list(t_car, t_phone, t_noise, t_bird), tidy)
names(dur_t_tests)[names(dur_t_tests)=="statistic"] <- "T-Statistic"
names(dur_t_tests)[names(dur_t_tests)=="p.value"] <- "P-Value"
names(dur_t_tests)[names(dur_t_tests)=="conf.low"] <- "CI (lower bound)"
names(dur_t_tests)[names(dur_t_tests)=="conf.high"] <- "CI (upper bound)"
dur_t_tests$AOI <- c('Car', 'Phone', 'Noise', 'Bird')

### "Face orientation measure" --------------
# non-face - face / non-face + face

sMeta$FOM <- (sMeta$nonflatlist - sMeta$flatlist) / (sMeta$flatlist + sMeta$nonflatlist)
FOM <- ggplot(sMeta, aes(age, FOM, colour = group)) +
  geom_smooth(method="lm", se = T, size = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y='Face Orientaiton Index', x = 'Age (days)', title = 'Face Orientation Index',
       subtitle = 'All Participants', fill = "Group") +
  theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(family = 'Helvetica', size = (10)),
        axis.title.y = element_text(family = 'Helvetica', size = (10)),
        legend.position = 'right') + 
  scale_colour_manual(values = c('navy', 'springgreen4'), labels=c('Bilingual', 'Monolingual')); FOM

FOM_m1 <- lm(FOM ~ group*age, data = sMeta)
summary(FOM_m1)
FOM_m1 <- apa_print(FOM_m1)

### Gender effect? (NTS: not done)------


### Degree of bilingualism ---------------------------------------------
names(gender)[names(gender)=="Group"] <- "group"
names(gender)[names(gender)=="Eng.exp"] <- "Eng%"
names(gender)[names(gender)=="Second.lang.exp"] <- "2ndlang%"
names(gender)[names(gender)=="lang.mixing.scale"] <- "langmix"
names(gender)[names(gender)=="CDI.Eng"] <- "CDI"

### Behavioural correlations (NTS: not done) ---------------------------------------------

# Bayesian Exploration (NTS: not done) ----------------------------------------------------
###### ISSUE INSTALLING RJAGS

## First model 
# mod = function(){
#   # priors
#   b0~dnorm(0,.001)
#   z~dnorm(0,.001)
#   sigma~dunif(0,100)
#   tau<-1/(sigma*sigma)
#   # likelihood
#   for (i in 1:18225){
#     mu[i]<-b0+3000*exp(-z*time[i])
#     Price[i]~dnorm(mu[i], tau)
#     Price_pred[i]~dnorm(mu[i], tau)
#   }
# }
# 
# # write model
# model.file = "model.txt"
# write.model(mod, model.file)
# 
# # no iniital values
# inits <- NULL
# 
# # what parameters we want to track 
# params = c("tau", "z", "b0", "Price_pred")
# 
# ## hyperparameters
# # number of interactions
# ni = 10000
# # burn in interval
# nb = 1000
# # thinning interval
# nt = 1
# # number of chains
# nc = 3
# 
# # compile model 
# jmod = jags.model(file = model.file, 
#                   data = df, n.chains = nc,
#                   inits = inits, n.adapt = 1000)


# Graveyard (NTS: this is where i put code i'm not using anymore----- 


# count5b <- arrange(melt(count5,id=c('id', 'group', 'age')), by.x=id)
# names(count5b)[names(count5b)=="value"] <- "count"
# lat5aov <- aov(lat ~ variable, data = lat5) # aov with 5 variable levels
# summary(lat5aov) # if significant, do planned post-hoc comparisons (t-tests)
# lat5aov <- apa_print(lat5aov)

## Plot with all AOIs for all participants
# lat5 <- filter(lat5, variable!='non-face')
# facelat <- subset(lat2, variable=='face')
# # Table with Fixation Latencies to Face vs Non-Face
# lat2b <- tidyr::pivot_wider(lat2, names_from = variable, values_from = lat)
# lat2table <- as.data.frame(matrix(ncol = 5, nrow = 2))
# names(lat2table)[names(lat2table)=="V1"] <- "Groups" # change annoying name to nice one
# names(lat2table)[names(lat2table)=="V2"] <- "Face(M)" # change annoying name to nice one
# names(lat2table)[names(lat2table)=="V3"] <- "Face(SD)" # change annoying name to nice one
# names(lat2table)[names(lat2table)=="V4"] <- "Non-Face(M)"
# names(lat2table)[names(lat2table)=="V5"] <- "Non-Face(SD)"
# lat2table$Groups <- (c('Monolingual', 'Bilingual'))

# ## Table with Fixation Latencies to All AOIs
# lat5b <- tidyr::pivot_wider(lat5, names_from = variable, values_from = lat)
# lat5table <- as.data.frame(matrix(ncol = 3, nrow = 5))
# names(lat5table)[names(lat5table)=="V1"] <- "Areas of Interest" # change annoying name to nice one
# names(lat5table)[names(lat5table)=="V2"] <- "Mean" # change annoying name to nice one
# names(lat5table)[names(lat5table)=="V3"] <- "SD" # change annoying name to nice one
# lat5table$`Areas of Interest` <- (c('Face', 'Car', 'Phone', 'Noise', 'Bird'))
# lat5table$Mean <- round(c(mean(lat5b$face), mean(lat5b$car), 
#                      mean(lat5b$phone), mean(lat5b$noise),
#                      mean(lat5b$bird)), digits = 2)
# lat5table$SD <- round(c(sd(lat5b$face), sd(lat5b$car), 
#                      sd(lat5b$phone), sd(lat5b$noise),
#                      sd(lat5b$bird)), digits = 2)
# lat5table <- as.data.frame(lat5table) # bc RMarkdown is being grumpy
# 
# ### MODEL: anova -- fixation latency measure ~ stimulus (2 levels, face vs non-face)
# lat2aov <- aov(lat ~ variable, data = lat2) # aov with 2 variable levels
# summary(lat2aov) # if significant, do planned post-hoc comparisons (t-tests)
# lat2aov <- apa_print(lat2aov)

## Table with Fixation Latencies to Face vs Non-Face
# lat2b <- tidyr::pivot_wider(lat2, names_from = variable, values_from = lat)
# lat2table <- as.data.frame(matrix(ncol = 3, nrow = 2))
# names(lat2table)[names(lat2table)=="V1"] <- "Areas of Interest" # change annoying name to nice one
# names(lat2table)[names(lat2table)=="V2"] <- "Mean" # change annoying name to nice one
# names(lat2table)[names(lat2table)=="V3"] <- "SD" # change annoying name to nice one
# lat2table$`Areas of Interest` <- (c('Face', 'Non-Face'))
# lat2table$Mean <- round(c(mean(lat2b$face), mean(lat2b$nonface)), digits = 2)
# lat2table$SD <- round(c(sd(lat2b$face), sd(lat2b$nonface)), digits = 2)
# lat2table <- as.data.frame(lat2table) # bc RMarkdown is being grumpy

# HYPOTHESIS: group interaction of variable x group
# lat5aov2 <- aov(lat ~ variable*group, data = lat5)
# summary(lat5aov2) # if significant, do planned post-hoc comparisons (t-tests)
# 
## bilinguals should have significantly shorter lat to face than monolingual
# difference between monolingual and bilinguals' latency to face ROI?

# mono <- subset(facelat, group=='M')
# bi <- subset(facelat, group=='B')
# t.test(mono$lat, bi$lat) # borderline significant

# ## Table with Fixation Latencies to Face vs Non-Face by Group
# lat2tabledesc <- as.data.frame(matrix(ncol = 3, nrow = 2))
# names(lat2tabledesc)[names(lat2tabledesc)=="V1"] <- "Areas of Interest" # change annoying name to nice one
# names(lat2tabledesc)[names(lat2tabledesc)=="V2"] <- "Mean" # change annoying name to nice one
# names(lat2tabledesc)[names(lat2tabledesc)=="V3"] <- "SD" # change annoying name to nice one
# lat2tabledesc$`Areas of Interest` <- (c('Face', 'Non-Face'))
# lat2tabledesc$Mean <- round(c(mean(lat2b$face), mean(lat2b$nonface)), digits = 2)
# lat2tabledesc$SD <- round(c(sd(lat2b$face), sd(lat2b$nonface)), digits = 2)
# lat2tabledesc <- as.data.frame(lat2tabledesc) # bc RMarkdown is being grumpy

# # investigating group as categorical predictor
# levels(sMeta$group)[levels(sMeta$group)=="b"] <- "Bilingual"
# levels(sMeta$group)[levels(sMeta$group)=="m"] <- "Monolingual"
# 
# latbox <- facelat %>% ggplot(aes(x = group, y = lat, fill = group)) +
#   geom_boxplot() + 
#   labs(y='Face Latency (sec)', title = 'Face Latency by Group') + 
#   guides(fill=guide_legend(title='Language Group')) +
#   scale_fill_manual(values=c('slateblue', 'gray'), labels=c('Bilingual', 'Monolingual')) +
#   scale_x_discrete(labels=c('Bilingual', 'Monolingual')) + 
#   theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(family = 'Helvetica', size = (10)), 
#         legend.title = element_text(family = 'Helvetica', face = 'bold', size = (10)),
#         legend.text = element_text(family = 'Helvetica', size = (10)),
#         legend.position = 'right'); latbox
# 
# latdens <- facelat %>% ggplot(aes(x = lat, fill = group)) +
#   geom_density(alpha = 0.5) +
#   labs(x = 'Face Latency (sec)', title = 'Face Latency Distributions by Group') + 
#   guides(fill=guide_legend(title='Language Group')) +
#   scale_fill_manual(values=c('slateblue', 'gray'), labels=c('Bilingual', 'Monolingual')) +
#   theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
#         axis.title.x = element_text(size = 10),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_blank(), 
#         legend.title = element_text(family = 'Helvetica', face = 'bold', size = (10)),
#         legend.text = element_text(family = 'Helvetica', size = (10)),
#         legend.position = 'right'); latdens


# count5aov <- aov(count ~ variable, data = count5b)
# count5aov <- apa_print(count5aov)
# 
# count5table <- as.data.frame(matrix(ncol = 3, nrow = 5))
# names(count5table)[names(count5table)=="V1"] <- "Areas of Interest" # change annoying name to nice one
# names(count5table)[names(count5table)=="V2"] <- "Mean" # change annoying name to nice one
# names(count5table)[names(count5table)=="V3"] <- "SD" # change annoying name to nice one
# count5table$`Areas of Interest` <- (c('Face', 'Car', 'Phone', 'Noise', 'Bird'))
# count5table$Mean <- round(c(mean(count5$face), mean(count5$car), 
#                           mean(count5$phone), mean(count5$noise),
#                           mean(count5$bird)), digits = 2)
# count5table$SD <- round(c(sd(count5$face), sd(count5$car), 
#                         sd(count5$phone), sd(count5$noise),
#                         sd(count5$bird)), digits = 2)
# count5table <- as.data.frame(count5table) # bc RMarkdown is being grumpy


### HYPOTHESIS: group interaction of variable x group
# count5aov2 <- aov(count ~ variable*group, data = count5b)
# summary(count5aov2) # if significant, do planned post-hoc comparisons (t-tests)
# # no effect of group
# 
# ## bilinguals should have significantly more counts to face than monolinguals
# facecount <- subset(count2, variable=='face')
# mono <- subset(facecount, group=='M')
# bi <- subset(facecount, group=='B')
# t.test(mono$count, bi$count)
# # no effect of group 

### plot group count to face ROI
# fixgroupbox <- count2 %>% ggplot(aes(x = group, y = count, fill = group)) +
#   geom_boxplot() + 
#   labs(y='Number of Visits to Face AOI', title = 'Visits to Face AOI by Group') + 
#   guides(fill=guide_legend(title='Language Group')) +
#   scale_fill_manual(values=c('slateblue', 'gray'), labels=c('Bilingual', 'Monolingual')) +
#   scale_x_discrete(labels=c('Bilingual', 'Monolingual')) + 
#   theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(family = 'Helvetica', size = (10)), 
#         legend.title = element_text(family = 'Helvetica', face = 'bold', size = (10)),
#         legend.text = element_text(family = 'Helvetica', size = (10)),
#         legend.position = 'right'); fixgroupbox
# no effect of group on number of visits to face AOI


# # linear models of face latency
# names(sMeta)[names(sMeta)=="flatlist"] <- "facelat" # change annoying name to nice one
# ggplot(sMeta, aes(x = age, y = facelat)) + 
#   geom_smooth(method='lm') +
#   ggtitle('Face latency ~ age')
# 
# ggplot(sMeta, aes(x=age, y=facelat, colour = group)) +
#   geom_smooth(method='lm') +
#   ggtitle('Face latency ~ age * group')
# 
# facelat_m1 <- lm(facelat ~ age, data = sMeta)
# glance(facelat_m1)$r.squared # this model accounts for very little of the variance in face latencies
# 
# facelat_m2 <- lm(facelat ~ age * group, data = sMeta)
# glance(facelat_m2)$r.squared # this model accounts for 16.8% of variance in face latencies
# 
# # bilinguals' bimodal distribution?
# bilat <- data.frame()
# bilat <- as.data.frame(sMeta[which(sMeta$group %in% c('B')),]$facelat)
# names(bilat)[1] <- "facelat" # change annoying name to nice one
# bilat$age <- sMeta[which(sMeta$group %in% c('B')),]$age
# names(bilat)[2] <- "age" # change annoying name to nice one
# bilat
# cor.test(bilat$facelat, bilat$age)
# 
# # linear models of face count
# names(sMeta)[names(sMeta)=="fcountlist"] <- "facecount" # change annoying name to nice one
# 
# # investigating group as categorical predictor
# countbox <- sMeta %>% ggplot(aes(x = group, y = facecount, fill = group)) +
#   geom_boxplot()
# 
# countdens <- sMeta %>% ggplot(aes(x = facecount, fill = group)) +
#   geom_density(alpha = 0.5)
# 
# plot1 <- countbox
# plot2 <- countdens
# plot_grid(plot1, plot2, labels = "AUTO")

# # durgroupbox <- dur2 %>% ggplot(aes(x = group, y = dur, fill = group)) +
# #   geom_boxplot() + 
# #   labs(y='Fixation Duration (sec)', title = 'Fixation Duration to Face vs Non-Face AOIs by Group') + 
# #   guides(fill=guide_legend(title='Language Group')) +
# #   scale_fill_manual(values=c('slateblue', 'gray'), labels=c('Bilingual', 'Monolingual')) +
# #   scale_x_discrete(labels=c('Bilingual', 'Monolingual')) + 
# #   theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
# #         axis.title.x = element_blank(),
# #         axis.text.x = element_text(size = 10),
# #         axis.title.y = element_text(family = 'Helvetica', size = (10)), 
# #         legend.title = element_text(family = 'Helvetica', face = 'bold', size = (10)),
# #         legend.text = element_text(family = 'Helvetica', size = (10)),
# #         legend.position = 'right'); durgroupbox
# 
# # dur5aov <- aov(dur ~ variable, data = dur5) # aov with 5 variable levels
# # dur5aov <- apa_print(dur5aov) # if significant, do planned post-hoc comparisons (t-tests)
# 
# 
# ### MODEL: anova -- fixation duration ~ stimulus (2 levels, face vs non-face)
# 
# # ## Plot with just face & non-face for all participants
# # dur2plot <- dur2 %>% ggplot(aes(x = variable, y = dur, fill = variable))+
# #   geom_boxplot() + 
# #   labs(y='Fixation Duration (sec)', x = 'Areas of Interest', title = 'Fixation Duration to Face vs Non-Face AOIs', subtitle = 'All Participants') +
# #   scale_x_discrete(labels=c('Face', 'Non-Face')) +
# #   scale_fill_manual(values=c('lightblue', 'lightslategrey')) +
# #   theme(plot.title = element_text(hjust = 0.5, family = 'Helvetica', face = 'bold', size = (14)),
# #         plot.subtitle = element_text(hjust = 0.5, family = 'Helvetica', size = (12)),
# #         axis.title.x = element_text(size = 10),
# #         axis.text.x = element_text(family = 'Helvetica', size = (10)),
# #         axis.title.y = element_text(family = 'Helvetica', size = (10)),
# #         legend.position = 'none'); dur2plot
# 
# ## bilinguals should have significantly more counts to face than monolinguals
# facecount <- subset(dur5, variable=='face')
# mono <- subset(facecount, group=='M')
# bi <- subset(facecount, group=='B')
# t_test_face <- t.test(mono$dur, bi$dur)

# no effect of group 



