### Analysis code for face pop-out
# Last updated 15 Nov 2019
# "Bilingual and Monolingual Infants’ Attention Capture and Maintenance For Faces"

# Dependencies ------------------------------------------------------------
# load all packages you need 
require("praise")
require("lme4")
require ("ggplot2")
require("readxl")
require("reshape2")
require("tidyverse")
# require("rjags")
require("coda")
require("MCMCvis")
require("ggpubr")
require("naniar")

# Pre-processing ----------------------------------------------------------
df <- read_xlsx('/Volumes/leap/MATLAB data/lm_preproc/lm_analysis/popout/popout_wide_20191118T143330.xlsx') # read in tall excel file from lm_analysis folder 
df[df == 65535] <- NA # replace all 65535s with NA - 65535 is the automatic number used for missing data with lm scripts
df[df == 'A012'] <- 'P012' # fix ID numbers
df[df == 'P11'] <- 'P011'
df[df == 'B01'] <- 'B001'

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
  if(freq < 5) { # if the freq < 5
    mylist <- c(mylist, id) # then put ID number in mylist
    print(paste(id, "is less than 5")) # then tell us which ID < 5
  } else { # if freq > 5
    next # then go to next one
  }
}
mydf <- df1[!df1$id %in% mylist, ] # if df1 IDs are in mylist, do not include those rows in mydf
mydf <- mydf %>% arrange(id) # group rows by ID 

# Analysis ----------------------------------------------------------------

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
nonfcountlist <- vector() # global 'non-face' latency measure

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
}

s <- data.frame(ids, flatlist, clatlist, platlist, nlatlist, blatlist, nonflatlist, 
                   fcountlist, ccountlist, pcountlist, ncountlist, bcountlist, nonfcountlist) # make a data frame of all ordered lists

behavedata <- read_xlsx('/Volumes/leap/Behavioural/IDs.xlsx') # load metadata about each participant from server
meta <- data.frame(behavedata$'ID no', behavedata$Age, behavedata$Group) # make new data frame with ids, age, and group
names(meta)[names(meta)=="behavedata..ID.no."] <- "id" # change annoying name to nice one
names(meta)[names(meta)=="behavedata.Age"] <- "age" # change annoying name to nice one
names(meta)[names(meta)=="behavedata.Group"] <- "group" # change annoying name to nice one

s <- merge(s, meta, by.x = 'ids', by.y = 'id') # merge by id numbers 

# Assumptions of ANOVA ----------------------------------------------------
# 1: Random sampling
# 2: Equal variance
# 3: Independence of errors
# 4: Normal distribution of errors
# 5: Additivity of treatment effects

## NTS: check all assumptions

# Pre-registered hypotheses -----------------------------------------------

# Attention capture
  # MODEL: anova -- fixation latency measure ~ stimulus (5 levels)
lat5 <- data.frame(s$ids, s$group, s$age, s$flatlist, s$clatlist, s$platlist, s$nlatlist, s$blatlist)
names(lat5)[names(lat5)=="s.ids"] <- "id" # change annoying name to nice one
names(lat5)[names(lat5)=="s.group"] <- "group" # change annoying name to nice one
names(lat5)[names(lat5)=="s.age"] <- "age" # change annoying name to nice one
names(lat5)[names(lat5)=="s.flatlist"] <- "face" # change annoying name to nice one
names(lat5)[names(lat5)=="s.clatlist"] <- "car" # change annoying name to nice one
names(lat5)[names(lat5)=="s.platlist"] <- "phone" # change annoying name to nice one
names(lat5)[names(lat5)=="s.nlatlist"] <- "noise" # change annoying name to nice one
names(lat5)[names(lat5)=="s.blatlist"] <- "bird" # change annoying name to nice one

lat5 <- arrange(melt(lat5,id=c('id', 'group', 'age')), by.x=id) # format data for aov
names(lat5)[names(lat5)=="value"] <- "lat" # change annoying name to nice one

lat5aov <- aov(lat ~ variable, data = lat5) # aov with 5 variable levels
summary(lataov) # if significant, do planned post-hoc comparisons (t-tests)

t.test(s$flatlist, s$clatlist)
boxplot(s$flatlist, s$clatlist)
t.test(s$flatlist, s$platlist) 
boxplot(s$flatlist, s$platlist)
t.test(s$flatlist, s$nlatlist) 
boxplot(s$flatlist, s$nlatlist)
t.test(s$flatlist, s$blatlist) 
boxplot(s$flatlist, s$blatlist)
t.test(s$flatlist, s$nonflatlist) 
boxplot(s$flatlist, s$nonflatlist)

  # MODEL: anova -- fixation latency measure ~ stimulus (2 levels, face vs non-face)
lat2 <- data.frame(s$ids, s$group, s$age, s$flatlist, s$nonflatlist)
names(lat2)[names(lat2)=="s.ids"] <- "id" # change annoying name to nice one
names(lat2)[names(lat2)=="s.group"] <- "group" # change annoying name to nice one
names(lat2)[names(lat2)=="s.age"] <- "age" # change annoying name to nice one
names(lat2)[names(lat2)=="s.flatlist"] <- "face" # change annoying name to nice one
names(lat2)[names(lat2)=="s.nonflatlist"] <- "nonface" # change annoying name to nice one

lat2 <- arrange(melt(lat2,id=c('id', 'group', 'age')), by.x=id) # format data for aov
names(lat2)[names(lat2)=="value"] <- "lat" # change annoying name to nice one

lat2aov <- aov(lat ~ variable, data = lat2) # aov with 2 variable levels
summary(lataov) # if significant, do planned post-hoc comparisons (t-tests)

  # HYPOTHESIS: face will be < all other 4 categories (car, phone, noise, bird)

  # then do anova -- fixation latency measure ~ stimulus * group (with fact of 5)
    # first with 5 levels of stimulus
lat5aov2 <- aov(lat ~ variable*group, data = lat5) 
summary(lat5aov2)

    # then with 2 levels of stimulus (face/non-face)
lat2aov2 <- aov(lat ~ variable*group, data = lat2)
summary(lat2aov2)

# Attention maintenance
  # MODEL: anova -- fixation count measures ~ stimulus * group (stim = 5 levels)
count5 <- data.frame(s$ids, s$group, s$age, s$fcountlist, s$ccountlist, s$pcountlist, s$ncountlist, s$bcountlist)
names(count5)[names(count5)=="s.ids"] <- "id" # change annoying name to nice one
names(count5)[names(count5)=="s.group"] <- "group" # change annoying name to nice one
names(count5)[names(count5)=="s.age"] <- "age" # change annoying name to nice one
names(count5)[names(count5)=="s.fcountlist"] <- "face" # change annoying name to nice one
names(count5)[names(count5)=="s.ccountlist"] <- "car" # change annoying name to nice one
names(count5)[names(count5)=="s.pcountlist"] <- "phone" # change annoying name to nice one
names(count5)[names(count5)=="s.ncountlist"] <- "noise" # change annoying name to nice one
names(count5)[names(count5)=="s.bcountlist"] <- "bird" # change annoying name to nice one

count5 <- arrange(melt(count,id=c('id', 'group', 'age')), by.x=id)
names(count5)[names(count5)=="value"] <- "count"

count5aov <- aov(count ~ variable, data = count5)
summary(count5aov)

  # MODEL: anova -- same as above, but stimulus is 2 levels (face vs non-face)
count2 <- data.frame(s$ids, s$group, s$age, s$fcountlist, s$nonfcountlist)
names(count2)[names(count2)=="s.ids"] <- "id" # change annoying name to nice one
names(count2)[names(count2)=="s.group"] <- "group" # change annoying name to nice one
names(count2)[names(count2)=="s.age"] <- "age" # change annoying name to nice one
names(count2)[names(count2)=="s.fcountlist"] <- "face" # change annoying name to nice one
names(count2)[names(count2)=="s.nonfcountlist"] <- "nonface" # change annoying name to nice one

count2 <- arrange(melt(count,id=c('id','group','age')), by.x=id)
names(count2)[names(count2)=='value'] <- 'count'

count2aov <- aov(count ~ variable, data = count2)
summary(count2aov)

  # HYPOTHESIS: face will be > all other 4 categories (car, phone, noise, bird)
t.test(s$fcountlist, s$ccountlist)  
boxplot(s$fcountlist, s$ccountlist)
t.test(s$fcountlist, s$pcountlist)
boxplot(s$fcountlist, s$pcountlist)
t.test(s$fcountlist, s$ncountlist)
boxplot(s$fcountlist, s$ncountlist)
t.test(s$fcountlist, s$bcountlist)
boxplot(s$fcountlist, s$bcountlist)

  # then do anova -- fixation count measures ~ stimulus * group with stim as 5
count5aov2 <- aov(count ~ variable*group, data = count5)
summary(count5aov2)

  # then with stim as 2 (face vs nonface)
count2aov2 <- aov(count ~ variable*group, data = count2)
summary(count2aov2)

# Exploratory analyses ----------------------------------------------------

# Degree of bilingualism

# Behavioural correlations

# Bayesian Exploration ----------------------------------------------------
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
