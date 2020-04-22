### Cleaning data for ML for face pop-out and 50 faces
# Last updated 23 March 2019 - PLANNING TO DO THIS IN MATLAB INSTEAD BC IT SEEMS SAFER

require("dplyr")
require("readxl")

### VID no.1 ------
face_aoi1 <- read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/face_AOI1.xlsx')

ids1 <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/ids1.xlsx'))
ids1 <- t(ids1)
time1 <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/time1.xlsx'))

df1 <- data.frame(face_aoi1)
colnames(df1) = ids1

df1$time <- time1
names(df1)[names(df1)=="time.Var1"] <- "time" # change annoying name to nice one

df1[,]=ifelse(df1 != 'TRUE',0,1)





### VID no. 2 ------
face_aoi2 <- read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/face_AOI2.xlsx')

ids2 <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/ids2.xlsx'))
ids2 <- t(ids2)
time2 <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/time2.xlsx'))

df2 <- data.frame(face_aoi2)
colnames(df2) = ids2

df2$time <- time2
names(df2)[names(df2)=="time.Var1"] <- "time" # change annoying name to nice one


### VID no. 3 ------
face_aoi3 <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/face_AOI3.xlsx'))

ids3 <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/ids3.xlsx'))
ids3 <- t(ids3)
time3 <- data.frame(read_xlsx('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/time3.xlsx'))

df3 <- data.frame(face_aoi3)
colnames(df3) = ids3

df3$time <- time3
names(df3)[names(df3)=="time.Var1"] <- "time" # change annoying name to nice one

