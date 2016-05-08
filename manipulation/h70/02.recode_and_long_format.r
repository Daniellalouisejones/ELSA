
#Clear the variables from previous runs.

rm(list=ls(all=TRUE))  

# Clear console

cat("\f")

# load packages

library(tidyr)
library(dplyr)

# read dataset

indata <- read.csv2("H70_Amsterdam_160429.csv", encoding = "UTF-8-BOM")

# rename IDvar

names(indata)[1] <- "id"

n_distinct(indata$id) # 1142 individuals

# rename avlage

names(indata)[6] <- "deathag"

# rename intage variables by including _ between the variable name and timepoint 
# for easier separation later.

names(indata)[3:5] <- c("age_w1","age_w2","age_w3")

# Recode MMSE to impairment -----------------------------------------------

# First wave

indata$state_w1 <- ifelse(indata$PE1371 >= 27 & indata$PE1371 <= 30, 1,
                            ifelse(indata$PE1371 >= 23 & indata$PE1371 <= 26, 2,3))

# Second wave

indata$state_w2 <- ifelse(indata$PG1293 >= 27 & indata$PG1293 <= 30, 1,
                            ifelse(indata$PG1293 >= 23 & indata$PG1293 <= 26, 2,3))

# Third wave

indata$state_w3 <- ifelse(indata$PH222 >= 27 & indata$PH222 <= 30, 1,
                            ifelse(indata$PH222 >= 23 & indata$PH222 <= 26, 2,3))

# Exclude individuals without data on any of the three waves

indata <- indata[!(is.na(indata$state_w1)) | !(is.na(indata$state_w3))| !(is.na(indata$state_w3)),]

n_distinct(indata$id) # 873 individuals with data on wave 1, 2 or 3

# Death states

indata$state_dead <- ifelse(is.na(indata$deathag) == TRUE,-2,4) # -2 = right censored

indata$age_dead <- ifelse(is.na(indata$deathag) == T, 85.1400, indata$deathag) #85.13973 = right censored age"

# All the converted mmmse variables includes the names "state" and "age"

# Finally, dead or right cencored state is given the name "dead".

# Convert to long format ------------------------------------------------


# gather all state and age variables
dta_long <- gather(indata, varpoint, value, contains("state"), contains("age"))
# One variable called "varpoint" and one variable called "value".
# varpoint includes variable name and timepoint and value includes "state","age"

# separate time and variable name
dta_long <- separate(dta_long, varpoint, c("var","point"), sep = "_")

# table

table(dta_long$point) # four time points, w1, w2, w4 and death/right censored
table(dta_long$var) # two variables, "age" and "state" (long format variables)

# spread will create two variables of "var" and "value" and separate "state" and "age"

dta_long <- spread(dta_long, var, value)

# sort on ID.

dta_long <- arrange(dta_long, id)

# delete raws with "na" on state

dta_long <- filter(dta_long, is.na(state) == FALSE)

# check so it is right ------------------------------------------------

n_distinct(dta_long$id) # 873 individuals

# MSM ---------------------------------------------------------------------

# select and sort variables for msm analysis for dataset in wide format "dta_wide"

dta_wide <- indata[c(1:5,13:18)] 
dta_wide <- dta_wide[c(1,3,4,5,11,7,8,9,10,2,6)]
n_distinct(dta_wide$id) # 873 individuals

head(dta_wide)

#  id    age_w1    age_w2   age_w3 age_dead  state_w1 state_w2 state_w3 state_dead
#2 3304 70.67123       NA       NA 75.27671        1       NA       NA          4
#3 3323 70.84384 75.55616 80.94521 85.14000        1        1        1         -2
#4 3328 72.07945       NA 80.99178 85.14000        1       NA        1         -2
#5 3427 71.11781 75.53973 81.10959 85.14000        1        1        1         -2
#6 3447 70.52329 75.25753 80.30137 85.14000        1        1        1         -2
#8 5842 70.60000 75.54795 80.82192 83.76712        1        1        1          4

# select and sort variables for msm analysis with dataset in long format "dta_long"

dta_long <- dta_long[c(1,2,10,12,13)] 
dta_long <- dta_long[c(1,4,5,2,3)]
n_distinct(dta_long$id) # 873 individuals

head (dta_long)

#    id     age     state sex   education
# 1 3304 70.67123     1   0        -1
# 2 3304 75.27671     4   0        -1
# 3 3323 70.84384     1   0         1
# 4 3323 75.55616     1   0         1
# 5 3323 80.94521     1   0         1
# 6 3323 85.14000    -2   0         1

# Preliminaries:
library(msm)
source("./Practical1/ELECT.r")

# Define rounding in output:
digits <- 3

# Data info:
subjects <- as.numeric(names(table(dta_long$id)))
N <- length(subjects)
cat("\nSample size:",N,"\n")
cat("\nFrequencies observed state:"); print(table(dta_long$state))
cat("\nFrequencies number of observations:"); print(table(table(dta_long$id)))
cat("\nState table:"); print(statetable.msm(state,id,data=dta_long))

# State table:    to
# from  -2   1   2   3   4
#    1 492 762 102  37 189
#    2  71  76  27  23  37
#    3  35   3   6  16  49
