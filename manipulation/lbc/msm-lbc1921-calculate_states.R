path_file <- "./data/unshared/LBC1921_IALSA_IC_08APR2016_ALLVARS.sav" # data file location
# ---- load-data ---------------------------------------------------------------
data <- Hmisc::spss.get(path_file, use.value.labels = TRUE, datevars = "avldat" ) #read in file
head(data)

#### Calculate states (MMSE cuttoffs follow Marioni et al 2012) ###

# wave 1
data$states_w1 <- 0
data$states_w1[(data$mmse>=27)] <- 1
data$states_w1[(data$mmse<27 &          
                 data$mmse>=23)] <- 2
data$states_w1[(data$mmse<23)] <- 3
data$states_w1[(data$agedays.death<28400)] <- 4 # denotes death before wave 1
table(data$states_w1)
# 0   1   2   3 
# 21 472  71   5 

# wave 2
data$states_w2 <- 0
data$states_w2[(data$mmse83>=27)] <- 1
data$states_w2[(data$mmse83<27 &          
                    data$mmse83>=23)] <- 2
data$states_w2[(data$mmse83<23)] <- 3
data$states_w2[(data$agedays.death < 29960)] <- 4 #death before wave 2
table(data$states_w2)
# 0   1   2   3   4 
# 195 266  47   8  53 

# wave 3
data$states_w3 <- 0
data$states_w3[(data$mmse87>=27)] <- 1
data$states_w3[(data$mmse87<27 &          
                    data$mmse87>=23)] <- 2
data$states_w3[(data$mmse87<23)] <- 3
data$states_w3[(data$agedays.death <  31300)] <- 4 #death before wave 3
table(data$states_w3)
# 0   1   2   3   4 
# 198 167  30  10 164 

# wave 4
data$states_w4 <- 0
data$states_w4[(data$mmse.w4>=27)] <- 1
data$states_w4[(data$mmse.w4<27 &          
                    data$mmse.w4>=23)] <- 2
data$states_w4[(data$mmse.w4<23)] <- 3
data$states_w4[(data$agedays.death <  32840)] <- 4 #death before wave 4
table(data$states_w4)
# 0   1   2   3   4 
# 144  93  27   9 296 

# wave 5
data$states_w5 <- 0
data$states_w5[(data$mmse.w5>=27)] <- 1
data$states_w5[(data$mmse.w5<27 &          
                    data$mmse.w5>=23)] <- 2
data$states_w5[(data$mmse.w5<23)] <- 3
data$states_w5[(data$agedays.death >  0)] <- 4 # any time of death
table(data$states_w5)
# 0   1   2   3   4 
# 85  32  10   1 441 

#### Select variables for the analysis ####
vars <-  c("studyno", "gender", "yrseduc", "agedays.death",
            "mhtdays",
            "agedayep.83",
            "agedaywtcrf",
            "agedays.w4",
            "agedays.w5",
            "states_w1",
            "states_w2",
            "states_w3",
            "states_w4",
            "states_w5")
ds <- data[vars]

#### Rename time-varying vars for easy reshape ####
names(ds)[5:14] <- c("age1","age2", "age3", "age4", "age5", "states1", "states2", "states3", "states4", "states5")

#### Reshape wide-to-long ####
long <- reshape(ds, varying=c("age1","age2", "age3", "age4", "age5", "states1", "states2", "states3", "states4", "states5"), idvar = "studyno", direction="long", sep = "")

#### Order file to be msm-friendly ####
final <- long[order(long$studyno),]
head(final)
