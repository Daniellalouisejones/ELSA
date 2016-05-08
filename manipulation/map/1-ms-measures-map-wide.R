# knitr::stitch_rmd(script="./manipulation/rename-classify.R", output="./manipulation/rename-classify.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load_sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load_packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE) #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit", quietly=TRUE)
# requireNamespace("plyr", quietly=TRUE)

# ---- declare-globals ----------------------------------------------
data_path_input  <- "./data/unshared/derived/dto.rds"
metadata_path_input <- "./data/meta/map/meta-data-map.csv" # input file with your manual classification

# ---- load-data ------------------------------------------------
dto <- readRDS(data_path_input)
names(dto)
ds <- dto[["unitData"]] %>% 
  dplyr::select_(
    "id", 
    "fu_year", # follow-up year, wave indicator
    "age_at_visit", "age_death", 
    "msex", # Gender
    "educ", # Years of education
    "smoke_bl", # Smoking at baseline
    "alco_life", # Lifetime daily alcohol intake -baseline
    "mmse" # Mini Mental State Exam 
  ) %>% 
  dplyr::filter(!(is.na(age_death) & is.na(age_at_visit)))

i <- 1
head(ds) 

# ---- define-variables -----------------------------------------
wave_indicator_varname <- "fu_year"
time_invariant_varnames <- c(
  "id",
  "msex",
  "educ", 
  "smoke_bl", 
  "alco_life",
  "age_death" 
) 
# death indicator
(age_death <- "age_death")
# temporal pattern of the response vector
(time_points <- unique(ds[,wave_indicator_varname]))
# source for computing alive state
(indicator_name <- "mmse")
(indicator_name_wide <- paste(indicator_name,time_points, sep="_"))
# current age
(age_name <- "age_at_visit")
(age_name_wide <- paste(age_name,time_points, sep="_"))
(time_invariant_varnames_long <- c(age_name,indicator_name ))
(time_invariant_varnames_wide <- c(age_name_wide,indicator_name_wide ))


# ---- define-functions ---------------------------------------------
make_long_from_wide <- function(
  d = d, # data in wide format, with encoded multi-state 
  time_invariant
){
  (time_variant <- setdiff(names(d), time_invariant))
  
  ds_long <- data.table::melt(data = d, id.vars = time_invariant,  measure.vars = time_variant)
  ds_long$variable <- as.character(ds_long$variable)
  unique(ds_long$variable)
  # 
  regex <- "^(\\w+?)_(\\d+?)$" 
  d_long <- ds_long %>%
    dplyr::mutate(
      measure = gsub(regex,"\\1",variable),
      time_point = gsub(regex,"\\2",variable)
    ) %>%
    dplyr::select(-variable)
  head(d_long)  
  
  d_wide <- d_long %>%
    tidyr::spread(key=measure,value=value) %>%
    dplyr::arrange_(.dots=time_invariant)
  head(d_wide)
  return(d_wide)
}
# ds_long <- make_long_from_wide(d,time_invariant_varnames)


# ---- transform-to-wide-time ----------------------------------------

(make_these_long <- c( age_name, indicator_name ))
library(data.table) ## v >= 1.9.6
ds_wide <- data.table::dcast(
  data.table::setDT(ds),
  id  + msex + educ + smoke_bl + alco_life + age_death ~ fu_year, 
  value.var = make_these_long) 
ds_wide %>% dplyr::glimpse()

# ---- recode-all-waves -------------------------------------------------------
d <- as.data.frame(ds_wide )
dl <- make_long_from_wide(d,time_invariant_varnames)
dl %>% dplyr::filter(id %in% c(2136155))

# -1 = denotes an intermediate missing state, e.i. There is a time of interview, but no observed state
# -2 = denotes being alive, bun in unknown state (there is no time of interview?)

for(w in time_points){
  # define varnames at waves
  (state_w <- paste0("state_",w))
  (age_w <- paste0("age_at_visit_",w))
  (var_w <- paste0("mmse_",w))
  (alive_w <- paste0("alive_",w))


    d[,alive_w] <- 1 # start assuming R is alive
    for( i in 1:nrow(ds_wide)){
      d[i,alive_w] <- ifelse( 
        test = ( !is.na(d[i,"age_death"]) & is.na(d[i,age_w]) ) , # only one case when R could be declared dead
        yes = FALSE,
        no = d[i,alive_w]
      ) 
    }
 
  # R is alive, but ...
      d[,state_w] <- ifelse(
        # ... no age or measure present = right censored = unknown living state
        d[,alive_w]==1 & is.na(d[,age_w]) & is.na(d[,var_w]),  -2,  ifelse(
          # ... age is present, but measure is missing = intermediate missing state
          d[,alive_w]==1 & !is.na(d[,age_w]) & is.na(d[,var_w]),  -1, ifelse(
            # ... and has no cognitive impairment (HEALTHY)
            d[,alive_w]==1 & d[,var_w] >= 27,  1,  ifelse(
              # ... nd has mild cognitive impairment (MCI)
              d[,alive_w]==1 & d[,var_w] >= 23 & d[i,var_w] < 27,  2,  ifelse(
                # ... and has severe cognitive impairment (SCI)
                d[,alive_w]==1 & d[,var_w] < 23,  3, ifelse(
                  # not alive
                  d[,alive_w]==0, 4, NA)))))) 

}

names(d) # data in wide format, with encoded multi-state 
dl <- make_long_from_wide(d,time_invariant_varnames)
dl %>% dplyr::filter(id %in% c(2136155))
dl %>% dplyr::filter(id %in% c(33027))
dl %>% dplyr::filter(id %in% c(2817047))

dl %>% dplyr::filter(alive==0 & state==1)

msm::statetable.msm(state,id,dl)
# ---- wide-to-long-for-time -------------------------------

# ---- function-for-wide-to-long-conversion -----------
#  melt with respect to the index type
# (time_invariant <- time_invariant_varnames) #make sure values in csv are correct!


dd <- ds_long[!is.na(ds_long$age_death),c("id","time_point", "age_death","age_at_visit", "mmse","alive")]
# dd[dd$id==2136155,]
dd[dd$id==7142937,]





############## snippets below ###################

t((d[28,]))


d <- as.data.frame(ds_wide )
for(w in time_points){
  # define varnames at waves
  state_w <- paste0("state_",w)
  age_w <- paste0("age_at_visit_",w)
  var_w <- paste0("mmse_",w)
  
  d[,state_w] <- NA # missing unless we provide enough information
  
  for(i in nrow(d)){
    # if(is.na(d[i,"age_death"])){ # i'm not dead yet!
    if(
      !is.na(d[i,"age_death"]) # age of death is not missing
      | # or
      d[i,"age_death"] < d[i,age_w] # respondent is not dead yet
    ){ # in this case, compute alive state{ # in this case, compute alive state
      d[i,state_w] <- ifelse(
        d[i,var_w] >= 27, 1, ifelse( # no impairment
          d[i,var_w] >= 23 & d[i,var_w] < 27, 2, ifelse( # mild impairment
            d[i,var_w] < 23, 3, NA)))  # sever imparement 
    }else{
      d[i,state_w] <- 4 # dead
    } 
    
  }
}
}


# d$state_0[d$age_death >]
# 
# for(i in nrow(d)){
#   # if(is.na(d[i,"age_death"])){ # i'm not dead yet!
#   if(
#     !is.na(d[i,"age_death"]) # age of death is not missing
#     | # or
#     d[i,"age_death"] < d[i,age_w] # respondent is not dead yet
#   ){ # in this case, compute alive state
#     d[i,state_w] <- ifelse(
#       d[i,var_w] >= 27, 1, ifelse( # no impairment
#         d[i,var_w] >= 23 & d[i,var_w] < 27, 2, ifelse( # mild impairment
#           d[i,var_w] < 23, 3, NA)))  # sever imparement 
#   }else{
#     d[i,state_w] <- 4 # dead
#   } 
#   


# ---- recognize-NAs-----------------------------------------------
dsf <- dto[["unitData"]]
sum(is.na(dsf$age_bl))
sum(is.na(dsf$age_death))
sum(is.na(dsf$age_at_visit))

# ---- recode-one-wave -------------------------------------------------------
# d <- as.data.frame(ds_wide )
# d$state_0 <- 0 # missing state?
# d$state_0[d$mmse_0 >= 27] <- 1 # no impairment, alive & healthy
# d$state_0[d$mmse_0 >= 23 & d$mmse_0 < 27] <- 2 # mild cognitive impairment (MCI)
# d$state_0[d$mmse_0 < 23] <- 3 # sever cognitive impairment (SCI)
# d$state_0[!is.na(d$age_death) & d$age_death > d$age_at_visit_0]
# 
# # after the baseline
# d$state_0 <- 0 # missing state?
# d$state_0[d$mmse_0 >= 27] <- 1 # no impairment, alive & healthy
# d$state_0[d$mmse_0 >= 23 & d$mmse_0 < 27] <- 2 # mild cognitive impairment (MCI)
# d$state_0[d$mmse_0 < 23] <- 3 # sever cognitive impairment (SCI)
# 

# 

# 
# msm::statetable.msm(state,id,d)
# 
# d$alive_1[is.na()] 
# 
# # example of a inconsistent pattern
 t(as.numeric(d[28,c("id", paste0("age_at_visit_",0:16),"age_death")]))
# 
