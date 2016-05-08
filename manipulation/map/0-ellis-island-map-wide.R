# the purpose of this script is to create a data object (dto) which will hold all data and metadata from each candidate study of the exercise
# run the line below to stitch a basic html output. For elaborated report, run the corresponding .Rmd file
# knitr::stitch_rmd(script="./manipulation/map/0-ellis-island-map.R", output="./manipulation/map/stitched-output/0-ellis-island-map.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/general-graphs.R") 
# source("./scripts/graph-presets.R") # fonts, colors, themes 
# ---- load-packages ----------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("tidyr")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit") #For asserting conditions meet expected patterns.

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

# ---- transform-to-wide-time ----------------------------------------
# wave_counter <- 0:17
wave_counter <- unique(ds$fu_year)
(ms_outcome_wide_names <- paste0("mmse_",wave_counter))
(ms_age_at_visit_wide_names <- paste0("age_", wave_counter))
(not_long_stem <- c( "age_at_visit", "mmse"))
(long_stem <- setdiff(names(ds),not_long_stem))
# ds_wide <- ds_no_duplicates %>%
# ds_wide <- ds %>%
#   tidyr::spread_(key="fu_year", value %in% c("age_at_visit", "mmse") )

library(data.table) ## v >= 1.9.6
ds_wide <- data.table::dcast(
  data.table::setDT(ds),
  id + age_death + msex + educ + smoke_bl + alco_life ~ fu_year, value.var = c(
    "age_at_visit", "mmse")) 

# ---- save-to-disk ---------------------------------------
saveRDS(ds_wide,"./data/unshared/derived/wide-data-map-mmse.rds")

# ---- recode-1 ------------------------------------------




d <- as.data.frame(ds_wide )
for(i in 1:nrow()){
  d[i,"state_0"] <- ifelse(
    is.na(d[i,"age_death"]) & is.na(d[i,"age_at_visit"]), NA, 
    ifelse( !is.na(d[i,"age_death"]) & d[i,"age_death"] >
    
    
    d[i,"mmse_0"] >=27 & ( d[i,"age_death"] > d[i,"age_at_visit_0"] ) ,  1, # healthy
    ifelse(
      d[i,"mmse_0"] <26 & d[i,"mmse_0"] >=23 & is.na(d[i,"age_death"]) , 2, # Mild Cognitive Impairment
         ifelse(
           d[i,"mmse_0"] <=22 & is.na(d[i,"age_death"]) , 3, # dementia
           ifelse(
             d[i,""]
           )
           
           )
              
              
              )
    )  {
   d[i,"state_0"] <- 1
  }
  if(d[i,"mmse_0"] <26 & d[i,"mmse_0"] >=23 & !is.na(d[i,"age_at_visit"]) )  {
    d[i,"state_0"] <- 2
  }
  if(d[i,"mmse_0"] <=22 & !is.na(d[i,"age_at_visit"]) )  {
    d[i,"state_0"] <- 3
  }
  if(!is.na(d[i,"age_death"]) )  {
    d[i,"state_0"] <- 3
  }
}







