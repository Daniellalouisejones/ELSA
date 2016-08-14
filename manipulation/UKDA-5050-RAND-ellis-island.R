# This is the Ellis-island script for UKDA-5050 data download https://discover.ukdataservice.ac.uk/catalogue/?sn=5050&type=data%20catalogue

# The purpose of this script is to create a data object (dto) 
# (dto) which will hold all data and metadata from each data file
# Run the line below to stitch a basic html output. For elaborated report, run the corresponding .Rmd file
# knitr::stitch_rmd(script="./manipulation/UKDA-5050-ellis-island.R", output="./manipulation/stitched-output/UKDA-5050-ellis-island.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("tidyr")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit") #For asserting conditions meet expected patterns.

# ---- dto-1 ---------------------------------------------------------
#
# There will be a total of (4) elements in (dto)
dto <- list() # creates empty list object to populate with script to follow 
#
### dto (1) : names of the observation occasions (waves)
#
# inspect what files there are
order_number <- 103647
folderPath <- paste0("./data/unshared/raw/",order_number,"UKDA-5050-spss/spss/spss19/")
# folderPath <- "./data/unshared/raw/103646/UKDA-5050-spss/spss/spss19/"
(listFiles <- list.files(folderPath, full.names = T,  pattern = ".sav", recursive = F))
# list the names of the studies to be used in subsequent code
# waveNames <- c("w1", "w2", "w3", "w4", "w5","w6")
# names(waveNames) <- waveNames
# dto[["waveName"]] <- waveNames
# names(dto[["waveName"]]) <- "occasions of measurement"
# names(dto)
# dto$waveName
# ---- dto-2 ---------------------------------------------------------
#
### dto (2) : file paths to corresponding data files
#

# at this point the object `dto` contains components:
# lapply(dto,class)
# lapply(dto,names)

# manually declare the file paths to enforce the order and prevent mismatching
harmonized_data    <- paste0(folderPath, "h_elsa.sav")
# wave_1_path_input  <- paste0(folderPath,"wave_1_core_data_v3.sav")
# wave_2_path_input  <- paste0(folderPath,"wave_2_core_data_v4.sav")
# wave_3_path_input  <- paste0(folderPath,"wave_3_elsa_data_v4.sav")
# wave_4_path_input  <- paste0(folderPath,"wave_4_elsa_data_v3.sav")
# wave_5_path_input  <- paste0(folderPath,"wave_5_elsa_data_v4.sav")
# wave_6_path_input  <- paste0(folderPath,"wave_6_elsa_data_v2.sav")
# 


# combine file paths into a single object
# filePaths <- c(wave_1_path_input,
#                wave_2_path_input,
#                wave_3_path_input,
#                wave_4_path_input,
#                wave_5_path_input,
#                wave_6_path_input
#               )
# dto[["filePath"]] <- filePaths
# names(dto$filePath) <- paste0("filePath_w_",waveNames)
# names(dto$filePath) <- waveNames

ds <- Hmisc::spss.get(harmonized_data , use.value.labels = TRUE)
names(h_elsa)
save_csv <- names_labels(h_elsa)
write.csv(save_csv, paste0("./data/shared/meta/names-labels-live/meta-",order_number,"-live.csv"), 
          row.names = T)  
# augment meta data manually
metaData <- read.csv(paste0("./data/shared/meta/meta-",order_number,"-dead.csv"),
                     header = T, stringsAsFactors = F )  



ds %>% 
  dplyr::group_by(raedyrs.e) %>% 
  dplyr::summarize(n=n())
# ---- dto-3 ---------------------------------------------------------

#
### dto (3) : datasets with raw source data from each wave
#
# at this point the object `dto` contains components:
# lapply(dto,class)
# lapply(dto,names)

# next, we will add another element to this list `dto`  and call it "unitData"
# it will be a list object in itself, storing datasets from studies as seperate elements
# no we will reach to the file paths in `dto[["filePath"]][[i]] and input raw data sets
# where `i` is iteratively each study in `dto[["waveName"]][[i]]

# disable BELOW after creating dto[1:3]
data_list <- list() # declare a list to populate
for(i in seq_along(dto[["waveName"]])){
  # i <- 1
  # input the 5 SPSS files in .SAV extension provided with the exercise
  # ds <- Hmisc::spss.get(dto[["filePath"]][i], use.value.labels = TRUE)
  data_list[[i]] <- Hmisc::spss.get(dto[["filePath"]][i], use.value.labels = TRUE)
}
names(data_list) <- waveNames # name the elements of the data list
dto[["unitData"]] <- data_list # include data list into the main list as another element
names(dto$unitData) <- waveNames
rm(data_list)
names(dto) # elements in the main list object
names(dto[["unitData"]]) # elements in the subelement
saveRDS(dto,   "./data/unshared/derived/dto_cache.rds")
rm(dto)
# disable ABOVE after creating dto[1:3]

dto <- readRDS("./data/unshared/derived/dto_cache.rds")

h_esla <- Hmisc::spss.get("./data/unshared/raw/RAND/UKDA-5050-spss/spss/spss19/h_elsa.sav" , use.value.labels = TRUE)

names(h_elsa)
save_csv <- names_labels(h_elsa)
write.csv(save_csv, paste0("./data/shared/meta/names-labels-live/nl-",i,".csv"), 
          row.names = T)  

metaData <- read.csv(paste0("./data/shared/meta/meta-",order_number,"-dead.csv"),
                     header = T, stringsAsFactors = F )  


# ---- dto-4 ---------------------------------------------------------
#
### dto (4) : collect metadata
#

# at this point the object `dto` contains components:
lapply(dto,class)
lapply(dto,names)



# ---- inspect-raw-data -------------------------------------------------------------
# inspect the variable names and their labels in the raw data files
# names_labels(dto$unitData$w1)
# names_labels(dto$unitData$w2)
# names_labels(dto$unitData$w3)
# names_labels(dto$unitData$w4)
# names_labels(dto$unitData$w5)
# names_labels(dto$unitData$w6)

# ---- tweak-data --------------------------------------------------------------

# ---- collect-meta-data -----------------------------------------
# to prepare for the final step in which we add metadata to the dto
# we begin by extracting the names and (hopefuly their) labels of variables from each dataset
# and combine them in a single rectanguar object, long/stacked with respect to study names
for(i in waveNames){  
  save_csv <- names_labels(dto[["unitData"]][[i]])
  write.csv(save_csv, paste0("./data/shared/meta/names-labels-live/nl-",i,".csv"), 
            row.names = T)  
}  
# these individual .cvs contain the original variable names and labels
# now we combine these files to create the starter for our metadata object
dum <- list()
for(i in waveNames){  
  dum[[i]] <- read.csv(paste0("./data/shared/meta/names-labels-live/nl-",i,".csv"),
                       header = T, stringsAsFactors = F )  
}
mdsraw <- plyr::ldply(dum, data.frame,.id = "wave_name") # convert list of ds into a single ds
mdsraw["X"] <- NULL # remove native counter variable, not needed
write.csv(mdsraw, "./data/shared/meta/names-labels-live/names-labels-live.csv", row.names = T)  

# ----- import-meta-data-dead -----------------------------------------
# after the final version of the data files used in the analysis have been obtained
# we made a dead copy of `./data/shared/derived/meta-raw-live.csv` and named it `./data/shared/meta-data-map.csv`
# decisions on variables' renaming and classification is encoded in this map
# reproduce ellis-island script every time you make changes to `meta-data-map.csv`
dsm <- read.csv("./data/shared/meta/meta-data-elsa.csv")
# dsm <- read.csv("./data/shared/meta/names-labels-live/names-labels-live.csv")
dsm["X"] <- NULL # remove native counter variable, not needed


# attach metadata object as the 4th element of the dto
dto[["metaData"]] <- dsm


dto[["metaData"]] %>%
  dplyr::filter(retained==TRUE) %>%
  dplyr::mutate(name = as.character(name),
                name_new = as.character(name_new),
                label = as.character(label)) %>%
  dplyr::select_("wave_name","name", "name_new", "label") %>%
  DT::datatable(
    class   = 'cell-border stripe',
    caption = "This is the primary metadata file. Edit at `./data/shared/meta-data-map.csv",
    filter  = "top",
    options = list(pageLength = 6, autoWidth = TRUE)
  )



#######################  developing code beyond this point


assemble_dto <- function(dto, get_these_variables){
  
  l <- list() #  list object with data frome each wave
  for(wave_name_ in dto[["waveName"]]){
    dsm <- dto$metaData %>% 
      dplyr::filter(retained==TRUE, wave_name==wave_name_) %>% 
    get_these_variables <- unique(as.character(dsm$name))
     
    
    d <- dto[["unitData"]][[wave_name_]][,get_these_variables] # get wave data from dto
    variables_present <- colnames(d) %in% get_these_variables # variables on the list
    l[[s]] <- d[, variables_present] # keep only them
  }
  return(l)
}
get_these_variables <- c(
  "id",
  "year_of_wave","age_in_years","year_born",
  "female",
  "marital", "single",
  "educ3",
  "smoke_now","smoked_ever",
  "current_work_2",
  "current_drink",
  "sedentary",
  "poor_health"
)

ldto <- assemble_dto(dto=dto, get_these_variables = get_these_variables)
lapply(lsh, names) # view the contents of the list object
ds <- plyr::ldply(lsh,data.frame, .id = "study_name")
ds$id <- 1:nrow(ds) # some ids values might be identical, replace
ds %>% names()








# ---- variables-to-extract



dto2 <- list()
dto2$waveName <- dto$waveName
dto2$metaData <- dto$metaData


for(i in waveNames){  
  # i <- "w1"
  selected_variables <- c(id, fluency_varnames)
  dto2[[i]] <- dto$unitData[[i]] %>% dplyr::select(one_of(c("CfAni")))
}  
# these individual .cvs contain the original variable names and labels
# now we combine these files to create the starter for our metadata object
dum <- list()
for(i in waveNames){  
  dum[[i]] <- read.csv(paste0("./data/shared/meta/names-labels-live/nl-",i,".csv"),
                       header = T, stringsAsFactors = F )  
}
mdsraw <- plyr::ldply(dum, data.frame,.id = "wave_name") # convert list of ds into a single ds



# ---- verify-values -----------------------------------------------------------
# testit::assert("`model_name` should be a unique value", sum(duplicated(ds$model_name))==0L)
# testit::assert("`miles_per_gallon` should be a positive value.", all(ds$miles_per_gallon>0))
# testit::assert("`weight_gear_z` should be a positive or missing value.", all(is.na(ds$miles_per_gallon) | (ds$miles_per_gallon>0)))

# ---- save-to-disk ------------------------------------------------------------

# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data/unshared/derived/dto.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/dto.rds")
# each element this list is another list:
names(dto)
# 1st element - names of the studies as character vector
dto[["studyName"]]
# 2nd element - file paths of the data files for each study
dto[["filePath"]]
# 3rd element - list objects with 
names(dto[["unitData"]])
dplyr::tbl_df(dto[["unitData"]][["alsa"]]) 
# 4th element - dataset with augmented names and labels for variables from all involved studies
dplyr::tbl_df(dto[["metaData"]])
