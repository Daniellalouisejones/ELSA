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
# data_path_input  <- "./data/unshared/derived/map-ds0.rds"
data_path_input  <- "../MAP/data-unshared/derived/ds0.rds"
metadata_path_input <- "./data/meta/map/meta-data-map.csv" # input file with your manual classification

# ---- load-data ------------------------------------------------
unitData <- readRDS(data_path_input)
metaData <- read.csv(metadata_path_input,stringsAsFactors=F,header=T)
metaData$X <- NULL # remove automatic variable




# ---- inspect-data ----------------------------------------------
# basic demographic variables
ds <- unitData # assing alias
length(unique(ds$id)) # there are this many of subjects
table(ds[,"msex"], ds[,"race"], useNA = "always")
t <- table(ds[,"educ"], ds[,"race"]); t[t==0]<-".";t
# age variables
t <- table(ds[,"fu_year"], ds[,"died"]); t[t==0]<-".";t
# histogram_continuous(ds, variable_name = "age_death", bin_width = 1)
# histogram_continuous(ds, variable_name = "age_bl", bin_width = 1)
# histogram_continuous(ds, variable_name = "age_at_visit", bin_width = 1)

# ---- assemble-data-object-dto-1 --------------------------------------
dto <- list()
# the first element of data transfer object contains unit data
dto[["unitData"]] <- unitData
# the second element of data transfer object contains meta data
dto[["metaData"]] <-  metaData
# verify and glimpse
dto[["unitData"]] %>% dplyr::glimpse()
dto[["metaData"]] %>% dplyr::glimpse()


# ----- view-metadata-1 ---------------------------------------------
meta_data <- dto[["metaData"]] %>%
  dplyr::filter(include == TRUE) %>%
  # dplyr::select(name, name_new, type, label, construct) %>%
  dplyr::arrange(type, construct, name)
knitr::kable(meta_data)

# ----- apply-meta-data-1 -------------------------------------
# rename variables
d_rules <- dto[["metaData"]] %>%
  dplyr::filter(name %in% names(ds)) %>% 
  dplyr::select(name, name_new ) # leave only collumn, which values you wish to append
names(ds) <- d_rules[,"name_new"]
# transfer changes to dto
dto[["unitData"]] <- ds

# ---- save-to-disk ------------------------------------------------------------

# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data/unshared/derived/dto.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/dto.rds")
names(dto)
# 1st element - unit(person) level data
dplyr::tbl_df(dto[["unitData"]])
# 2nd element - meta data, info about variables
dto[["metaData"]]













