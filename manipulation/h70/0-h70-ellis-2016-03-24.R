# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes 
source("http://www.ucl.ac.uk/~ucakadl/ELECT.r") # load  ELECT functions

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library("msm") # multistate modeling (cannot be declared silently)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.
requireNamespace("flexsurv") # parameteric survival and multi-state
requireNamespace("mstate") # multistate modeling
requireNamespace("foreign") # data input

# ---- declare-globals ---------------------------------------------------------
path_file <- "./data/unshared/Koh1930_70_75_79_Amsterdam.sav"
# ---- load-data ---------------------------------------------------------------
ds <- Hmisc::spss.get(path_file, use.value.labels = TRUE, datevars = "avldat" )
head(ds)
# ---- inspect-data -------------------------------------------------------------
names_labels(ds)

# ---- tweak-data --------------------------------------------------------------


# basic inspect
names_labels(ds)
length(unique(ds$lopnr))
table(ds$sex)
table(ds$far)
summary(ds$avl)

# ---- subset-variables ------------------------------------
ds <- ds [ , c(
  "lopnr", "sex", "far", "avldat", "avlnum", "avlage", #time invariant
  "AG87", #Education (years)
  "PE1197", #Alcohol (Gram/week)
  "AE385", #Physical activity (70)
  "intage2000", "intage2005", "intage2009",
  "cook.70", "cook.75", "cook.79", 
  "bath.70", "bath.75", "bath.79",
  "dressing.70", "dressing.75", "dressing.79",
  "toilet.70", "toilet.75", "toilet.79",
  "feed.70", "feed.75", "feed.79",
  "chair.70", "chair.75", "chair.79",
  "dem2000", "dem2005", "dem2009",
  "AE69", "AG58", "AH108", #BMI
  "srh.70", "srh.75", "srh.79", #Self-rated health
  "AE320", "AG384", "AH414"  #Smoking
  
  )]
# ---- declare-types-of-variables --------------------------

time_invariant <- c("lopnr", "sex", "far", "avldat", "avlnum", "avlage", "AG87", "PE1197", "AE385"  )
(time_variant <- setdiff(names(ds), time_invariant))

# ---- wide-to-long-for-time -------------------------------

#  melt with respect to the index type
ds_long <- data.table::melt(data =ds, id.vars = time_invariant,  measure.vars = time_variant)
d <- ds_long %>% dplyr::group_by(variable) %>% dplyr::summarise(count=n())
print(d,nrow=30);unique(d$variable)
regex1 <- "^intage|cook|bath|dressing|toilet|feed|chair|srh|dem"
regex2 <- "^AE|AG|AH"
regex3 <- "2000|2005|2009$"
regex4 <- ".70|.75|.79$" 
regex5 <- "69|58|108|320|384|414$"
ds_long <- ds_long %>%
  dplyr::arrange_(.dots=time_invariant) %>%
  dplyr::mutate(
    year = variable, age=variable,
    year = sub(pattern = regex3, x=variable, replacement = ""),
    age = sub(pattern = regex4, x=variable, replacement = ""),
    variable =  sub(pattern = regex2, x=variable, replacement = ""  )
  )
head(ds_long, 20)

# ---- inspect-for-duplicates ------------------------------
# dput(colnames(ds_long))
ds_distinct <- ds_long %>%
  dplyr::distinct()


ds_no_duplicates <- ds_long %>%
  dplyr::group_by(
    lopnr, sex, far, avldat, avlnum, avlage, variable, year
  ) %>%  #Lacks "value"
  dplyr::summarize(
    # value  = dplyr::first(value, na.rm=T)
    value  = mean(value, na.rm=T)
  ) %>%
  dplyr::ungroup()


coefficient_of_variation <- function(x)( sd(x)/mean(x) )

ds_find_duplicates <- ds_long %>%
  dplyr::distinct() %>% 
  dplyr::group_by(
    lopnr, sex, far, avldat, avlnum, avlage, variable, year
  ) %>%  #Lacks "value"
  dplyr::filter(!is.na(value)) %>% # !!Careful that you don't remove legit NAs (esp, in nonduplicated rows).
  dplyr::summarize(
    count      = n(),
    values     = paste(value, collapse=";"),
    value_cv   = coefficient_of_variation(value)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(1<count) %>%
  dplyr::filter(.001 < value_cv) 

testit::assert("No meaningful duplicate rows should exist.", nrow(ds_find_duplicates)==0L)

# ---- long-to-wide-time -------------------------------

ds_wide <- ds_no_duplicates %>%
  tidyr::spread(key=variable, value=value) %>%
  dplyr::mutate(
    sex = factor(sex)
  ) %>%
  dplyr::arrange_(.dots = time_invariant)
names_labels(as.data.frame(ds_wide))
head(ds_wide)

# compare to the vignette dataset
path_data_example_i <- "./data/shared/raw/dataExample1.RData"
load(path_data_example_i)
head(data)


# ---- save-data-to-disk ---------------------------------
saveRDS(ds_wide, "./data/unshared/derived/ds0-h70.rds")
