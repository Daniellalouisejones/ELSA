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
# requireNamespace("flexsurv") # parameteric survival and multi-state
# requireNamespace("mstate") # multistate modeling
requireNamespace("foreign") # data input

# ---- declare-globals ---------------------------------------------------------
path_file <- "./data/unshared/LBC1921_IALSA_IC_30MAR2016.sav"
# ---- load-data ---------------------------------------------------------------
ds <- Hmisc::spss.get(path_file, use.value.labels = TRUE, datevars = "avldat" )
head(ds)
# ---- extract-meta-data -------------------------------------------------------
ml <- names_labels(ds)
write.csv(ml, "./data/shared/meta-lbc1921-raw.csv")
# ---- import-meta-data -------------------------------------------------------
mds <- read.csv("./data/shared/meta-lbc1921-dead.csv", stringsAsFactors = F)

# ---- view-meta-data-1--------------------------------------------------------
d_rules <- mds %>% 
  dplyr::select(name,name_new,time_variant,fu_point)
d_rules
names(ds)
# ---- tweak-data --------------------------------------------------------------
# (a <- as.data.frame(names(ds))) #prints the whole thing
# names(a) <- "name";names(a)
# a$name <- as.character(a$name)
# (b <- mds %>% dplyr::select(name,name_new))
# str(a);str(b)
# ab <- a %>%
#   dplyr::left_join(b,by=c("name"="name"))
# print(ab)
# names(ds) <- ab$name_new

# ---- declare-types-of-variables --------------------- 
# which ones will be time in/variante
md <- mds %>% 
  dplyr::filter(time_variant==0) %>%
  dplyr::select(name,name_new,time_variant,fu_point)
# ds <- ds %>%
#   dplyr::select(id,)
(time_invariant <- unique(md$name)) #make sure values in csv are correct!
(time_variant <- setdiff(names(ds), time_invariant))

# ---- wide-to-long-for-time -------------------------------

#  melt with respect to the index type
ds_long <- data.table::melt(data =ds, id.vars = time_invariant,  measure.vars = time_variant)
table(ds_long$variable)
d_rule <- mds %>%
  dplyr::select(name,name_new,fu_point)
names(ds_long);names(d_rule)
str(ds_long$variable);str(d_rule$name)
ds_long$variable <- as.character(ds_long$variable)
d_long <- ds_long %>%
  dplyr::left_join(d_rule,by=c("variable"="name")) %>%
  dplyr::mutate(variable=name_new) %>%
  dplyr::select(-variable)
  head(d_long)  

ds_wide <- ds_long %>%
  tidyr::spread(key=variable,value=value) %>%
  dplyr::arrange_(.dots=time_invariant)


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
