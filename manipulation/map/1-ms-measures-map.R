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

# ---- declare_globals ---------------------------------------------------------
# path_input  <- "./data-phi-free/raw/results-physical-cognitive.csv"
# path_input <- paste0("./data/shared/parsed-results-pc-",study,".csv")
# path_input  <- "./data/shared/parsed-results.csv"
path_input <- "./data/unshared/derived/dto.rds"
path_output <- "data/unshared/derived/dto.rds"

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metad
dto <- readRDS("./data/unshared/derived/dto.rds")

# ---- inspect-data -------------------------------------------------------------
names(dto)
# 1st element - unit(person) level data
dplyr::tbl_df(dto[["unitData"]])
# 2nd element - meta data, info about variables
dto[["metaData"]]



# ---- tweak_data --------------------------------------------------------------
ds <- dto[["unitData"]]

# table(ds$fu_year, ds$dementia)

# ---- look-up-pattern-for-single-id --------------------------------------------------------------
# if died==1, all subsequent focal_outcome==DEAD.
set.seed(1)
ids <- sample(unique(ds$id),3)
d <- ds %>% 
  dplyr::filter(id %in% ids) %>%
  dplyr::select_("id","fu_year","age_death","age_at_visit","dementia") %>%
  dplyr::mutate(
    age_death = as.numeric(age_death)
  )
d
# d$id <- substring(d$id,1,1)
# write.csv(d,"./data/shared/musti-state-dementia.csv")  

# ---- ms_dementia -------------------------------------------------------------
ds_alive <- d %>% 
  dplyr::rename(
    dementia_now=dementia, 
    fu_point = fu_year) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(
    dead_now = FALSE,
    dementia_now   = as.logical(dementia_now),
    dementia_ever  = any(dementia_now)
  ) %>% 
  dplyr::ungroup()
ds_alive
# str(ds_alive )
ds_dead <- ds_alive %>% 
  dplyr::filter(!is.na(age_death)) %>% 
  dplyr::group_by(id) %>% 
  dplyr::arrange(fu_point) %>% 
  dplyr::summarize(
    dead_now = TRUE,
    fu_point       = max(fu_point) + 1L,
    age_death      = max(age_death),
    age_at_visit   = NA_real_,
    dementia_last  = dplyr::last(dementia_now),
    dementia_now   = ifelse(dementia_last, TRUE, NA),
    dementia_ever  = any(dementia_now)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-dementia_last)
ds_dead
# str(ds_dead)
ds <- ds_alive %>%
  dplyr::union(ds_dead) %>%
  dplyr::arrange(id, fu_point)
ds
# compute the multistate variable
ds <- ds %>%
  dplyr::mutate(
    state = ifelse(!dead_now & !dementia_now, 1,
            ifelse(!dead_now & dementia_now, 2, 
                           ifelse(dead_now, 3, NA)))
  ) 
ds
ds$state <- ordered(ds$state, levels = c(1,2,3),
                    labels = c("Healthy","Sick","Dead"))
str(ds)

a <- dto[["unitData"]] %>% 
  dplyr::filter(id %in% ids) %>%
  dplyr::mutate(fu_point = fu_year) %>% 
  dplyr::select_("id", "fu_point")
b <- ds %>% dplyr::select_("id","fu_point")
ab <- dplyr::union(a,b)
dsab <- dto[["unitData"]] %>% 
  dplyr::filter(id %in% ids) %>% 
  dplyr::select(id, msex, age_at_visit)


c <- dplyr::left_join(dto[[]])
# a <- dto[["unitData"]]
# d <- ds %>% dplyr::select(id,state)
dsab <- dplyr::left_join(,a,  by="id")

table(aa$fu_point, aa$state)

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



















