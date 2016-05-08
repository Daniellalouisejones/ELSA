# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graph-presets.R") # fonts, colors, themes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals ---------------------------------------------------------
filePath <- "./data/unshared/raw/ELSA_Portland_New_nomiss_years_variablenames.dat"
variable_names <- c("idauniq", "wave", "dimar", "hedia01", "hedia02", "hedia03", "hedia04", "hedia05", "hedia06", "hedia07", "hedia08", "hedia09", "hedia10", "heage", "hedib01", "hedib02", "hedib03", "hedib04", "hedib05", "hedib06", "hedib07", "hedib08", "hedib09", "hedib10", "hesmk", "heala", "heacta", "heactb", "heactc", "mmwlka", "mmwlkb", "cflisen", "cfani", "cfmem", "cfpascr", "cflisd", "cfmscr", "cfpbscr", "cfptscr", "cfwhz1", "cfwhz2", "cfwhz3", "cfwhz4", "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh", "edqual", "scfamm", "scsca", "scscc", "intdatm", "intdaty", "iintdtm", "iintdty", "indobyr", "indager", "aethnicr", "heagcr", "heagcry", "hedbwlu", "hedbwas", "hedbwar", "hedbwos", "hedbwca", "hedbwpd", "hedbwps", "hedbwad", "hedbwde", "herosmd", "hemobcs", "hehsm86", "hehsm96", "hedcc", "heaidcr", "iintdatm", "iintdaty", "chesmk", "cfmersp", "cfprmem", "cfspeed", "cfanig", "cfexind", "cfaccur", "cfrecal", "hedimbp", "hediman", "hedimmi", "hedimhf", "hedimhm", "hedimar", "hedimdi", "hedbts", "hedimst", "hedimch", "hediblu", "hedibas", "hedibar", "hedibos", "hedibca", "hedibpd", "hedibps", "hedibad", "hedibde", "cfmeind", "cfind", "hedizm51", "hedizm52", "hediagbp", "hediagan", "hediagmi", "hediaghf", "hediaghm", "hediagar", "hediagdh", "hediagdi", "hediagst", "hediaghc", "hebdialu", "hebdiaas", "hebdiaar", "hebdiaos", "hebdiaca", "hebdiapd", "hebdiaps", "hebdiaad", "hebdiade", "hepdiagl", "hepdiadi", "hepdiamd", "hepdiaca", "cvd7dibt", "sex", "sysval", "diaval", "pulval", "mapval", "mmgsd1", "mmgsn1", "mmgsd2", "mmgsn2", "mmgsd3", "mmgsn3", "chol", "trig", "fglu", "htval", "wtval", "bmival", "bmiobe", "fvc1", "fev1", "pf1", "fvc2", "fev2", "pf2", "fvc3", "fev3", "pf3", "htfvc", "htfev", "htpf", "mmssre", "mmssti", "mmssna", "mmstre", "mmstti", "mmftre2", "mmftti", "mmlore", "mmloti", "mmlsre", "mmlsti", "mmcrre", "mmrrre", "mmrrfti", "mmrrtti", "mmrroc", "vitd", "mmftre", "prfvc", "pcfvc", "prfev", "pcfev", "htpef", "prpef", "pcpef", "bagey", "years", "diab", "cardio", "walkspee")
length(variable_names)
# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
# ds <- read.table(filePath, col.names = variable_names)
ds <- read.delim(filePath, header=TRUE, stringsAsFactors = FALSE)

names(ds) <- variable_names
# names(ds)[1:20]

# Maheeha, please see how meta-data objects are used in
# https://github.com/IALSA/ialsa-2016-groningen/blob/master/manipulation/0-ellis-island.R
# https://github.com/IALSA/ialsa-2016-amsterdam/blob/master/manipulation/map/0-ellis-island-map.R

# ---- collect-meta-data -----------------------------------------
# prepare metadata to be added to the dto
# we begin by extracting the names and (hopefuly their) labels of variables from each dataset
# and combine them in a single rectanguar object, long/stacked with respect to study names
save_csv <- names_labels(ds)
write.csv(save_csv,"./data/meta/names-labels-live.csv",row.names = T)

# ---- inspect-data -------------------------------------------------------------
names(ds)
table(ds$smoke)
ds %>%
  dplyr::group_by(wave) %>%
  dplyr::select(pcfev) %>%
dplyr::filter(!pcfev %in% c(-99999, -1)) %>%
dplyr::summarize(mean_smoke = mean(pcfev),
                 sd_smoke=  sd(pcfev),
                 count = n())

#### get temporal pattern of repsonse for a single subject for a particular variable
set.seed(7)
ds_long <- ds
(ids <- sample(unique(ds_long$idauniq),1))
d <-ds_long %>%
  dplyr::filter(idauniq %in% ids ) %>%
  dplyr::select_("idauniq","wave", "dimar")
d



# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------


# ---- wide-to-long-for-time -------------------------------
(time_invariant <- c("idauniq", "gender")) #make sure values in csv are correct!
(time_variant <- setdiff(names(ds), time_invariant))

#  melt with respect to the index type
ds_long <- data.table::melt(data =ds, id.vars = time_invariant,  measure.vars = time_variant)
table(ds_long$variable)





# ---- reproduce ---------------------------------------
rmarkdown::render(input = "./sandbox/report-a.Rmd" ,
                  output_format="html_document", clean=TRUE)
