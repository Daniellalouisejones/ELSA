



This report was automatically generated with the R package **knitr**
(version 1.6).


```r
# the purpose of this script is to create a data object (dto) which will hold all data and metadata from each candidate study of the exercise
# run the line below to stitch a basic html output. For elaborated report, run the corresponding .Rmd file
# knitr::stitch_rmd(script="./manipulation/map/0-ellis-island-map.R", output="./manipulation/map/stitched-output/0-ellis-island-map.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 
```



```r
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graph-presets.R") # fonts, colors, themes 
```

```r
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("tidyr")
```

```
## Loading required namespace: tidyr
```

```r
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit") #For asserting conditions meet expected patterns.
```

```r
data_path_input  <- "./data/unshared/derived/map-ds0.rds"
# data_path_input  <- "../MAP/data-unshared/derived/ds0.rds"
metadata_path_input <- "./data/meta/map/meta-data-map.csv" # input file with your manual classification
```

```r
unitData <- readRDS(data_path_input)
metaData <- read.csv(metadata_path_input,stringsAsFactors=F,header=T)
metaData$X <- NULL # remove automatic variable
```

```r
# basic demographic variables
ds <- unitData # assing alias
length(unique(ds$id)) # there are this many of subjects
```

```
## [1] 1696
```

```r
table(ds[,"msex"], ds[,"race"], useNA = "always")
```

```
##       
##           1    2    3    6 <NA>
##   0    6796  443   13   34    0
##   1    2310   94    8   10    0
##   <NA>    0    0    0    0    0
```

```r
t <- table(ds[,"educ"], ds[,"race"]); t[t==0]<-".";t
```

```
##     
##      1    2   3  6 
##   0  4    .   .  . 
##   2  8    .   .  . 
##   3  7    .   2  . 
##   4  16   1   .  . 
##   5  17   4   .  . 
##   6  42   3   .  . 
##   7  26   .   .  . 
##   8  141  25  .  . 
##   9  63   8   .  . 
##   10 134  22  .  5 
##   11 186  23  .  . 
##   12 2103 126 .  10
##   13 727  79  .  6 
##   14 1042 92  .  . 
##   15 473  36  .  13
##   16 1949 66  14 2 
##   17 459  .   .  . 
##   18 847  41  5  8 
##   19 239  7   .  . 
##   20 265  4   .  . 
##   21 194  .   .  . 
##   22 58   .   .  . 
##   23 34   .   .  . 
##   24 26   .   .  . 
##   25 10   .   .  . 
##   28 21   .   .  .
```

```r
# age variables
t <- table(ds[,"fu_year"], ds[,"died"]); t[t==0]<-".";t
```

```
##     
##      0    1  
##   0  1014 681
##   1  873  594
##   2  739  527
##   3  632  446
##   4  539  369
##   5  427  313
##   6  407  259
##   7  360  198
##   8  327  139
##   9  249  93 
##   10 176  61 
##   11 94   39 
##   12 32   24 
##   13 27   17 
##   14 22   6  
##   15 19   2  
##   16 2    .
```

```r
# histogram_continuous(ds, variable_name = "age_death", bin_width = 1)
# histogram_continuous(ds, variable_name = "age_bl", bin_width = 1)
# histogram_continuous(ds, variable_name = "age_at_visit", bin_width = 1)
```

```r
dto <- list()
# the first element of data transfer object contains unit data
dto[["unitData"]] <- unitData
# the second element of data transfer object contains meta data
dto[["metaData"]] <-  metaData
# verify and glimpse
dto[["unitData"]] %>% dplyr::glimpse()
```

```
## Variables:
## $ id                    (int) 9121, 9121, 9121, 9121, 9121, 33027, 330...
## $ study                 (chr) "MAP ", "MAP ", "MAP ", "MAP ", "MAP ", ...
## $ scaled_to.x           (chr) "MAP ", "MAP ", "MAP ", "MAP ", "MAP ", ...
## $ agreeableness         (int) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ conscientiousness     (int) 35, 35, 35, 35, 35, NA, NA, NA, NA, NA, ...
## $ extraversion          (int) 34, 34, 34, 34, 34, 30, 30, 32, 32, 32, ...
## $ neo_altruism          (int) 27, 27, 27, 27, 27, NA, NA, NA, NA, NA, ...
## $ neo_conscientiousness (int) 35, 35, 35, 35, 35, NA, NA, NA, NA, NA, ...
## $ neo_trust             (int) 25, 25, 25, 25, 25, NA, NA, NA, NA, NA, ...
## $ openness              (int) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ anxiety_10items       (dbl) 0, 0, 0, 0, 0, NA, NA, 0, 0, 0, 0, 0, 0,...
## $ neuroticism_12        (int) 9, 9, 9, 9, 9, NA, NA, 16, 16, 16, 16, 1...
## $ neuroticism_6         (int) 8, 8, 8, 8, 8, 1, 1, 14, 14, 14, 14, 14,...
## $ age_bl                (dbl) 79.97, 79.97, 79.97, 79.97, 79.97, 81.01...
## $ age_death             (dbl) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ died                  (int) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ educ                  (int) 12, 12, 12, 12, 12, 14, 14, 8, 8, 8, 8, ...
## $ msex                  (int) 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0...
## $ race                  (int) 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1...
## $ spanish               (int) 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2...
## $ apoe_genotype         (int) 34, 34, 34, 34, 34, 33, 33, 33, 33, 33, ...
## $ ldai_bl               (dbl) 0.2, 0.2, 0.2, 0.2, 0.2, 0.0, 0.0, 0.0, ...
## $ q3smo_bl              (int) 20, 20, 20, 20, 20, NA, NA, NA, NA, NA, ...
## $ q4smo_bl              (int) 31, 31, 31, 31, 31, NA, NA, NA, NA, NA, ...
## $ smoke_bl              (int) 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ smoking               (int) 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ fu_year               (int) 0, 1, 2, 3, 4, 0, 1, 0, 2, 3, 4, 5, 6, 0...
## $ scaled_to.y           (chr) "RMM", "RMM", "RMM", "RMM", "RMM", "RMM"...
## $ cesdsum               (int) 0, 0, 0, 0, 0, 1, 2, 6, 1, 3, 1, 0, 2, 1...
## $ r_depres              (int) 4, 4, 4, 4, NA, 4, 4, 3, 4, 4, 4, 4, 4, ...
## $ intrusion             (dbl) NA, NA, NA, 2.000, NA, NA, NA, NA, NA, N...
## $ neglifeevents         (int) NA, NA, NA, 4, NA, NA, NA, NA, NA, NA, N...
## $ negsocexchange        (dbl) NA, NA, NA, 1.833, NA, NA, NA, NA, NA, N...
## $ nohelp                (dbl) NA, NA, NA, 1.667, NA, NA, NA, NA, NA, N...
## $ panas                 (dbl) NA, NA, NA, 1.1, NA, NA, NA, NA, NA, NA,...
## $ perceivedstress       (dbl) NA, NA, NA, 2.5, NA, NA, NA, NA, NA, NA,...
## $ rejection             (dbl) NA, NA, NA, 1.667, NA, NA, NA, NA, NA, N...
## $ unsympathetic         (dbl) NA, NA, NA, 2, NA, NA, NA, NA, NA, NA, N...
## $ dcfdx                 (int) 1, 1, 1, 1, NA, 1, NA, 1, 2, 1, 1, 1, 1,...
## $ dementia              (int) 0, 0, 0, 0, NA, 0, NA, 0, 0, 0, 0, 0, 0,...
## $ r_stroke              (int) NA, NA, 4, 4, NA, 4, NA, 4, NA, 4, NA, N...
## $ cogn_ep               (dbl) 0.0733900, -0.0022709, -0.5334674, 0.211...
## $ cogn_global           (dbl) 0.25733, 0.33753, 0.06659, 0.40477, 0.36...
## $ cogn_po               (dbl) 0.299150, 0.893262, 0.893262, 0.885836, ...
## $ cogn_ps               (dbl) 0.48564, 0.88321, 0.78369, 0.59650, 0.82...
## $ cogn_se               (dbl) 0.7804552, 0.9388327, 1.2027951, 1.25558...
## $ cogn_wo               (dbl) 0.1576990, -0.0047532, -0.3199035, -0.16...
## $ cts_bname             (int) 15, 15, 15, 15, 15, 12, NA, 15, 15, 15, ...
## $ cts_catflu            (int) 40, 43, 48, 49, 45, 20, NA, 31, 27, 26, ...
## $ cts_db                (int) 6, 6, 4, 4, 4, 6, NA, 2, 4, 3, 5, 3, 4, ...
## $ cts_delay             (int) 10, 7, 7, 10, 10, 4, NA, 14, 11, 11, 6, ...
## $ cts_df                (int) 9, 8, 9, 9, 9, 8, NA, 6, 8, 9, 9, 10, 9,...
## $ cts_doperf            (int) 7, 7, 6, 7, 8, 7, NA, 7, 4, 5, 5, 6, 6, ...
## $ cts_ebdr              (int) 9, 9, 6, 8, 9, 10, NA, 8, 0, 8, 7, 10, 1...
## $ cts_ebmt              (int) 7, 9, 6, 12, 9, 11, NA, 8, 10, 7, 9, 12,...
## $ cts_idea              (int) 8, 8, 8, 8, 8, 8, NA, 7, 8, 8, 8, 8, 8, ...
## $ cts_lopair            (int) 9, 14, 14, 15, 15, 5, NA, 10, 12, 8, 7, ...
## $ cts_mmse30            (dbl) 29, 29, 30, 30, 29, 29, NA, 27, 25, 28, ...
## $ cts_nccrtd            (int) 24, 34, 26, 27, 28, 19, NA, 16, 19, 13, ...
## $ cts_pmat              (int) 13, 12, 12, 11, 11, 10, NA, 11, 7, 13, 1...
## $ cts_read_nart         (int) 9, 8, 9, 8, 9, 10, NA, 5, 5, 3, 3, 3, 4,...
## $ cts_read_wrat         (int) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ cts_sdmt              (int) 49, 43, 53, 47, 51, 39, NA, 24, 21, 30, ...
## $ cts_story             (int) 10, 9, 7, 8, 11, 8, NA, 14, 14, 10, 9, 8...
## $ cts_wli               (int) 19, 18, 17, 18, 16, 20, NA, 16, 13, 11, ...
## $ cts_wlii              (int) 7, 6, 5, 6, 5, 5, NA, 5, 5, 3, 5, 5, 3, ...
## $ cts_wliii             (int) 10, 10, 10, 10, 10, 10, NA, 10, 10, 10, ...
## $ age_at_visit          (dbl) 79.97, 81.08, 81.61, 82.60, 83.62, 81.01...
## $ iadlsum               (int) 0, 0, 0, 0, 0, 5, 3, 0, 0, 0, 0, 0, 0, 1...
## $ katzsum               (int) 6, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0...
## $ rosbscl               (int) 1, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 2, 3, 2...
## $ rosbsum               (int) 2, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 1, 0, 1...
## $ vision                (int) 1, 1, 1, 2, 2, 1, 4, 1, 2, 1, NA, 2, 7, ...
## $ visionlog             (dbl) 0.3, 0.3, 0.3, 0.4, 0.4, 0.3, 0.7, 0.3, ...
## $ fev                   (dbl) 1.650, 1.280, 1.655, NA, 1.525, 1.110, N...
## $ mep                   (dbl) 39.5, 33.5, 43.0, NA, 39.0, 84.0, NA, 95...
## $ mip                   (dbl) 32.5, 36.5, 35.0, NA, 41.5, 73.5, NA, 82...
## $ pvc                   (dbl) 2.285, 1.285, 1.690, NA, 1.940, 1.510, N...
## $ bun                   (int) 13, NA, 13, 13, 14, 32, 35, 14, 12, 12, ...
## $ ca                    (dbl) 9.5, NA, 9.4, 9.4, 9.2, 9.2, 8.8, 9.7, 9...
## $ chlstrl               (int) 235, NA, 205, 230, 210, 201, 197, 134, 1...
## $ cl                    (int) 96, NA, 102, 93, 99, 103, 103, 102, 104,...
## $ co2                   (int) 24, NA, 24, 25, 25, 22, 25, 23, 20, 21, ...
## $ crn                   (dbl) 0.85, NA, 0.98, 0.89, 0.82, 1.30, 2.00, ...
## $ fasting               (int) 2, NA, 2, 2, 2, 3, 3, 3, 2, 1, NA, 3, 1,...
## $ glucose               (int) 92, NA, 77, 85, 92, 180, 153, 170, 203, ...
## $ hba1c                 (dbl) 5.6, NA, 5.5, 5.5, 6.1, NA, NA, NA, 7.0,...
## $ hdlchlstrl            (int) 79, NA, 76, 79, 69, 50, 51, 30, 37, 40, ...
## $ hdlratio              (dbl) 3.0, NA, 2.7, 2.9, 3.0, 4.0, 3.9, 4.5, 4...
## $ k                     (dbl) 5.0, NA, 4.2, 4.3, 4.5, 4.8, 4.6, 4.2, 4...
## $ ldlchlstrl            (int) 133, NA, 112, 134, 119, 107, 99, NA, 48,...
## $ na                    (int) 136, NA, 139, 130, 136, 139, 140, 139, 1...
## $ alcohol_g             (dbl) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ bmi                   (dbl) 26.83, 27.62, 26.98, 26.40, 35.14, 32.59...
## $ htm                   (dbl) 1.778, 1.753, 1.778, 1.778, 1.549, 1.600...
## $ phys5itemsum          (dbl) 1.7500, 4.0417, 0.6250, 0.4583, 2.1667, ...
## $ wtkg                  (dbl) 84.82, 84.82, 85.28, 83.46, 84.37, 83.46...
## $ bp11                  (chr) "159/090", "145/090", "160/095", "147/09...
## $ bp2                   (chr) "159/090", "150/084", "160/088", "149/09...
## $ bp3                   (int) 1, 1, NA, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1,...
## $ bp31                  (chr) "157/082", "109/077", "151/082", "138/08...
## $ hypertension_cum      (int) 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0...
## $ dm_cum                (int) 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0...
## $ thyroid_cum           (int) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ chf_cum               (int) 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0...
## $ claudication_cum      (int) 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0...
## $ heart_cum             (int) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ stroke_cum            (int) NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ vasc_3dis_sum         (dbl) 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0...
## $ vasc_4dis_sum         (dbl) 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0...
## $ vasc_risks_sum        (dbl) 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0...
## $ gait_speed            (dbl) 0.4394, NA, NA, 0.4877, 0.3483, 0.4829, ...
## $ gripavg               (dbl) 41.00, 41.00, 35.50, NA, 25.75, 59.00, N...
```

```r
dto[["metaData"]] %>% dplyr::glimpse()
```

```
## Variables:
## $ name          (chr) "id", "study", "scaled_to.x", "agreeableness", "...
## $ label         (chr) NA, NA, NA, "NEO agreeableness-ROS", "Conscienti...
## $ type          (chr) "design", "design", "design", "personality", "pe...
## $ name_new      (chr) "id", "study", "scaled_to.x", "agreeableness", "...
## $ construct     (chr) "id", "", "", "", "", "", "", "", "", "", "", ""...
## $ self_reported (lgl) FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
## $ longitudinal  (lgl) FALSE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
## $ unit          (chr) "person", "", "", "", "", "", "", "", "", "", ""...
## $ include       (lgl) TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA...
```

```r
meta_data <- dto[["metaData"]] %>%
  dplyr::filter(include == TRUE) %>%
  # dplyr::select(name, name_new, type, label, construct) %>%
  dplyr::arrange(type, construct, name)
knitr::kable(meta_data)
```

```
## 
## 
## |name          |label                                                      |type        |name_new      |construct              |self_reported |longitudinal |unit             |include |
## |:-------------|:----------------------------------------------------------|:-----------|:-------------|:----------------------|:-------------|:------------|:----------------|:-------|
## |apoe_genotype |ApoE genotype                                              |clinical    |apoe_genotype |apoe                   |FALSE         |FALSE        |scale            |TRUE    |
## |chf_cum       |Medical Conditions - congestive heart failure -
## cumulative |clinical    |chf_cum       |cardio                 | TRUE         | TRUE        |category         |TRUE    |
## |dcfdx         |Clinical dx summary                                        |clinical    |dcfdx         |cognition              |FALSE         | TRUE        |category         |TRUE    |
## |r_stroke      |Clinical stroke dx                                         |clinical    |r_stroke      |stroke                 |FALSE         | TRUE        |category         |TRUE    |
## |stroke_cum    |Clinical Diagnoses - Stroke - cumulative                   |clinical    |stroke_cum    |stroke                 |FALSE         | TRUE        |category         |TRUE    |
## |cts_mmse30    |MMSE - 2014                                                |cognitive   |mmse          |dementia               | TRUE         | TRUE        |0 to 30          |TRUE    |
## |dementia      |Dementia diagnosis                                         |cognitive   |dementia      |dementia               |FALSE         | TRUE        |0, 1             |TRUE    |
## |cogn_ep       |Calculated domain score-episodic memory                    |cognitive   |cogn_ep       |episodic memory        |FALSE         | TRUE        |composite        |TRUE    |
## |cogn_global   |Global cognitive score                                     |cognitive   |cogn_global   |global cognition       |FALSE         | TRUE        |composite        |TRUE    |
## |cogn_po       |Calculated domain score - perceptual orientation           |cognitive   |cogn_po       |perceptual orientation |FALSE         | TRUE        |composite        |TRUE    |
## |cogn_ps       |Calculated domain score - perceptual speed                 |cognitive   |cogn_ps       |perceptual speed       |FALSE         | TRUE        |composite        |TRUE    |
## |cogn_se       |Calculated domain score - semantic memory                  |cognitive   |cogn_se       |semantic memory        |FALSE         | TRUE        |composite        |TRUE    |
## |cogn_wo       |Calculated domain score - working memory                   |cognitive   |cogn_wo       |working memory         |FALSE         | TRUE        |composite        |TRUE    |
## |age_bl        |Age at baseline                                            |demographic |age_bl        |age                    |FALSE         |FALSE        |year             |TRUE    |
## |age_death     |Age at death                                               |demographic |age_death     |age                    |FALSE         |FALSE        |year             |TRUE    |
## |died          |Indicator of death                                         |demographic |died          |age                    |FALSE         |FALSE        |category         |TRUE    |
## |educ          |Years of education                                         |demographic |educ          |education              | TRUE         |FALSE        |years            |TRUE    |
## |race          |Participant's race                                         |demographic |race          |race                   | TRUE         |FALSE        |category         |TRUE    |
## |spanish       |Spanish/Hispanic origin                                    |demographic |spanish       |race                   | TRUE         |FALSE        |category         |TRUE    |
## |msex          |Gender                                                     |demographic |msex          |sex                    |FALSE         |FALSE        |category         |TRUE    |
## |id            |NA                                                         |design      |id            |id                     |FALSE         |FALSE        |person           |TRUE    |
## |fu_year       |Follow-up year                                             |design      |fu_year       |time                   |FALSE         | TRUE        |time point       |TRUE    |
## |iadlsum       |Instrumental activities of daily liviing                   |physical    |iadlsum       |physact                | TRUE         | TRUE        |scale            |TRUE    |
## |fev           |forced expiratory volume                                   |physical    |fev           |physcap                |FALSE         | TRUE        |liters           |TRUE    |
## |gait_speed    |Gait Speed - MAP                                           |physical    |gait_speed    |physcap                |FALSE         | TRUE        |min/sec          |TRUE    |
## |gripavg       |Extremity strength                                         |physical    |gripavg       |physcap                |FALSE         | TRUE        |lbs              |TRUE    |
## |katzsum       |Katz measure of disability                                 |physical    |katzsum       |physcap                | TRUE         | TRUE        |scale            |TRUE    |
## |mep           |maximal expiratory pressure                                |physical    |mep           |physcap                |FALSE         | TRUE        |cm H20           |TRUE    |
## |mip           |maximal inspiratory pressure                               |physical    |mip           |physcap                |FALSE         | TRUE        |cm H20           |TRUE    |
## |pvc           |pulmonary vital capacity                                   |physical    |pvc           |physcap                |FALSE         | TRUE        |liters           |TRUE    |
## |rosbscl       |Rosow-Breslau scale                                        |physical    |rosbscl       |physcap                | TRUE         | TRUE        |scale            |TRUE    |
## |rosbsum       |Rosow-Breslau scale                                        |physical    |rosbsum       |physcap                | TRUE         | TRUE        |scale            |TRUE    |
## |vision        |Vision acuity                                              |physical    |vision        |physcap                |FALSE         | TRUE        |scale            |TRUE    |
## |visionlog     |Visual acuity                                              |physical    |visionlog     |physcap                |FALSE         | TRUE        |scale            |TRUE    |
## |bmi           |Body mass index                                            |physical    |bmi           |physique               |FALSE         | TRUE        |kg/msq           |TRUE    |
## |htm           |Height(meters)                                             |physical    |htm           |physique               |FALSE         | TRUE        |meters           |TRUE    |
## |wtkg          |Weight (kg)                                                |physical    |wtkg          |physique               |FALSE         | TRUE        |kilos            |TRUE    |
## |alcohol_g     |Grams of alcohol per day                                   |substance   |alcohol_g     |alcohol                | TRUE         |FALSE        |grams            |TRUE    |
## |ldai_bl       |Lifetime daily alcohol intake -baseline                    |substance   |alco_life     |alcohol                | TRUE         |FALSE        |drinks/day       |TRUE    |
## |q3smo_bl      |Smoking quantity-baseline                                  |substance   |q3smo_bl      |smoking                | TRUE         |FALSE        |cigarettes / day |TRUE    |
## |q4smo_bl      |Smoking duration-baseline                                  |substance   |q4smo_bl      |smoking                | TRUE         |FALSE        |years            |TRUE    |
## |smoke_bl      |Smoking at baseline                                        |substance   |smoke_bl      |smoking                | TRUE         |FALSE        |category         |TRUE    |
## |smoking       |Smoking                                                    |substance   |smoking       |smoking                | TRUE         |FALSE        |category         |TRUE    |
```

```r
# rename variables
d_rules <- dto[["metaData"]] %>%
  dplyr::filter(name %in% names(ds)) %>% 
  dplyr::select(name, name_new ) # leave only collumn, which values you wish to append
names(ds) <- d_rules[,"name_new"]
# transfer changes to dto
dto[["unitData"]] <- ds
```

```r
# Save as a compress, binary R dataset.  It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data/unshared/derived/dto.rds", compress="xz")
```

```r
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/dto.rds")
names(dto)
```

```
## [1] "unitData" "metaData"
```

```r
# 1st element - unit(person) level data
dplyr::tbl_df(dto[["unitData"]])
```

```
## Source: local data frame [9,708 x 113]
## 
##        id study scaled_to.x agreeableness
## 1    9121  MAP         MAP             NA
## 2    9121  MAP         MAP             NA
## 3    9121  MAP         MAP             NA
## 4    9121  MAP         MAP             NA
## 5    9121  MAP         MAP             NA
## 6   33027  MAP         MAP             NA
## 7   33027  MAP         MAP             NA
## 8  204228  MAP         MAP             NA
## 9  204228  MAP         MAP             NA
## 10 204228  MAP         MAP             NA
## ..    ...   ...         ...           ...
## Variables not shown: conscientiousness
##   (int), extraversion (int), neo_altruism
##   (int), neo_conscientiousness (int),
##   neo_trust (int), openness (int),
##   anxiety_10items (dbl), neuroticism_12
##   (int), neuroticism_6 (int), age_bl
##   (dbl), age_death (dbl), died (int),
##   educ (int), msex (int), race (int),
##   spanish (int), apoe_genotype (int),
##   alco_life (dbl), q3smo_bl (int),
##   q4smo_bl (int), smoke_bl (int), smoking
##   (int), fu_year (int), scaled_to.y
##   (chr), cesdsum (int), r_depres (int),
##   intrusion (dbl), neglifeevents (int),
##   negsocexchange (dbl), nohelp (dbl),
##   panas (dbl), perceivedstress (dbl),
##   rejection (dbl), unsympathetic (dbl),
##   dcfdx (int), dementia (int), r_stroke
##   (int), cogn_ep (dbl), cogn_global
##   (dbl), cogn_po (dbl), cogn_ps (dbl),
##   cogn_se (dbl), cogn_wo (dbl), cts_bname
##   (int), catfluency (int), cts_db (int),
##   cts_delay (int), cts_df (int),
##   cts_doperf (int), cts_ebdr (int),
##   cts_ebmt (int), cts_idea (int),
##   cts_lopair (int), mmse (dbl),
##   cts_nccrtd (int), cts_pmat (int),
##   cts_read_nart (int), cts_read_wrat
##   (int), cts_sdmt (int), cts_story (int),
##   cts_wli (int), cts_wlii (int),
##   cts_wliii (int), age_at_visit (dbl),
##   iadlsum (int), katzsum (int), rosbscl
##   (int), rosbsum (int), vision (int),
##   visionlog (dbl), fev (dbl), mep (dbl),
##   mip (dbl), pvc (dbl), bun (int), ca
##   (dbl), chlstrl (int), cl (int), co2
##   (int), crn (dbl), fasting (int),
##   glucose (int), hba1c (dbl), hdlchlstrl
##   (int), hdlratio (dbl), k (dbl),
##   ldlchlstrl (int), na (int), alcohol_g
##   (dbl), bmi (dbl), htm (dbl),
##   phys5itemsum (dbl), wtkg (dbl), bp11
##   (chr), bp2 (chr), bp3 (int), bp31
##   (chr), hypertension_cum (int), dm_cum
##   (int), thyroid_cum (int), chf_cum
##   (int), claudication_cum (int),
##   heart_cum (int), stroke_cum (int),
##   vasc_3dis_sum (dbl), vasc_4dis_sum
##   (dbl), vasc_risks_sum (dbl), gait_speed
##   (dbl), gripavg (dbl)
```

```r
# 2nd element - meta data, info about variables
dto[["metaData"]]
```

```
##                      name
## 1                      id
## 2                   study
## 3             scaled_to.x
## 4           agreeableness
## 5       conscientiousness
## 6            extraversion
## 7            neo_altruism
## 8   neo_conscientiousness
## 9               neo_trust
## 10               openness
## 11        anxiety_10items
## 12         neuroticism_12
## 13          neuroticism_6
## 14                 age_bl
## 15              age_death
## 16                   died
## 17                   educ
## 18                   msex
## 19                   race
## 20                spanish
## 21          apoe_genotype
## 22                ldai_bl
## 23               q3smo_bl
## 24               q4smo_bl
## 25               smoke_bl
## 26                smoking
## 27                fu_year
## 28            scaled_to.y
## 29                cesdsum
## 30               r_depres
## 31              intrusion
## 32          neglifeevents
## 33         negsocexchange
## 34                 nohelp
## 35                  panas
## 36        perceivedstress
## 37              rejection
## 38          unsympathetic
## 39                  dcfdx
## 40               dementia
## 41               r_stroke
## 42                cogn_ep
## 43            cogn_global
## 44                cogn_po
## 45                cogn_ps
## 46                cogn_se
## 47                cogn_wo
## 48              cts_bname
## 49             cts_catflu
## 50                 cts_db
## 51              cts_delay
## 52                 cts_df
## 53             cts_doperf
## 54               cts_ebdr
## 55               cts_ebmt
## 56               cts_idea
## 57             cts_lopair
## 58             cts_mmse30
## 59             cts_nccrtd
## 60               cts_pmat
## 61          cts_read_nart
## 62          cts_read_wrat
## 63               cts_sdmt
## 64              cts_story
## 65                cts_wli
## 66               cts_wlii
## 67              cts_wliii
## 68           age_at_visit
## 69                iadlsum
## 70                katzsum
## 71                rosbscl
## 72                rosbsum
## 73                 vision
## 74              visionlog
## 75                    fev
## 76                    mep
## 77                    mip
## 78                    pvc
## 79                    bun
## 80                     ca
## 81                chlstrl
## 82                     cl
## 83                    co2
## 84                    crn
## 85                fasting
## 86                glucose
## 87                  hba1c
## 88             hdlchlstrl
## 89               hdlratio
## 90                      k
## 91             ldlchlstrl
## 92                     na
## 93              alcohol_g
## 94                    bmi
## 95                    htm
## 96           phys5itemsum
## 97                   wtkg
## 98                   bp11
## 99                    bp2
## 100                   bp3
## 101                  bp31
## 102      hypertension_cum
## 103                dm_cum
## 104           thyroid_cum
## 105               chf_cum
## 106      claudication_cum
## 107             heart_cum
## 108            stroke_cum
## 109         vasc_3dis_sum
## 110         vasc_4dis_sum
## 111        vasc_risks_sum
## 112            gait_speed
## 113               gripavg
##                                                                      label
## 1                                                                     <NA>
## 2                                                                     <NA>
## 3                                                                     <NA>
## 4                                                    NEO agreeableness-ROS
## 5                                                Conscientiousness-ROS/MAP
## 6                                                 NEO extraversion-ROS/MAP
## 7                                                   NEO altruism scale-MAP
## 8                                                NEO conscientiousness-MAP
## 9                                                            NEO trust-MAP
## 10                                                         NEO openess-ROS
## 11                                   Anxiety-10 item version - ROS and MAP
## 12                                       Neuroticism - 12 item version-RMM
## 13                                      Neuroticism - 6 item version - RMM
## 14                                                         Age at baseline
## 15                                                            Age at death
## 16                                                      Indicator of death
## 17                                                      Years of education
## 18                                                                  Gender
## 19                                                      Participant's race
## 20                                                 Spanish/Hispanic origin
## 21                                                           ApoE genotype
## 22                                 Lifetime daily alcohol intake -baseline
## 23                                               Smoking quantity-baseline
## 24                                               Smoking duration-baseline
## 25                                                     Smoking at baseline
## 26                                                                 Smoking
## 27                                                          Follow-up year
## 28                                              No label found in codebook
## 29                                     CESD-Measure of depressive symptoms
## 30                                       Major depression dx-clinic rating
## 31                                  Negative social exchange-intrusion-MAP
## 32                                                    Negative life events
## 33                                                Negative social exchange
## 34                                       Negative social exchange-help-MAP
## 35                                                             Panas score
## 36                                                        Perceived stress
## 37                                Negative social exchange - rejection-MAP
## 38                             Negative social exchange-unsymapathetic-MAP
## 39                                                     Clinical dx summary
## 40                                                      Dementia diagnosis
## 41                                                      Clinical stroke dx
## 42                                 Calculated domain score-episodic memory
## 43                                                  Global cognitive score
## 44                        Calculated domain score - perceptual orientation
## 45                              Calculated domain score - perceptual speed
## 46                               Calculated domain score - semantic memory
## 47                                Calculated domain score - working memory
## 48                                                    Boston naming - 2014
## 49                                                 Category fluency - 2014
## 50                                                 Digits backwards - 2014
## 51                                               Logical memory IIa - 2014
## 52                                                  Digits forwards - 2014
## 53                                                   Digit ordering - 2014
## 54                               East Boston story - delayed recall - 2014
## 55                                    East Boston story - immediate - 2014
## 56                                                    Complex ideas - 2014
## 57                                                 Line orientation - 2014
## 58                                                             MMSE - 2014
## 59                                                Number comparison - 2014
## 60                                             Progressive Matrices - 2014
## 61                                                  Reading test-NART-2014
## 62                                              Reading test - WRAT - 2014
## 63                                        Symbol digit modalitities - 2014
## 64                                    Logical memory Ia - immediate - 2014
## 65                                            Word list I- immediate- 2014
## 66                                           Word list II - delayed - 2014
## 67                                      Word list III - recognition - 2014
## 68                                               Age at cycle - fractional
## 69                                Instrumental activities of daily liviing
## 70                                              Katz measure of disability
## 71                                                     Rosow-Breslau scale
## 72                                                     Rosow-Breslau scale
## 73                                                           Vision acuity
## 74                                                           Visual acuity
## 75                                                forced expiratory volume
## 76                                             maximal expiratory pressure
## 77                                            maximal inspiratory pressure
## 78                                                pulmonary vital capacity
## 79                                                     Blood urea nitrogen
## 80                                                                 Calcium
## 81                                                             Cholesterol
## 82                                                                Chloride
## 83                                                          Carbon Dioxide
## 84                                                              Creatinine
## 85                      Whether blood was collected on fasting participant
## 86                                                                 Glucose
## 87                                                          Hemoglobin A1c
## 88                                                         HDL cholesterol
## 89                                                               HDL ratio
## 90                                                               Potassium
## 91                                                         LDL cholesterol
## 92                                                                  Sodium
## 93                                                Grams of alcohol per day
## 94                                                         Body mass index
## 95                                                          Height(meters)
## 96  Summary of self reported physical activity\nmeasure (in hours) ROS/MAP
## 97                                                             Weight (kg)
## 98                           Blood pressure measurement- sitting - trial 1
## 99                           Blood pressure measurement- sitting - trial 2
## 100                                                     Hx of Meds for HTN
## 101                                   Blood pressure measurement- standing
## 102                         Medical conditions - hypertension - cumulative
## 103                                Medical history - diabetes - cumulative
## 104                      Medical Conditions - thyroid disease - cumulative
## 105            Medical Conditions - congestive heart failure -\ncumulative
## 106                          Medical conditions - claudication -cumulative
## 107                                Medical Conditions - heart - cumulative
## 108                               Clinical Diagnoses - Stroke - cumulative
## 109                Vascular disease burden (3 items w/o chf)\nROS/MAP/MARS
## 110                     Vascular disease burden (4 items) - MAP/MARS\nonly
## 111                                          Vascular disease risk factors
## 112                                                       Gait Speed - MAP
## 113                                                     Extremity strength
##              type              name_new
## 1          design                    id
## 2          design                 study
## 3          design           scaled_to.x
## 4     personality         agreeableness
## 5     personality     conscientiousness
## 6     personality          extraversion
## 7     personality          neo_altruism
## 8     personality neo_conscientiousness
## 9     personality             neo_trust
## 10    personality              openness
## 11    personality       anxiety_10items
## 12    personality        neuroticism_12
## 13    personality         neuroticism_6
## 14    demographic                age_bl
## 15    demographic             age_death
## 16    demographic                  died
## 17    demographic                  educ
## 18    demographic                  msex
## 19    demographic                  race
## 20    demographic               spanish
## 21       clinical         apoe_genotype
## 22      substance             alco_life
## 23      substance              q3smo_bl
## 24      substance              q4smo_bl
## 25      substance              smoke_bl
## 26      substance               smoking
## 27         design               fu_year
## 28         design           scaled_to.y
## 29  psychological               cesdsum
## 30  psychological              r_depres
## 31  psychological             intrusion
## 32  psychological         neglifeevents
## 33  psychological        negsocexchange
## 34  psychological                nohelp
## 35  psychological                 panas
## 36  psychological       perceivedstress
## 37  psychological             rejection
## 38  psychological         unsympathetic
## 39       clinical                 dcfdx
## 40      cognitive              dementia
## 41       clinical              r_stroke
## 42      cognitive               cogn_ep
## 43      cognitive           cogn_global
## 44      cognitive               cogn_po
## 45      cognitive               cogn_ps
## 46      cognitive               cogn_se
## 47      cognitive               cogn_wo
## 48      cognitive             cts_bname
## 49      cognitive            catfluency
## 50      cognitive                cts_db
## 51      cognitive             cts_delay
## 52      cognitive                cts_df
## 53      cognitive            cts_doperf
## 54      cognitive              cts_ebdr
## 55      cognitive              cts_ebmt
## 56      cognitive              cts_idea
## 57      cognitive            cts_lopair
## 58      cognitive                  mmse
## 59      cognitive            cts_nccrtd
## 60      cognitive              cts_pmat
## 61      cognitive         cts_read_nart
## 62      cognitive         cts_read_wrat
## 63      cognitive              cts_sdmt
## 64      cognitive             cts_story
## 65      cognitive               cts_wli
## 66      cognitive              cts_wlii
## 67      cognitive             cts_wliii
## 68    demographic          age_at_visit
## 69       physical               iadlsum
## 70       physical               katzsum
## 71       physical               rosbscl
## 72       physical               rosbsum
## 73       physical                vision
## 74       physical             visionlog
## 75       physical                   fev
## 76       physical                   mep
## 77       physical                   mip
## 78       physical                   pvc
## 79       clinical                   bun
## 80       clinical                    ca
## 81       clinical               chlstrl
## 82       clinical                    cl
## 83       clinical                   co2
## 84       clinical                   crn
## 85       clinical               fasting
## 86       clinical               glucose
## 87       clinical                 hba1c
## 88       clinical            hdlchlstrl
## 89       clinical              hdlratio
## 90       clinical                     k
## 91       clinical            ldlchlstrl
## 92       clinical                    na
## 93      substance             alcohol_g
## 94       physical                   bmi
## 95       physical                   htm
## 96       physical          phys5itemsum
## 97       physical                  wtkg
## 98       clinical                  bp11
## 99       clinical                   bp2
## 100      clinical                   bp3
## 101      clinical                  bp31
## 102      clinical      hypertension_cum
## 103      clinical                dm_cum
## 104      clinical           thyroid_cum
## 105      clinical               chf_cum
## 106      clinical      claudication_cum
## 107      clinical             heart_cum
## 108      clinical            stroke_cum
## 109      clinical         vasc_3dis_sum
## 110      clinical         vasc_4dis_sum
## 111      clinical        vasc_risks_sum
## 112      physical            gait_speed
## 113      physical               gripavg
##                  construct self_reported
## 1                       id         FALSE
## 2                                     NA
## 3                                     NA
## 4                                     NA
## 5                                     NA
## 6                                     NA
## 7                                     NA
## 8                                     NA
## 9                                     NA
## 10                                    NA
## 11                                    NA
## 12                                    NA
## 13                                    NA
## 14                     age         FALSE
## 15                     age         FALSE
## 16                     age         FALSE
## 17               education          TRUE
## 18                     sex         FALSE
## 19                    race          TRUE
## 20                    race          TRUE
## 21                    apoe         FALSE
## 22                 alcohol          TRUE
## 23                 smoking          TRUE
## 24                 smoking          TRUE
## 25                 smoking          TRUE
## 26                 smoking          TRUE
## 27                    time         FALSE
## 28                                    NA
## 29                                    NA
## 30                                    NA
## 31                                    NA
## 32                                    NA
## 33                                    NA
## 34                                    NA
## 35                                    NA
## 36                                    NA
## 37                                    NA
## 38                                    NA
## 39               cognition         FALSE
## 40                dementia         FALSE
## 41                  stroke         FALSE
## 42         episodic memory         FALSE
## 43        global cognition         FALSE
## 44  perceptual orientation         FALSE
## 45        perceptual speed         FALSE
## 46         semantic memory         FALSE
## 47          working memory         FALSE
## 48         semantic memory         FALSE
## 49         semantic memory         FALSE
## 50          working memory         FALSE
## 51         episodic memory         FALSE
## 52          working memory         FALSE
## 53          working memory         FALSE
## 54         episodic memory         FALSE
## 55         episodic memory         FALSE
## 56    verbal comprehension         FALSE
## 57  perceptual orientation         FALSE
## 58                dementia          TRUE
## 59        perceptual speed         FALSE
## 60  perceptual orientation         FALSE
## 61         semantic memory         FALSE
## 62         semantic memory         FALSE
## 63        perceptual speed         FALSE
## 64         episodic memory         FALSE
## 65         episodic memory         FALSE
## 66         episodic memory         FALSE
## 67         episodic memory         FALSE
## 68                                    NA
## 69                 physact          TRUE
## 70                 physcap          TRUE
## 71                 physcap          TRUE
## 72                 physcap          TRUE
## 73                 physcap         FALSE
## 74                 physcap         FALSE
## 75                 physcap         FALSE
## 76                 physcap         FALSE
## 77                 physcap         FALSE
## 78                 physcap         FALSE
## 79                                    NA
## 80                                    NA
## 81                                    NA
## 82                                    NA
## 83                                    NA
## 84                                    NA
## 85                                    NA
## 86                                    NA
## 87                                    NA
## 88                                    NA
## 89                                    NA
## 90                                    NA
## 91                                    NA
## 92                                    NA
## 93                 alcohol          TRUE
## 94                physique         FALSE
## 95                physique         FALSE
## 96                 physact          TRUE
## 97                physique         FALSE
## 98            hypertension         FALSE
## 99            hypertension         FALSE
## 100           hypertension          TRUE
## 101           hypertension         FALSE
## 102           hypertension          TRUE
## 103               diabetes          TRUE
## 104                                   NA
## 105                 cardio          TRUE
## 106                                   NA
## 107                                   NA
## 108                 stroke         FALSE
## 109                                   NA
## 110                                   NA
## 111                                   NA
## 112                physcap         FALSE
## 113                physcap         FALSE
##     longitudinal             unit include
## 1          FALSE           person    TRUE
## 2             NA                       NA
## 3             NA                       NA
## 4             NA                       NA
## 5             NA                       NA
## 6             NA                       NA
## 7             NA                       NA
## 8             NA                       NA
## 9             NA                       NA
## 10            NA                       NA
## 11            NA                       NA
## 12            NA                       NA
## 13            NA                       NA
## 14         FALSE             year    TRUE
## 15         FALSE             year    TRUE
## 16         FALSE         category    TRUE
## 17         FALSE            years    TRUE
## 18         FALSE         category    TRUE
## 19         FALSE         category    TRUE
## 20         FALSE         category    TRUE
## 21         FALSE            scale    TRUE
## 22         FALSE       drinks/day    TRUE
## 23         FALSE cigarettes / day    TRUE
## 24         FALSE            years    TRUE
## 25         FALSE         category    TRUE
## 26         FALSE         category    TRUE
## 27          TRUE       time point    TRUE
## 28            NA                       NA
## 29            NA                       NA
## 30            NA                       NA
## 31            NA                       NA
## 32            NA                       NA
## 33            NA                       NA
## 34            NA                       NA
## 35            NA                       NA
## 36            NA                       NA
## 37            NA                       NA
## 38            NA                       NA
## 39          TRUE         category    TRUE
## 40          TRUE             0, 1    TRUE
## 41          TRUE         category    TRUE
## 42          TRUE        composite    TRUE
## 43          TRUE        composite    TRUE
## 44          TRUE        composite    TRUE
## 45          TRUE        composite    TRUE
## 46          TRUE        composite    TRUE
## 47          TRUE        composite    TRUE
## 48          TRUE          0 to 15      NA
## 49          TRUE          0 to 75      NA
## 50          TRUE          0 to 12      NA
## 51          TRUE          0 to 25      NA
## 52          TRUE          0 to 12      NA
## 53          TRUE          0 to 14      NA
## 54          TRUE          0 to 12      NA
## 55          TRUE          0 to 12      NA
## 56          TRUE           0 to 8      NA
## 57          TRUE          0 to 15      NA
## 58          TRUE          0 to 30    TRUE
## 59          TRUE          0 to 48      NA
## 60          TRUE          0 to 16      NA
## 61          TRUE          0 to 10      NA
## 62          TRUE          0 to 15      NA
## 63          TRUE         0 to 110      NA
## 64          TRUE          0 to 25      NA
## 65          TRUE          0 to 30      NA
## 66          TRUE          0 to 10      NA
## 67          TRUE          o to 10      NA
## 68            NA                       NA
## 69          TRUE            scale    TRUE
## 70          TRUE            scale    TRUE
## 71          TRUE            scale    TRUE
## 72          TRUE            scale    TRUE
## 73          TRUE            scale    TRUE
## 74          TRUE            scale    TRUE
## 75          TRUE           liters    TRUE
## 76          TRUE           cm H20    TRUE
## 77          TRUE           cm H20    TRUE
## 78          TRUE           liters    TRUE
## 79            NA                       NA
## 80            NA                       NA
## 81            NA                       NA
## 82            NA                       NA
## 83            NA                       NA
## 84            NA                       NA
## 85            NA                       NA
## 86            NA                       NA
## 87            NA                       NA
## 88            NA                       NA
## 89            NA                       NA
## 90            NA                       NA
## 91            NA                       NA
## 92            NA                       NA
## 93         FALSE            grams    TRUE
## 94          TRUE           kg/msq    TRUE
## 95          TRUE           meters    TRUE
## 96            NA                       NA
## 97          TRUE            kilos    TRUE
## 98          TRUE                       NA
## 99          TRUE                       NA
## 100         TRUE                       NA
## 101         TRUE                       NA
## 102         TRUE                       NA
## 103         TRUE                       NA
## 104           NA                       NA
## 105         TRUE         category    TRUE
## 106           NA                       NA
## 107           NA                       NA
## 108         TRUE         category    TRUE
## 109           NA                       NA
## 110           NA                       NA
## 111           NA                       NA
## 112         TRUE          min/sec    TRUE
## 113         TRUE              lbs    TRUE
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_Canada.1252 
## [2] LC_CTYPE=English_Canada.1252   
## [3] LC_MONETARY=English_Canada.1252
## [4] LC_NUMERIC=C                   
## [5] LC_TIME=English_Canada.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices
## [4] utils     datasets  methods  
## [7] base     
## 
## other attached packages:
## [1] ggplot2_1.0.0 magrittr_1.5 
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1     colorspace_1.2-4  
##  [3] dichromat_2.0-0    digest_0.6.4      
##  [5] dplyr_0.2          evaluate_0.5.5    
##  [7] extrafont_0.16     extrafontdb_1.0   
##  [9] formatR_1.0        grid_3.1.1        
## [11] gtable_0.1.2       knitr_1.6         
## [13] MASS_7.3-33        munsell_0.4.2     
## [15] parallel_3.1.1     plyr_1.8.1        
## [17] proto_0.3-10       RColorBrewer_1.0-5
## [19] Rcpp_0.11.2        reshape2_1.4      
## [21] Rttf2pt1_1.3.2     scales_0.2.4      
## [23] stringr_0.6.2      testit_0.3        
## [25] tools_3.1.1
```

```r
Sys.time()
```

```
## [1] "2016-04-13 07:51:12 PDT"
```

