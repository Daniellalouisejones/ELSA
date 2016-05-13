
# All data land on Ellis Island
knitr::stitch_rmd(
  script="./manipulation/map/0-ellis-island-map.R", 
  output="./manipulation/map/stitched-output/0-ellis-island-map.md"
)

# Initial reivew of variables
rmarkdown::render(
  input = "./reports/review-variables/map/review-variables-map.Rmd" ,
  output_format="html_document", 
  clean=TRUE
)


knitr::stitch_rmd(
  script="./manipulation/map/1-ms-measure-map.R", 
  output="./manipulation/map/stitched-output/1-ms-measure-map.md"
)
