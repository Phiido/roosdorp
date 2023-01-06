
# Packages that are accessible to lib functions.
packages <- list(

  base  = list("tidyverse", "remotes", "fs", "pbapply", "textclean",
               "foreach", "doParallel"),
  data  = list("DBI", "XLConnect", "writexl", "foreign", "haven",
               "sweidnumbr", "httr"),
  epi   = list("Epi", "MortalityTables"),
  stat  = list("infer", "pwr", "naivebayes", "pROC", "reclin", "sigr",
               "WVPlots", "xgboost"),
  model = list("tidymodels", "mgcv", "randomForest", "vcd", "rpart",
               "ranger", "vtreat", "survival"),
  report= list("rmarkdown", "bookdown", "tinytex", "knitr", "shiny"),
  bench = list("lobstr", "profvis","benchmarkme", "bench"),
  dev   = list("devtools", "roxygen2", "styler", "lintr",
               "withr", "available", "R6")
)

usethis::use_data(packages, internal = TRUE, overwrite = TRUE)
