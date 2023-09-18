
# Packages that are accessible to lib functions.
packages <- list(

  base  = list("tidyverse", "remotes", "fs", "pbapply", "textclean",
               "foreach", "doParallel", "reticulate"),
  data  = list("DBI", "XLConnect", "writexl", "foreign", "haven",
               "sweidnumbr", "httr"),
  epi   = list("Epi", "MortalityTables", "Survival", "ggsurvfit", "dagitty"),
  stat  = list("infer", "pwr", "WebPower","naivebayes", "pROC", "reclin",
               "sigr", "WVPlots", "xgboost", "random", "rsimsum"),
  model = list("tidymodels", "mgcv", "randomForest", "vcd", "rpart",
               "ranger", "vtreat", "survival", "gbm", "caret"),
  report= list("rmarkdown", "bookdown", "tinytex", "knitr", "shiny"),
  shiny = list("DT", "leaflet", "plotly", "shinythemes", "shinydashboard",
               "shinyBS", "shinyWidgets", "formattable", "shinyjs"),
  bench = list("lobstr", "profvis","benchmarkme", "bench"),
  dev   = list("devtools", "roxygen2", "styler", "lintr",
               "withr", "available", "R6", "shinytest", "shinyloadtest")
)

usethis::use_data(packages, internal = TRUE, overwrite = TRUE)
