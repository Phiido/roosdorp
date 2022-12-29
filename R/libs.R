
# Packages that are accessible to lib functions.
packages <- list(base = list("tidyverse", "writexl", "remotes", "fs", "rlang",
                         "pbapply", "textclean", "foreach", "doParallel"),
             epi = list("Epi", "MortalityTables"),
             stat = list("infer", "pwr", "naivebayes", "pROC", "rpart",
                         "randomForest", "vtreat", "reclin"),
             mdown = list("rmarkdown", "bookdown", "tinytex", "knitr"),
             bench = list("lobstr", "profvis","benchmarkme", "bench"),
             dev = list("devtools", "roxygen2", "styler",
                        "lintr", "withr", "available", "R6")
)

usethis::use_data(packages, overwrite = TRUE)
