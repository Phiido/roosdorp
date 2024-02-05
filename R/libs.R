
# Packages that are accessible to lib functions.
packages <- list(

  # General-purpose packages and those improving quality of life in R programming
  base = list(
    'tidyverse',  # A collection of R packages designed for data science
    'remotes',    # Install R packages from remote repositories
    'fs',         # Provides a cross-platform interface for file system operations
    'pbapply',    # Adds progress bar functionality
    'textclean',  # Tools for text cleaning and processing
    'foreach',    # Provides a looping construct for parallel execution of code
    'doParallel', # Foreach parallel adaptor for the 'parallel' package
    'reticulate', # Interface to Python modules, classes, and functions
    'labelled',   # Manipulating labelled data
    'gt'          # Create display tables from data frames
  ),

  # Packages focused on data manipulation and interaction
  data = list(
    'DBI',        # Database interface definition for communication between R and SQL databases
    'XLConnect',  # Excel connector, providing comprehensive functionality to read, write and manipulate Excel files
    'writexl',    # Portable, easy and fast way to write data frames to Excel .xlsx files
    'foreign',    # Read and write data stored by statistical software like Minitab, S, SAS, SPSS
    'haven',      # Import and export 'SPSS', 'Stata' and 'SAS' files
    'sweidnumbr', # Handle Swedish identity numbers
    'httr'        # User-friendly HTTP requests
  ),

  # Visualization focused packages
  viz = list(
    'ggpubr',     # 'ggplot2' based publication ready plots
    'GGally',     # Extension to 'ggplot2'
    'WVPlots',    # Commonly used data visualizations
    'plotly'      # Create interactive web graphics
  ),

  # Epidemiology related packages
  epi = list(
    'Epi',             # Functions for epidemiological analysis
    'MortalityTables', # Work with mortality tables
    'survival',        # Survival analysis
    'ggsurvfit',       # Visualization for survival analysis
    'dagitty',         # Graphical analysis of causal models
    'rstpm2'           # Smooth Survival Models
  ),

  # Statistical analysis and testing packages
  stat = list(
    'infer',        # Tidy statistical inference
    'pwr',          # Power analysis functions
    'WebPower',     # Basic and advanced statistical power analysis
    'naivebayes',   # Implementation of Naive Bayes classifier
    'pROC',         # Tools to visualize, smooth and compare ROC curves
    'reclin',       # Record linkage toolkit
    'sigr',         # Concise formatting of significance testing results
    'xgboost',      # Extreme Gradient Boosting
    'random',       # True random numbers using RANDOM.ORG
    'rsimsum',      # Summarize simulation studies
    'metafor',      # Meta-analysis package
    'spind',        # Spatial methods and data
    'rstatix',      # Pipe-friendly framework for basic statistical tests
    'car',          # Companion to Applied Regression
    'lme4',         # Linear mixed-effects models
    'lmerTest',     # Tests in linear mixed effects models
    'emmeans',      # Estimated marginal means, aka least-squares means
    'multcomp',     # Simultaneous inference for general linear hypotheses
    'geepack',      # Generalized estimating equations
    'dlnm',         # Functions for distributed lag linear and non-linear models
    'rms'           # Collection of functions that assist with and streamline modeling
  ),

  # Modeling and machine learning packages
  model = list(
    'tidymodels',    # A collection of packages for modeling and machine learning using tidyverse principles
    'mgcv',          # Mixed GAM computation vehicle with GCV/AIC/REML smoothness estimation
    'randomForest',  # Breiman and Cutler's random forests for classification and regression
    'vcd',           # Visualizing categorical data
    'rpart',         # Recursive partitioning for classification, regression and survival trees
    'ranger',        # Fast implementation of random forests
    'vtreat',        # Variable treatment for automated data preparation
    'gbm',           # Generalized boosted regression models
    'caret'          # Classification and regression training
  ),

  # Packages for reporting and documentation
  report = list(
    'rmarkdown',    # Dynamic documents for R
    'bookdown',     # Authoring books and technical documents with R markdown
    'tinytex',      # Helper functions to install and maintain 'TeX Live', and compile LaTeX documents
    'knitr'         # A general-purpose tool for dynamic report generation in R
  ),

  # Shiny web application related packages
  shiny = list(
    'shiny',         # Web application framework for R
    'DT',            # Interface to the 'DataTables' JavaScript library
    'leaflet',       # Create interactive web maps with the JavaScript 'Leaflet' library
    'plotly',        # Create interactive web graphics via 'plotly.js'
    'shinythemes',   # Themes for Shiny
    'shinydashboard',# Create dashboards with 'Shiny'
    'shinyBS',       # Twitter Bootstrap components for Shiny
    'shinyWidgets',  # Custom inputs widgets for Shiny
    'formattable',   # Create formattable data structures
    'shinyjs',       # Easily improve the user experience of your Shiny app
    'shinytest',     # Test Shiny apps
    'shinyloadtest'  # Load testing for Shiny applications
  ),

  # Performance analysis and benchmarking packages
  bench = list(
    'lobstr',       # Understanding and controlling object sizes
    'profvis',      # Interactive visualizations for profiling R code
    'benchmarkme',  # Crowd-sourced benchmarking
    'bench'         # High precision timing of R expressions
  ),

  # Development tools and utilities
  dev = list(
    'devtools',    # Tools to make an R developer's life easier
    'roxygen2',    # In-line documentation for R
    'styler',      # Non-invasive pretty printing of R code
    'lintr',       # Static code analysis for R
    'withr',       # Run code 'with' temporarily modified global state
    'available',   # Check if the name of a package is available, valid or already in use
    'R6'           # Encapsulated classes with reference semantics
  )
)

usethis::use_data(packages, internal = TRUE, overwrite = TRUE)
