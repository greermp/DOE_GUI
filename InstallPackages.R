library(reticulate)

setwd("~/A9/shinyDOE/DOE_GUI")

conda_list(conda = "auto")

# conda_install(envname = 'r-reticulate', packages = c('pyDOE2'))
# options(reticulate.conda_binary = "C:\\Users\\greer\\AppData\\Local\\r-miniconda\\envs\\r-reticulate/python.exe")

source_python('DoeMaker.py')


flights <- ff2n(5)
flights <- data.frame(flights)

DT::datatable(flights)

flights <- data.frame(flights)


flights %>% index=paste0()

rownames(flights) <- paste0("Case",seq(nrow(flights)))
