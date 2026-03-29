Sys.setenv(PATH = paste("C:/Program Files/Pandoc", Sys.getenv("PATH"), sep = ";"))
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/Pandoc")
Sys.setenv(`_R_CHECK_FORCE_SUGGESTS_` = "false")
rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"), error_on = "warning")
