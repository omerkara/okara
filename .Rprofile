#=============================== okara Package =================================
#================================== .Rprofile ==================================
#======================= Initial Settings for The Project ======================

#=============================== First Function ================================
.First <- function() {
    cat("\n********************************************************\n")
    cat("\nWelcome to okara Package!\nRemember to edit .Rprofile accordingly!\n")
    cat("\nAll comments, suggestions, and other correspondences should be sent to Omer Kara, okara.kara.ylsy@gmail.com.\n")
    cat("\n********************************************************\n")
    options(digits.secs = 3) ## show sub-second time stamps
    options(showWarnCalls = TRUE, showErrorCalls = TRUE)
    options(repos = c(CRAN = "https://cran.rstudio.com/"), browserNLdisabled = TRUE, deparse.max.lines = 2)
    options(digits = 8, show.signif.stars = TRUE, scipen = 6)
    options(replace.assign = TRUE, width = 90)
    # options("pdfviewer" = "skim")
}

#================================ Last Function ================================
.Last <- function()  cat("\nGoodbye!\n")

#================================ R Libraries =================================
.libPaths("/Volumes/Omer/Google Drive/Apps/R/R Libraries")

#================================== Settings ===================================
if (interactive()) {
    suppressMessages(require(devtools))
    suppressMessages(require(usethis))
    suppressMessages(require(roxygen2))
    suppressMessages(require(testthat))
    suppressMessages(require(knitr))
    suppressMessages(require(badger))

    suppressMessages(require(sp))
    suppressMessages(require(fields))
}

#==================================== END ======================================
