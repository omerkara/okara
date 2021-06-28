#================================ Load.Install =================================
#' @title Load or Install/Load Packages
#'
#' @description This function either loads or installs/loads the specified packages.
#'
#' @param Package.Names A vector of strings. The package names to be installed/loaded.
#' @param Quiet logical. If TRUE, suppress the output.
#' @param Update.All logical. If TRUE, updates the specified packages.
#'
#' @details Packages from GitHub can also be installed/loaded.
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return Install and/or loads the specified packages.
#'
#' @examples
#' \dontrun{
#' Load.Install(Package.Names = "plyr")
#' Load.Install(Package.Names = c("plyr", "dplyr"))
#' Load.Install(c("plyr", "dplyr", "tidyr", "stringr", "stringi", "Hmisc"))
#' Load.Install(c("plyr", "dplyr"), Quiet = TRUE, Update.All = TRUE)
#'
#' Load.Install("FinYang/tsdl")
#' }
#'
#' @export
#'
Load.Install <- function(Package.Names, Quiet = FALSE, Update.All = FALSE) {
    if (!requireNamespace("devtools")) stop("Required devtools package is missing.")
    if (!requireNamespace("utils")) stop("Required utils package is missing.")
    is_installed <- function(my.pkgs) is.element(my.pkgs, utils::installed.packages()[ ,1])
    github.pkgs <- grep("^.*?/.*?$", Package.Names, value = TRUE)
    github.bare.pkgs <- sub(".*?/", "", github.pkgs)
    cran.pkgs <- Package.Names[!(Package.Names %in% github.pkgs)]
    all.pkgs <- c(cran.pkgs, github.bare.pkgs)
    cran.missing <- cran.pkgs[which(!is_installed(cran.pkgs))]
    github.missing <- github.pkgs[which(!is_installed(github.bare.pkgs))]
    if (Update.All == TRUE) {
        cran.missing <- cran.pkgs
        github.missing <- github.pkgs
    } else {
        cran.missing <- cran.pkgs[which(!is_installed(cran.pkgs))]
        github.missing <- github.pkgs[which(!is_installed(github.bare.pkgs))]
    }
    if (length(cran.missing) > 0) {
        suppressWarnings(utils::install.packages(cran.missing, quiet = Quiet, dependencies = TRUE))
    }
    if (length(github.missing) > 0) {
        suppressWarnings(devtools::install_github(github.missing, quiet = Quiet, dependencies = TRUE))
    }
    failed.install <- all.pkgs[which(!is_installed(all.pkgs))]
    if (length(failed.install) > 0) {
        warning(paste0("Some packages failed to install: ", paste(failed.install, collapse = ", "), "."))
    }
    install.pkgs <- all.pkgs[which(is_installed(all.pkgs) == TRUE)]
    for (install.pkgs in install.pkgs) {
        suppressPackageStartupMessages(library(install.pkgs, character.only = TRUE, quietly = Quiet, verbose = FALSE))
    }
}

#==================================== END ======================================
