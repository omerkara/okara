#============================== Other Functions ================================
#====================== All Other Functions in Collection ======================

# Notes:
#
## This script contains some functions which are not directly related to data and estimation stages but might be used in either one starting with the data stage.

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

#=============================== Proceed.or.Stop ===============================
#' @title Tells to Proceed or Stop
#'
#' @description This function produces warning message if there is anything wrong with your code.
#'
#' @param result A result with a result of TRUE or FALSE.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A text message.
#'
#' @examples
#' Proceed.or.Stop(5 < 6)
#' Proceed.or.Stop(5 < 3)
#' Proceed.or.Stop(5 == 5)
#'
#' @export
#'
Proceed.or.Stop <- function(result) {
    if (result) {
        message("Yeeaah: Everything is fine. You can proceed safely.\n")
    }
    else if (!result) {
        warning("Oh NO!!!: Something is wrong. Be cautious. Read the comments belong to the code first, then debug the code if necessary.\n")
    }
}

#============================ Ask.User.YN.Question =============================
#' @title Interactive Yes/No Questions
#'
#' @description This function creates an interactive yes/no question.
#'
#' @param question string. Your question as a character string.
#' @param GUI logical. TRUE for using GUI or FALSE for using console.
#' @param add.lines.before logical. Simply add some line after your questions.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return logical.
#'
#' @examples
#' \dontrun{
#' if (Ask.User.YN.Question("Your question goes here?", GUI = FALSE, add.lines.before = TRUE)) {
#'     Some Code Here
#' } else {
#'     More Code Here
#' }
#' }
#'
#' @export
#'
Ask.User.YN.Question <- function(question, GUI = TRUE, add.lines.before = TRUE) {
    if (!requireNamespace("utils")) stop("Required utils package is missing.")
    choices <- c("yes", "no")
    if (add.lines.before & !GUI) cat("------------------------\n")
    answer <- utils::menu(choices, graphics = GUI, title = question)
    ifelse(answer == 1L, TRUE, FALSE) ## Returns TRUE or FALSE.
}

#=============================== Impute.NA.Mean ================================
#' @title Impute Missing Values with Mean
#'
#' @description This function fills NA values with mean value.
#'
#' @param x numeric. a numeric vector.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A numeric vector.
#'
#' @examples
#' Impute.NA.Mean(c(1:5, rep(NA, 1)))
#'
#' @export
#'
Impute.NA.Mean <- function(x) {
    replace(x, is.na(x), mean(x, na.rm = TRUE))
}

#============================== Decimal.Num.Count ==============================
#' @title Number of Decimal Places
#'
#' @description This function gives the number of decimal places of a number written as character string or numeric object.
#'
#' @param x Number in character or in numeric class.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return An integer.
#'
#' @examples
#' Decimal.Num.Count(0.1)
#' Decimal.Num.Count("0.1")
#'
#' Decimal.Num.Count(0.12345)
#' Decimal.Num.Count("0.12345")
#'
#' @export
#'
Decimal.Num.Count <- function(x) {
    if (class(x) == "numeric") {
        x <- as.character(x)
    }
    stopifnot(class(x) == "character")
    x <- gsub("(.*)(\\.)|([0]*$)", "", x)
    nchar(x)
}

#================================= logPercent ==================================
#' @title Percentage Change for Log Variables
#'
#' @description This function makes sure that the level variable of interest increases/decreases, 10% i.e., while you are dealing with the log form.
#'
#' @param Variable numeric. A numeric value or vector in logarithmic form.
#' @param Percent numeric. A numeric value representing percentage increase in the level form.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A value in the logarithmic form which makes sure that the level form value has increased by the specified percentage.
#'
#' @examples
#' x <- 100
#' percent <- 10
#' log(x)
#' logPercent(log(x), percent)
#' exp(logPercent(log(x), percent))
#'
#' @export
#'
logPercent <- function(Variable, Percent) {
    Variable.New <- Variable + log(1 + Percent/100)
    return(Variable.New)
}

#=================================== lagPad ====================================
#' @title Lag Function with Padding
#'
#' @description This function pads the lag taken numeric vector with NA values.
#'
#' @param x numeric. A numeric vector.
#' @param k numeric. A numeric lag value.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A lag taken numeric vector with padding.
#'
#' @examples
#' x <- 1:10
#' x.t1 <- lagPad(x, 2)
#' cbind(x, x.t1)
#'
#' @export
#'
lagPad <- function(x, k) {
    if (!is.vector(x))
        stop("x must be a vector")
    if (!is.numeric(x))
        stop("x must be numeric")
    if (!is.numeric(k))
        stop("k must be numeric")
    if (1 != length(k))
        stop("k must be a single number")
    c(rep(NA, k), x)[1:length(x)]
}

#=================================== diffPad ====================================
#' @title Difference Function with Padding
#'
#' @description This function takes the first difference of a given time series data or vector and pads it with the selected lag length while maintaining the attributes of the input data. ts object or vector can be used.
#'
#' @param x ts object or numeric vector.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A first difference taken ts object or numeric vector with padding.
#'
#' @examples
#' values <- c(1:10)
#' x <- values
#' x.ts <- ts(data.frame(V1 = values))
#' diffPad(x)
#' diffPad(x.ts)
#'
#' @export
#'
diffPad <- function(x) {
    if (!requireNamespace("stats")) stop("Required stats package is missing.")
    if (is.vector(x)) {
        output <- c(rep(NA, 1), base::diff(x, lag = 1, diff = 1))
    }
    if (stats::is.ts(x)) {
        output <- rbind(rep(NA, 1), base::diff(x, lag = 1, diff = 1))
    }
    attributes(output) <- attributes(x)
    return(output)
}

#================================== sampled ====================================
#' @title Bug free Redefinition of Sample Function
#'
#' @description If only one value is given in \code{\link[base]{sample}} function, it chooses a value between 1 and the number you specified. The redefined sample function solves this problem.
#'
#' @param x either a vector of one or more elements from which to choose, or a positive integer.
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements of the vector being sampled.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references For more information see \code{\link[base]{sample}}.
#'
#' @seealso
#'
#' @return For sample a vector of length size with elements drawn from either x or from the integers 1:x.
#'
#' @examples
#' sampled(2, 1)
#' sampled(1:5, 2)
#'
#' @export
#'
sampled <- function(x, size, replace = FALSE, prob = NULL) {
    if (length(x) == 1) {
        return(x)
    } else {
        sample(x, size = size, replace = replace, prob = prob)
    }
}

#============================== Perm.No.Replace ================================
#' @title Permutation without Replacement
#'
#' @description This function performs permutation of a given vector without replacement.
#'
#' @param v A numeric or character vector.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references For more information see \href{https://www.r-bloggers.com/2019/06/learning-r-permutations-and-combinations-with-base-r/}{here}.
#'
#' @seealso
#'
#' @return A numeric or character vector.
#'
#' @examples
#' Perm.No.Replace(v = c(1:3))
#'
#' @export
#'
Perm.No.Replace <- function(v) {
    n <- length(v)
    if (n == 1) {
        v
    }
    else {
        X <- NULL
        for (i in 1:n) {
            X <- rbind(X, cbind(v[i], Perm.No.Replace(v[-i])))
        }
        X
    }
}

#================================ SignPrint ====================================
#' @title Prints The Sign of a Value
#'
#' @description This function shows the sign of a given single numeric value.
#'
#' @param x numeric. A single numeric value.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return Character string either "+" or "-".
#'
#' @examples
#' SignPrint(2)
#' SignPrint(-2)
#'
#' @export
#'
SignPrint <- function(x) {
    ifelse(x > 0, "+", "-")
}

#================= round0, round1, round2, round3, and round4 ==================
#' @title Functions for Rounding with \code{\link[exams]{round2}}
#'
#' @description This set of functions simplify and specialize the \code{\link[exams]{round2}} function in exam package. Each function rounds the given number to the decimal points specified in the function itself.
#'
#' @param x numeric. A numeric vector.
#'
#' @details The function \code{\link[exams]{round2}} does what is known in German as kaufmaennisches Runden (rounding away from zero for trailing 5s).
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references For more information see \code{\link[exams]{round2}}.
#'
#' @seealso \code{\link[exams]{round2}}
#'
#' @return Numeric value or vector.
#'
#' @examples
#' round0(2.67896)
#' round1(2.67896)
#' round2(2.67896)
#' round3(2.67896)
#' round4(2.67896)
#'
#' @name roundN
NULL
#> NULL
#'
#' @rdname roundN
#' @export
#'
round0 <- function(x) {
    round(x + base::sign(x) * 1e-10, digits = 0)
}
#'
#' @rdname roundN
#' @export
#'
round1 <- function(x) {
    round(x + base::sign(x) * 1e-10, digits = 1)
}
#'
#' @rdname roundN
#' @export
#'
round2 <- function(x) {
    round(x + base::sign(x) * 1e-10, digits = 2)
}
#'
#' @rdname roundN
#' @export
#'
round3 <- function(x) {
    round(x + base::sign(x) * 1e-10, digits = 3)
}
#'
#' @rdname roundN
#' @export
#'
round4 <- function(x) {
    round(x + base::sign(x) * 1e-10, digits = 4)
}

#================= trunc0, trunc1, trunc2, trunc3, and trunc4 ==================
#' @title Functions for Truncating without Round
#'
#' @description This set of functions truncates the numbers without rounding. Each function truncates the given number to the decimal points specified in the function itself.
#'
#' @param x numeric. A numeric vector.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return Numeric value or vector.
#'
#' @examples
#' trunc0(2.67896)
#' trunc1(2.67896)
#' trunc2(2.67896)
#' trunc3(2.67896)
#' trunc4(2.67896)
#'
#' @name truncN
NULL
#> NULL
#'
#' @rdname truncN
#' @export
#'
trunc0 <- function(x) {
    trunc(x * 1)/1
}
#'
#' @rdname truncN
#' @export
#'
trunc1 <- function(x) {
    trunc(x * 10)/10
}
#'
#' @rdname truncN
#' @export
#'
trunc2 <- function(x) {
    trunc(x * 100)/100
}
#'
#' @rdname truncN
#' @export
#'
trunc3 <- function(x) {
    trunc(x * 1000)/1000
}
#'
#' @rdname truncN
#' @export
#'
trunc4 <- function(x) {
    trunc(x * 10000)/10000
}

#==================================== END ======================================
