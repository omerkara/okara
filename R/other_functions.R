#============================== Other Functions ================================
#====================== All Other Functions in Collection ======================

# Notes:
#
## This script contains some functions which are not directly related to data and estimation stages but might be used in either one starting with the data stage.
## Some of these functions have specific purposes and should not be used separately.
## Information for some functions is given in its dedicated space.

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
    choices <- c("yes", "no")
    if (add.lines.before & !GUI) cat("------------------------\n")
    answer <- utils::menu(choices, graphics = GUI, title = question)
    ifelse(answer == 1L, TRUE, FALSE) ## Returns TRUE or FALSE.
}

#================================ DigitsByRows =================================
#' @title Significant Digits By Row
#'
#' @description This function prints significant digits by row in a data frame.
#'
#' @param df A data frame.
#' @param digits The requested digits in order of rows.
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
#' @return A data frame.
#'
#' @examples
#' \dontrun{
#' DigitsByRows(df, digits = c(0, 2)) ## Do not run.
#' }
#'
#' @export
#'
DigitsByRows <- function(df, digits) {
    tmp0 <- data.frame(t(df))
    tmp1 <- mapply(
        function(df0, digits0) {
            base::formatC(df0, format = "f", digits = digits0)
        },
        df0 = tmp0, digits0 = digits
    )
    tmp1 <- data.frame(t(tmp1))
    names(tmp1) <- names(df)
    return(tmp1)
}

#============================== Perm.No.Replace ================================
#====================== Permutation with No Replacement ========================
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
#' @description This set of functions simplify and specialize the \code{\link[exams]{round2}} function. Each function rounds the given number to the decimal points specified in the function itself.
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
#' @seealso
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
