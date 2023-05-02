#============================ Estimation Functions =============================
#===================== Estimation Functions in Collection ======================

# Notes:
#
## This script contains some functions which are directly related to estimation stage.
## Some of these functions have specific purposes and should not be used separately.

#================================= SummaryR.lm =================================
#' @title Summary of OLS with Robust Std Errors
#'
#' @description This function gives summary for OLS with different type of robust std errors.
#'
#' @param model Name of the model estimated with \code{\link[stats]{lm}} function.
#' @param type Type of the robust std error calculation. See \code{\link[car]{hccm}} function in car package.
#' @param ... Arguments passed from other functions.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @import car stats
#'
#' @references
#'
#' @seealso
#'
#' @return A linear model summary.
#'
#' @examples
#' \dontrun{
#' SummaryR.lm(my.model, type = "hc4") ## Do not run.
#' SummaryR.lm(your.model, type = "hc1") ## Do not run.
#' }
#'
#' @export
#'
SummaryR.lm <- function(model, type = c("hc3", "hc0", "hc1", "hc2", "hc4"), ...) {
    if (!requireNamespace("stats")) stop("Required stats package is missing.")
    if (!requireNamespace("car")) stop("Required car package is missing.")
    type <- match.arg(type)
    V <- car::hccm(model, type = type)
    sumry <- base::summary(model)
    table <- stats::coef(sumry)
    table[, 2] <- sqrt(diag(V))
    table[, 3] <- table[,1]/table[, 2]
    table[, 4] <- 2 * stats::pt(abs(table[, 3]), stats::df.residual(model), lower.tail = FALSE)
    sumry$coefficients <- table
    p <- nrow(table)
    hyp <- cbind(0, diag(p - 1))
    sumry$fstatistic[1] <- car::linearHypothesis(model, hyp, white.adjust = type)[2,"F"]
    print(sumry)
    cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
}

#=================================== hoCoef ====================================
#' @title t-Tests for Class lm Objects
#'
#' @description This function performs t-Tests for class lm objects .
#'
#' @param object Model.
#' @param term Order of the parameter in the model.
#' @param bo Alternative hypothesis value.
#' @param alt Alternative hypothesis type.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @import car stats
#'
#' @references The hoCoef function is taken directly from the FSA package (version 0.8.32). For more information see \href{https://cran.r-project.org/web/packages/FSA/index.html}{here}.
#'
#' @seealso
#'
#' @return A t-test result table.
#'
#' @examples
#' \dontrun{
#' hoCoef(your.model, term = 2, bo = 0, alt = "two.sided") ## Do not run.
#' hoCoef(your.model, term = 2, bo = -1, alt = "less") ## Do not run.
#' hoCoef(your.model, term = 4, bo = 3, alt = "greater") ## Do not run.
#' }
#'
#' @export
#'
hoCoef <- function(object, term = 2, bo = 0, alt = c("two.sided", "less", "greater")) {
    if (!requireNamespace("stats")) stop("Required stats package is missing.")
    alt <- match.arg(alt)
    if (!"lm" %in% class(object))
        stop("'object' must be from 'lm'.")
    if (!term > 0)
        stop("'term' must be a positive number.")
    tmp <- base::summary(object)$coefficients
    if (term > length(rownames(tmp)))
        stop("'term' is greater than number of terms in the model.")
    est <- tmp[term,"Estimate"]
    se <- tmp[term,"Std. Error"]
    t <- (est - bo)/se
    df <- object$df.residual
    switch(alt,
           less = {p.value <- stats::pt(t, df, lower.tail = TRUE)},
           greater = {p.value <- stats::pt(t,df,lower.tail=FALSE)},
           two.sided = {p.value <- 2*stats::pt(abs(t), df, lower.tail = FALSE)}
    )
    res <- cbind(term,bo,est,se,t,df,p.value)
    colnames(res) <- c("term", "Ho Value", "Estimate", "Std. Error", "T", "df", "p value")
    rownames(res) <- ""
    res
}

#==================================== END ======================================
