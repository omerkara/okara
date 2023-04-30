#=============================== Data Functions ================================
#======================== Data Functions in Collection =========================

# Notes:
#
## This script contains some functions which are directly related to data stage but also might be use in estimation stage.
## Some of these functions have specific purposes and should not be used separately.

#================================== Head.Tail ==================================
#' @title Head and Tail of a Data Frame
#'
#' @description This function shows the first and last selected items of a data frame, then presents these together.
#'
#' @param x data.frame. Data as in data.frame class.
#' @param Select integer. Number of the first and the last items in the data frame.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A matrix look alike object.
#'
#' @examples
#' \dontrun{
#' Head.Tail(data, 5) ## Do not run.
#' Head.Tail(data, 10) ## Do not run.
#' }
#'
#' @export
#'
Head.Tail <- function(x, Select) {
    if (!requireNamespace("utils")) stop("Required utils package is missing.")
    if (Select %% 1 != 0)
        stop("Invalid Select. Please choose a whole number as Select.\n")
    rbind(utils::head(x, Select), utils::tail(x, Select))
}

#========================= Cubic.Spline.Interpolation ==========================
#' @title Cubic Spline Interpolation
#'
#' @description This function interpolates the selected columns of a data frame with cubic spline interpolation.
#'
#' @param Data data.frame. Data as in data.frame class.
#' @param Column.Names character. Name of the columns to be interpolated.
#'
#' @details The aim of this function is to interpolate the values between two entries. The argument maxgap in \code{\link[zoo]{na.spline}} function is internally selected in this function to interpolate all missing values. na.rm argument in \code{\link[zoo]{na.spline}} function is selected as FALSE since other NA values should be kept, if there is any.
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return An data.frame named as "data.interpolate" exported to the general environment.
#'
#' @examples
#' \dontrun{
#' Cubic.Spline.Interpolation(data, "Open")
#' data.interpolate
#'
#' Cubic.Spline.Interpolation(data, c("Open", "High", "Low", "Close"))
#' data.interpolate
#' }
#'
#' @export
#'
Cubic.Spline.Interpolation <- function(Data, Column.Names) {
    if (!requireNamespace("zoo")) stop("Required zoo package is missing.")
    # Checks Data argument.
    if (!is.data.frame(Data))
        stop("Invalid Data. Please choose a data.frame object for Data.\n")

    # Checks Column.Names argument.
    if (ncol(Data) < length(Column.Names))
        stop("Invalid Column.Names. Please choose a Column.Names object with lower length then number of columns in Data.\n")
    if (sum(!(Column.Names %in% colnames(Data))) != 0)
        stop(paste0("Invalid Column.Names. ", paste0(base::dQuote(Column.Names[!(Column.Names %in% colnames(Data))]), collapse = " and "), " columns does not exists in Data.\n"))

    # Storing the original colnames.
    col.names <- colnames(Data)

    # Interpolating.
    for (j in 1:length(Column.Names)) {
        k <- Column.Names[j]
        if (!(is.na(Data[1, k]))) {
            Data$count[1] <- ifelse(is.na(Data[1, k]) == TRUE, 1, 0)
            for (i in 2:nrow(Data)) {
                Data$count[i] <- ifelse(is.na(Data[i, k]) == TRUE, Data$count[i - 1] + 1, 0)
            }
        }
        if (is.na(Data[1, k])) {
            Data$count <- NA
            Data$count[as.numeric(rownames(Data[(is.na(Data[, k]) == FALSE),])[1])] <- ifelse(is.na(Data[as.numeric(rownames(Data[(is.na(Data[, k]) == FALSE),])[1]), k]) == TRUE, 1, 0)
            for (i in (as.numeric(rownames(Data[(is.na(Data[, k]) == FALSE),])[1]) + 1):nrow(Data)) {
                Data$count[i] <- ifelse(is.na(Data[i, k]) == TRUE, Data$count[i - 1] + 1, 0)
            }
        }
        max <- max(Data$count, na.rm = TRUE)
        Data <- Data[, -(grep("count", names(Data)))]
        variable <- Data[, k]
        if (j == 1) {
            main <- data.frame(zoo::na.spline(variable, maxgap = max, na.rm = FALSE))
            main <- cbind(Data[, colnames(Data)[!(colnames(Data) %in% Column.Names)]], main[, 1])
            colnames(main)[ncol(main)] <- k
        } else {
            temp <- data.frame(zoo::na.spline(variable, maxgap = max, na.rm = FALSE))
            main <- cbind(main, temp[, 1])
            colnames(main)[ncol(main)] <- k
        }
    }
    Data <- main[, col.names]

    # Naming and extracting the Data file to the general environment.
    assign("data.interpolate", Data, envir = globalenv()) ## Output data will be named as "data.interpolate".
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
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
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

#================================== Diff.Col ===================================
#' @title Difference of Subset Columns
#'
#' @description This function takes the difference of specified columns in a ts object. It should not be used for taking differences of all variables. Only a subset of variables can be used. Other variables are bind to the differenced data at the end.
#'
#' @param Data ts object.
#' @param Diff.ColNames character. Name of the columns to be differenced.
#' @param Output character. Name of the output.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A first difference taken ts object with a user selected name.
#'
#' @examples
#' x <- data.frame(V1 = c(1:10), V2 = c(1:10), V3 = c(1:10))
#' x.ts <- ts(x)
#' Diff.Col(Data = x.ts, Diff.ColNames = c("V1", "V3"), Output = "data.diff")
#' data.diff
#'
#' @export
#'
Diff.Col <- function(Data, Diff.ColNames, Output) {
    temp.level <- Data[-1, colnames(Data)[!(colnames(Data) %in% Diff.ColNames)], drop = FALSE]
    temp.diff <- base::diff(Data[ , colnames(Data)[colnames(Data) %in% Diff.ColNames], drop = FALSE], lag = 1, diff = 1)
    main <- cbind(temp.level, temp.diff)
    if ((ncol(Data) - length(Diff.ColNames)) == 1) {
        colnames(main)[1] <- c(paste0(colnames(main)[1], ".", colnames(Data)[!(colnames(Data) %in% Diff.ColNames)]))
    }
    if (length(Diff.ColNames) == 1) {
        colnames(main)[ncol(main)] <- c(paste0(colnames(main)[ncol(main)], ".", Diff.ColNames))
    }
    colnames(main) <- gsub("(temp.level.)|(temp.diff.)", "", colnames(main))
    main <- main[, colnames(Data)]
    assign(Output, main, envir = globalenv()) ## Output data will be named as the selected Output.
}

#========================= Data Conversion Functions ===========================
#' @title Functions for Converting Data
#'
#' @description This set of functions converts given data by the selected column with some specific purposes. The details are below.
#'
#' @param data data.frame. Data as in data.frame class.
#' @param variable character. A single string which is a column name in the data.
#'
#' @details All conversions are in character. See below.
#' \itemize{
#'   \item Dash.Convert.NA: Converts "-" into NA.
#'   \item Dash.Convert.Zero: Converts "-" into "0".
#'   \item Zero.Convert.NA: Converts "0" into NA.
#'   \item Null.Convert.NA: Converts "null" into NA.
#'   \item NA.Convert.Zero: Converts NA into "0".
#'   \item Negative.Convert.NA: Converts negative value into NA.
#'   \item NaN.Convert.Zero: Converts NaN into "0".
#'   \item NaN.Convert.NA: Converts NaN into NA.
#' }
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return A data frame.
#'
#' @examples
#' \dontrun{
#' Dash.Convert.NA(data, "variable.name") ## Do not run.
#' Dash.Convert.Zero(data, "variable.name") ## Do not run.
#' Zero.Convert.NA(data, "variable.name") ## Do not run.
#' Null.Convert.NA(data, "variable.name") ## Do not run.
#' NA.Convert.Zero(data, "variable.name") ## Do not run.
#' Negative.Convert.NA(data, "variable.name") ## Do not run.
#' NaN.Convert.Zero(data, "variable.name") ## Do not run.
#' NaN.Convert.NA(data, "variable.name") ## Do not run.
#' }
#'
#' @name convert.data
NULL
#> NULL
#'
#' @rdname convert.data
#' @export
#'
Dash.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == "-") { ## Converts "-" into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}
#'
#' @rdname convert.data
#' @export
#'
Dash.Convert.Zero <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == "-") { ## Converts "-" into "0".
                temp[, variable][i] <- "0"
            }
        }
    }
    return(temp)
}
#'
#' @rdname convert.data
#' @export
#'
Zero.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == 0) { ## Converts "0" into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}
#'
#' @rdname convert.data
#' @export
#'
Null.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] == "null") { ## Converts "null" into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}
#'
#' @rdname convert.data
#' @export
#'
NA.Convert.Zero <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (is.na(temp[, variable][i])) {
            temp[, variable][i] <- "0" ## Converts NA into "0".
        }
    }
    return(temp)
}
#'
#' @rdname convert.data
#' @export
#'
Negative.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (!is.na(temp[, variable][i])) {
            if (temp[, variable][i] < 0) { ## Converts negative value into NA.
                temp[, variable][i] <- NA
            }
        }
    }
    return(temp)
}
#'
#' @rdname convert.data
#' @export
#'
NaN.Convert.Zero <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (is.nan(temp[, variable][i])) {
            temp[, variable][i] <- "0" ## Converts NaN into "0".
        }
    }
    return(temp)
}
#'
#' @rdname convert.data
#' @export
#'
NaN.Convert.NA <- function(data, variable) {
    temp <- data
    for (i in 1:nrow(temp)) {
        if (is.nan(temp[, variable][i])) {
            temp[, variable][i] <- NA ## Converts NaN into NA.
        }
    }
    return(temp)
}

#================================== dataStr ====================================
#' @title Structure of Datasets
#'
#' @description This function lists the data sets (with their structure) in specified R packages.
#'
#' @param package character. Package name. Default: NULL
#' @param df logical. Give information only about all data.frame objects? DEFAULT: FALSE
#' @param ... Arguments passed from other functions.
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @references This function is taken from \href{https://github.com/brry/berryFunctions/blob/master/R/dataStr.R}{here}.
#'
#' @seealso \code{\link{str}}
#'
#' @return Invisible data.frame. Mainly prints via \code{\link{message}} in a for loop.
#'
#' @examples
#' \dontrun{
#' dataStr() ## All loaded packages on search path (package = NULL).
#' dataStr("datasets") ## Only datasets in base R.
#' dataStr("colorspace") ## Works with an installed but unloaded package.
#' }
#'
#' @export
#'
dataStr <- function(package = NULL, df = FALSE, ...) {
    if (!requireNamespace("utils")) stop("Required utils package is missing.")
    env <- new.env()
    d <- utils::data(..., package = package, envir = env)$results
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    # change things like  "beaver1 (beavers)"  to  "beaver1"
    itemsplit <- base::strsplit(d$Item, split = " ", fixed = TRUE)
    d$Object <- sapply(itemsplit, "[", 1)
    d$Call <- sapply(itemsplit, "[", 2)
    d$Call <- gsub("(","",gsub(")","",d$Call, fixed = TRUE), fixed = TRUE)
    d$Call[is.na(d$Call)] <- d$Object[is.na(d$Call)]
    # sort alphabetically within packages:
    d <- d[order(d$Package, tolower(d$Object)), ]
    d$class <- NA
    if(df) {
        d$nrow <- NA
        d$ncol <- NA
    }
    for(i in 1:nrow(d)) {
        x <- d[i,, drop = FALSE]
        utils::data(list = x$Call, package = x$Package, envir = env)
        obj <- get(x$Object, envir = env) # getExportedValue(asNamespace(package), x$Object)
        d[i,"class"] <- toString(class(obj))
        if(!df)
        {
            message(x$Package, "  |  ", x$Object, "  |  ", d[i,"class"], "  |  ", x$Title)
            message(utils::str(obj))
        } else if(grepl("data.frame", d[i,"class"]))
            d[i, c("nrow","ncol")] <- c(nrow(obj),ncol(obj))
    }
    if(df) {
        d <- d[grepl("data.frame", d$class), ]
    }
    return(invisible(d))
}

#==================================== END ======================================
