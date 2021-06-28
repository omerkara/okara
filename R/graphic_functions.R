#============================= Graphic Functions ===============================
#====================== Graphic Functions in Collection ========================

# Notes:
#
## This script contains some functions about graphics which are directly related to estimation stage.
## Some of these functions have specific purposes and should not be used separately.
## Information for some functions is given in its dedicated space.

#=================================== g_legend ==================================
#' @title Extracting Legend of a ggplot2 Object
#'
#' @description This function extracts legend from ggplot2 plots.
#'
#' @param a.gplot A ggplot2 object.
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
#' @return A ggplot2 legend.
#'
#' @examples
#' \dontrun{
#' g <- ggplot(...) ## Do not run.
#' g_legend() ## Do not run.
#' }
#'
#' @export
#'
g_legend <- function(a.gplot) {
    if (!requireNamespace("ggplot2")) stop("Required ggplot2 package is missing.")
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

#============================== annotation_compass =============================
#' @title Adding Text to a ggplot2 Object
#'
#' @description This function adds text to the specified places (using compass style) of a ggplot2 object.
#'
#' @param label character. Label of the legend
#' @param position Position of the legend. Options are "N", "NE", "E", "SE", "S", "SW", "W", "NW".
#' @param padding A numeric vector or unit object specifying x-values and y-values. Default is grid::unit(c(0.5, 0.5), "line")
#' @param ... Other arguments
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references For more information see \href{https://stackoverflow.com/questions/47916307/specify-position-of-geom-text-by-keywords-like-top-bottom-left-right}{here}.
#'
#' @seealso
#'
#' @return A ggplot2 object with legend.
#'
#' @examples
#' \dontrun{
#' g + annotation_compass(label = "sometext", position = "NE") ## Do not run.
#'
#' ## To do the same, you can always use:
#' g + geom_text(data = data.frame(), aes(label = "sometext",
#'               x = -Inf, y = Inf), hjust = 0, vjust = 1) ## Do not run.
#' g + ggplot2::annotate(geom = "text", label = "sometext",
#'                       x = Inf, y = Inf, hjust = 1,
#'                       vjust = 1, size = 24) ## Do not run.
#' }
#'
#' @export
#'
annotation_compass <- function(label, position = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), padding = grid::unit(c(0.5, 0.5), "line"), ...) {
    if (!requireNamespace("ggplot2")) stop("Required ggplot2 package is missing.")
    if (!requireNamespace("grid")) stop("Required grid package is missing.")
    position <- match.arg(position)
    x <- switch(position,
                N = 0.5,
                NE = 1,
                E = 1,
                SE = 1,
                S = 0.5,
                SW = 0,
                W = 0,
                NW = 0
    )
    y <- switch(position,
                N = 1,
                NE = 1,
                E = 0.5,
                SE = 0,
                S = 0,
                SW = 0,
                W = 0.5,
                NW = 1
    )
    hjust <- switch(position,
                    N = 0.5,
                    NE = 1,
                    E = 1,
                    SE = 1,
                    S = 0.5,
                    SW = 0,
                    W = 0,
                    NW = 0
    )
    vjust <- switch(position,
                    N = 1,
                    NE = 1,
                    E = 0.5,
                    SE = 0,
                    S = 0,
                    SW = 0,
                    W = 0.5,
                    NW = 1
    )
    f1 <- switch(position,
                 N = 0,
                 NE = -1,
                 E = -1,
                 SE = -1,
                 S = 0,
                 SW = 1,
                 W = 1,
                 NW = 1
    )
    f2 <- switch(position,
                 N = -1,
                 NE = -1,
                 E = 0,
                 SE = 1,
                 S = 1,
                 SW = 1,
                 W = 0,
                 NW = -1
    )
    ggplot2::annotation_custom(grid::textGrob(label,
                                              x = grid::unit(x,"npc") + f1 * padding[1],
                                              y = grid::unit(y,"npc") + f2 * padding[2],
                                              hjust = hjust, vjust = vjust, ...))
}

#=============================== Human Numbers =================================
#' @title Human Numbers For ggplot2 Graph Axis
#'
#' @description This set of functions formats the numbers in a ggplot2 graph axis so they are easily readable for humans. Use this function in a ggplot2 object for labels where you might use the comma or percent functions from the Scales package. Function checks whether numbers are positive or negative. It allows up to 1 significant figure and sapply used for element-wise application of the humanity function as a vector may include numbers where billions, millions or thousands are appropriate.
#'
#' @param x numeric. a numeric vector to format.
#' @param smbl character. a symbol you'd like to prefix your numbers by e.g. "$".
#' @param signif numeric. the number of significant places you want the function to return.
#'
#' @details All conversions are in character. See below.
#' \itemize{
#'   \item human_numbers: Main function with adjustable symbol and significant places.
#'   \item human_num: For no symbol.
#'   \item human_per: For percentage symbol.
#'   \item human_gbp: For Pound symbol.
#'   \item human_usd: For Dollar symbol.
#'   \item human_euro: For Euro symbol.
#'   \item human_tl: For TL symbol.
#' }
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references These functions are taken from \href{https://github.com/fdryan/R/blob/master/ggplot2_formatter.r}{here}.
#'
#' @seealso
#'
#' @return A character vector the same length as the input vector
#'
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' \dontrun{
#' ggplot2 + scale_y_continuous(labels = human_num)
#' ggplot2 + scale_x_continuous(labels = human_gbp)
#' ggplot2 + scale_x_continuous(labels = human_tl)
#' }
#'
#' @name human.numbers
NULL
#> NULL
#'
#' @rdname human.numbers
#' @export
#'
human_numbers <- function(x = NULL, smbl = "", signif = 1) {
    humanity <- function(y) {
        if (!is.na(y)) {
            tn <- round(abs(y) / 1e12, signif)
            b <- round(abs(y) / 1e9, signif)
            m <- round(abs(y) / 1e6, signif)
            k <- round(abs(y) / 1e3, signif)

            if (y >= 0) {
                y_is_positive <- ""
            } else {
                y_is_positive <- "-"
            }
            if (k < 1) {
                paste0(y_is_positive, smbl, round(abs(y), signif))
            } else if (m < 1) {
                paste0(y_is_positive, smbl, k , "k")
            } else if (b < 1) {
                paste0(y_is_positive, smbl, m ,"m")
            } else if (tn < 1) {
                paste0(y_is_positive, smbl, b ,"bn")
            } else {
                paste0(y_is_positive, smbl, tn, "tn")
            }
        } else if (is.na(y) | is.null(y)) {
            "-"
        }
    }
    sapply(x, humanity)
}
#'
#' @rdname human.numbers
#' @export
#'
human_num <- function(x) {human_numbers(x, smbl = "")}
#'
#' @rdname human.numbers
#' @export
#'
human_per <- function(x) {human_numbers(x, smbl = "%")}
#'
#' @rdname human.numbers
#' @export
#'
human_gbp <- function(x) {human_numbers(x, smbl = "£")}
#'
#' @rdname human.numbers
#' @export
#'
human_usd <- function(x) {human_numbers(x, smbl = "$")}
#'
#' @rdname human.numbers
#' @export
#'
human_euro <- function(x) {human_numbers(x, smbl = "€")}
#'
#' @rdname human.numbers
#' @export
#'
human_tl <- function(x) {human_numbers(x, smbl = "TL")}

#==================================== END ======================================
