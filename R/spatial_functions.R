#============================= Spatial Functions ===============================
#====================== Spatial Functions in Collection ========================

# Notes:
#
## This script contains some functions about spatial analyses which are directly related to estimation stage.
## Some of these functions have specific purposes and should not be used separately.

#================================== northarrow =================================
#' @title Adding North Arrow to Maps
#'
#' @description This function adds a north arrow to a map created with sp::plot.
#'
#' @param loc A two value numeric vector. The longitude and latitude where the north arrow is to be.
#' @param size numeric. Size of the north arrow.
#' @param bearing numeric. Angle of the north arrow.
#' @param cols character. Color of the north arrow.
#' @param cex numeric. Size of the letters.
#' @param ... Other arguments
#'
#' @details
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references Taken from Auxiliary Cartographic Functions in R: North Arrow, Scale Bar, and Label with a Leader Arrow, Tanimura, 2007. For more information see \href{https://www.jstatsoft.org/article/view/v019c01/v19c01.pdf}{here}.
#'
#' @seealso
#'
#' @return A sp::plot object with a north arrow.
#'
#' @examples
#' \dontrun{
#' northarrow(c(-124, 26), size = 1, cex = 1, bearing = 0) ## Do not run.
#' }
#'
#' @export
#'
northarrow <- function(loc, size, bearing = 0, cols, cex = 1, ...) {
    if (!requireNamespace("graphics")) stop("Required graphics package is missing.")
    # Checking arguments.
    if (missing(loc))
        stop("loc is missing")
    if (missing(size))
        stop("size is missing")

    # Default colors are white and black.
    if (missing(cols)) {
        cols <- rep(c("white", "black"), 8)
    }

    # Calculating coordinates of polygons.
    radii <- rep(size/c(1, 4, 2, 4), 4)
    x <- radii[(0:15) + 1] * cos((0:15) * pi/8 + bearing) + loc[1]
    y <- radii[(0:15) + 1] * sin((0:15) * pi/8 + bearing) + loc[2]

    # Drawing polygons.
    for (i in 1:15) {
        x1 <- c(x[i], x[i + 1], loc[1])
        y1 <- c(y[i], y[i + 1], loc[2])
        graphics::polygon(x1, y1, col = cols[i])
    }

    # Drawing the last polygon.
    graphics::polygon(c(x[16], x[1], loc[1]), c(y[16], y[1], loc[2]), col = cols[16])

    # Drawing letters
    b <- c("E", "N", "W", "S")
    for (i in 0:3) {
        graphics::text((size + graphics::par("cxy")[1]) * cos(bearing + i * pi/2) + loc[1], (size + graphics::par("cxy")[2]) * sin(bearing + i * pi/2) + loc[2], b[i + 1], cex = cex)
    }
}

#=================================== scalebar ==================================
#' @title Adding Scale to Projected Maps
#'
#' @description This function adds a scale to a projected map created with sp::plot.
#'
#' @param loc A two value numeric vector. The longitude and latitude where the scale bar is to be.
#' @param length numeric. The length of the divisions in the unit specified.
#' @param unit character. Some options are "m" and "km".
#' @param division.cex numeric. The size of the divisions.
#' @param ... Other arguments
#'
#' @details
#'
#' @note For an unprojected map in R, we can use the \code{\link[maps]{map.scale}} function in maps package. This is applicable to a map created not only by the maps package but also by other cartographic packages. However, in R, there is no scale bar applicable to a projected map. So, use it if your shapefile is projected. It should be noted that using scalebar for a decimal degree or the entire world is not logical because the function cannot convert units and retains them in the unconverted form in the current graphic device. Therefore, if necessary, users must project and coordinate the map appropriately prior to the execution of scalebar.
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references Taken from Auxiliary Cartographic Functions in R: North Arrow, Scale Bar, and Label with a Leader Arrow, Tanimura, 2007. For more information see \href{https://www.jstatsoft.org/article/view/v019c01/v19c01.pdf}{here}.
#'
#' @seealso
#'
#' @return A sp::plot object with a scale.
#'
#' @examples
#' \dontrun{
#' scalebar(c(-124, 26), length = 10000, unit = "km", division.cex = .8) ## Do not run.
#' }
#'
#' @export
#'
scalebar <- function(loc, length, unit = "km", division.cex = .8, ...) {
    if (!requireNamespace("graphics")) stop("Required graphics package is missing.")
    if (missing(loc))
        stop("loc is missing")
    if (missing(length))
        stop("length is missing")
    x <- c(0, length/c(4, 2, 4/3, 1), length * 1.1) + loc[1]
    y <- c(0, length/(10 * 3:1)) + loc[2]
    cols <- rep(c("black", "white"), 2)
    for (i in 1:4) {
        graphics::rect(x[i], y[1], x[i + 1], y[2], col = cols[i])
    }
    for (i in 1:5) {
        graphics::segments(x[i], y[2], x[i], y[3])
    }
    labels <- x[c(1, 3)] - loc[1]
    labels <- append(labels, paste(x[5] - loc[1], unit))
    graphics::text(x[c(1, 3, 5)], y[4], labels = labels, adj = .5, cex = division.cex)
}

#============================== km2ml and ml2km ================================
#' @title Functions for Converting Distances
#'
#' @description This set of functions converts given distances either from km-to-miles or miles-to-km.
#'
#' @param km numeric. A numeric value or vector of km distance.
#' @param ml numeric. A numeric value or vector of mile distance.
#'
#' @details
#' \itemize{
#'   \item km2ml: Converts kilometers to miles.
#'   \item ml2km: Converts miles to kilometers.
#' }
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
#' km2ml(1.6)
#' ml2km(1)
#'
#' @name convert.distance
NULL
#> NULL
#'
#' @rdname convert.distance
#' @export
#'
km2ml <- function(km) {
    out <- km * 0.621374
    return(out)
}
#'
#' @rdname convert.distance
#' @export
#'
ml2km <- function(ml) {
    out <- ml / 0.621374
    return(out)
}

#========================= km2d, ml2d, d2km, and d2ml ==========================
#' @title Functions for Converting Distances to/from Degrees
#'
#' @description This set of functions converts given distances either from km-to-degrees and miles-to-degrees or degrees-to-km and degrees-to-miles.
#'
#' @param km numeric. A single numeric km distance.
#' @param ml numeric. A single numeric mile distance.
#' @param d numeric. A single numeric degrees.
#' @param base.latitude numeric. A single numeric base latitude. Base latitude is generally selected as the mean latitude of your shapefile.
#'
#' @details
#' \itemize{
#'   \item km2d: Converts km to degrees by the selected latitude degree.
#'   \item ml2d: Converts miles to degrees by the selected latitude degree.
#'   \item d2km: Converts degrees to kilometers by the selected latitude degree.
#'   \item d2ml: Converts degrees to miles by the selected latitude degree.
#' }
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Ömer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return Numeric value.
#'
#' @examples
#' km2d(100)
#' ml2d(100)
#' d2km(100)
#' d2ml(100)
#'
#' @name convert.distance.degree
NULL
#> NULL
#'
#' @rdname convert.distance.degree
#' @export
#'
km2d <- function(km, base.latitude = 38.280479) {
    if (!requireNamespace("fields")) stop("Required fields package is missing.")
    one.degree.dist <- fields::rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = FALSE) ## 1 degree longitude distance in kilometers.
    out <- km / one.degree.dist
    return(out)
}
#'
#' @rdname convert.distance.degree
#' @export
#'
ml2d <- function(ml, base.latitude = 38.280479) {
    if (!requireNamespace("fields")) stop("Required fields package is missing.")
    one.degree.dist <- fields::rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = TRUE) ## 1 degree longitude distance in miles.
    out <- ml / one.degree.dist
    return(out)
}
#'
#' @rdname convert.distance.degree
#' @export
#'
d2km <- function(d, base.latitude = 38.280479) {
    if (!requireNamespace("fields")) stop("Required fields package is missing.")
    one.degree.dist <- fields::rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = FALSE) ## 1 degree longitude distance in kilometers.
    out <- d * one.degree.dist
    return(out)
}
#'
#' @rdname convert.distance.degree
#' @export
#'
d2ml <- function(d, base.latitude = 38.280479) {
    if (!requireNamespace("fields")) stop("Required fields package is missing.")
    one.degree.dist <- fields::rdist.earth(matrix(c(0, base.latitude), ncol = 2), matrix(c(1, base.latitude), ncol = 2), miles = TRUE) ## 1 degree longitude distance in miles.
    out <- d * one.degree.dist
    return(out)
}

#============================ deg2rad and rad2deg ==============================
#' @title Functions for Converting Degrees to/from Radians
#'
#' @description This set of functions either converts the given value from degrees-to-radians or radians-to-degrees.
#'
#' @param deg numeric. A numeric value or vector of degrees.
#' @param rad numeric. A numeric value or vector of radians.
#'
#' @details
#' \itemize{
#'   \item deg2rad: Converts degrees to radians.
#'   \item rad2deg: Converts radians to degrees.
#' }
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
#' deg2rad(100)
#' rad2deg(100)
#'
#' @name convert.degree.radian
NULL
#> NULL
#'
#' @rdname convert.degree.radian
#' @export
#'
deg2rad <- function(deg) {
    radian <- (deg * pi) / (180)
    return(radian)
}
#'
#' @rdname convert.degree.radian
#' @export
#'
rad2deg <- function(rad) {
    degree <- (rad * 180) / (pi)
    return(degree)
}

#==================================== END ======================================
