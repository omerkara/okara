#============================= Personal Functions ==============================
#====================== Personal Functions in Collection =======================

# Notes:
#
## This script contains personal functions which are only suitable to the package author.

#================================ Time.Keeping =================================
#' @title Keeping Time for Hourly Jobs
#'
#' @description This function keeps the punched hours for hourly jobs.
#'
#' @param Hours.Punch character. The path of the time keeping .txt file.
#' @param First.Date character. Default if NULL. The first date of time keeping. If NULL, then it will be generated from the data.
#' @param Last.Date character. Default if NULL. The last date of time keeping. If NULL, then it will be generated from the data.
#' @param Exclude.Weekends logical. If TRUE, the weekends will be excluded from the data.
#' @param Target.Hours numeric. Default is NULL. If a numeric value is given, the remaining hours for the current week is messaged.
#'
#' @details This is a personal function of the package author. So, it won't make sense to others.
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @import stringr tidyr lubridate
#'
#' @references
#'
#' @seealso
#'
#' @return Three data.frames called "Time.Detail", "Total.Days", "Total.Weeks", and a vector "total.hours".
#'
#' @examples
#' \dontrun{
#' Time.Keeping(Hours.Punch = "./hours_punch.txt")
#' Time.Keeping(Hours.Punch = "./hours_punch.txt", Exclude.Weekends = FALSE, Target.Hours = NULL)
#' Time.Keeping(Hours.Punch = "./hours_punch.txt", First.Date = "2016-08-16", Target.Hours = 20)
#' }
#'
#' @export
#'
Time.Keeping <- function(Hours.Punch, First.Date = NULL, Last.Date = NULL, Exclude.Weekends = FALSE, Target.Hours = NULL) {
    if (!requireNamespace("stringr")) stop("Required stringr package is missing.")
    if (!requireNamespace("tidyr")) stop("Required tidyr package is missing.")
    if (!requireNamespace("lubridate")) stop("Required lubridate package is missing.")
    if (!requireNamespace("magrittr")) stop("Required magrittr package is missing.")

    # Saves the current working directory for further use.
    WD.temp <- getwd()

    # Loading the time keeping data which will be Time.Detail data.
    data <- utils::read.delim(Hours.Punch, header = TRUE, sep = "\t", dec = ".", colClasses = c("character"), comment.char = "", na.string = "", encoding = "latin1")

    data <- tidyr::separate(data, "Punch.Time.Stamp", c("Date", "Time"), sep = " ", extra = "merge")
    data <- data.frame(sapply(data, function(trim) stringr::str_trim(trim, side = "both")), stringsAsFactors = FALSE) ## Trimming every cell for possible empty space.
    data$Punch.Time.Stamp <- paste(data$Date, data$Time)
    data$Punch.Time.Stamp <- strptime(data$Punch.Time.Stamp, "%Y-%m-%d %I:%M:%S %p")

    data <- data[order(data$Punch.Time.Stamp, decreasing = FALSE, na.last = FALSE), ]
    rownames(data) <- 1:nrow(data)

    # Total.Days data.
    if (is.null(First.Date)) {
        first.date <- min(as.Date(data$Date))
    } else {
        first.date <- First.Date
    }
    if (is.null(First.Date)) {
        last.date <- max(as.Date(data$Date))
    } else {
        last.date <- Last.Date
    }
    days <- seq(lubridate::date(first.date), lubridate::date(last.date), by = "day")
    if (Exclude.Weekends == TRUE) {
        Total.Days <- data.frame(Date = days[weekdays(days, abbreviate = FALSE) != "Saturday" & weekdays(days, abbreviate = FALSE) != "Sunday"])
    } else {
        Total.Days <- data.frame(Date = days)
    }

    Total.Days$Week <- as.numeric(strftime(Total.Days$Date, origin = "1970-01-01", tz = "UTC", format = "%V")) ## ISO 8601 convention week number. Monday is the first day.
    Total.Days$Date <- as.character(Total.Days$Date)
    Total.Days$Week <- Total.Days$Week - (min(Total.Days$Week) - 1)

    for (i in 1:length(sort(unique(data$Date)))) {
        temp <- data[data$Date == sort(unique(data$Date))[i], ]
        pairs <- (nrow(temp) / 2)
        if (pairs %% 1 == 0) {
            for (j in 1:nrow(temp)) {
                if ((j / 2) %% 1 != 0) {
                    if (j == 1) {
                        second <- as.numeric(temp$Punch.Time.Stamp[j + 1] - temp$Punch.Time.Stamp[j], units = "secs")
                    }
                    if (j != 1) {
                        second.temp <- as.numeric(temp$Punch.Time.Stamp[j + 1] - temp$Punch.Time.Stamp[j], units = "secs")
                        second <- second + second.temp
                    }
                    Total.Days[Total.Days$Date == sort(unique(temp$Date)), "Second"] <- second
                }
            }
        }
        if (pairs %% 1 != 0) {
            temp <- rbind(temp, c("Job Clock Out", "End", as.character(Sys.Date()), as.character(format(Sys.time(), "%I:%M:%S %p")), format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
            for (j in 1:nrow(temp)) {
                if ((j / 2) %% 1 != 0) {
                    if (j == 1) {
                        second <- as.numeric(temp$Punch.Time.Stamp[j + 1] - temp$Punch.Time.Stamp[j], units = "secs")
                    }
                    if (j != 1) {
                        second.temp <- as.numeric(temp$Punch.Time.Stamp[j + 1] - temp$Punch.Time.Stamp[j], units = "secs")
                        second <- second + second.temp
                    }
                    Total.Days[Total.Days$Date == sort(unique(temp$Date)), "Second"] <- second
                }
            }
        }
    }

    Total.Days$Minute <- Total.Days$Second / 60
    Total.Days$Hour <- okara::roundN(Total.Days$Minute / 60, 2)

    ## Total.Weeks data.
    Total.Weeks <- Total.Days
    for (i in 1:length(sort(unique(Total.Weeks$Week)))) {
        Total.Weeks[c(which(Total.Weeks$Week == sort(unique(Total.Weeks$Week))[i])), "Start.End"] <- paste0(min(lubridate::date(x = Total.Weeks[c(which(Total.Weeks$Week == sort(unique(Total.Weeks$Week))[i])), ]$Date), na.rm = TRUE), " | ", max(lubridate::date(x = Total.Weeks[c(which(Total.Weeks$Week == sort(unique(Total.Weeks$Week))[i])), ]$Date), na.rm = TRUE))
    }

    Total.Weeks <- Total.Weeks[, c("Start.End", "Week", "Second", "Minute", "Hour")]
    Total.Weeks <- stats::aggregate(cbind(Second, Minute, Hour) ~ Start.End + Week, data = Total.Weeks, FUN = "sum", na.action = "na.pass", na.rm = TRUE)

    for (j in 1:nrow(Total.Weeks)) {
        for (k in 1:ncol(Total.Weeks)) {
            if (Total.Weeks[j, k] == 0) {
                Total.Weeks[j, k] <- NA
            }
        }
    }

    # If there is a target hour for the current week, then the below code gives you the remaining hours for the current week.
    if (!is.null(Target.Hours)) {
        hours.left <- Target.Hours - Total.Weeks$Hour[max(Total.Weeks$Week[!is.na(Total.Weeks$Hour)])]
        if (hours.left <= 0) {
            message(paste0("Used more than ", dQuote(Target.Hours), " hours in the current week."))
        }
        if (hours.left > 0) {
            minutes.left <- hours.left * 60
            seconds.left <- hours.left * 60 * 60
            target.time <- Sys.time() + seconds.left
            message(minutes.left %/% 60, " hours and ", okara::roundN(minutes.left %% 60, 0), " minutes left in the current week.")
            message(paste0("Hours left: ", okara::roundN(hours.left, 2)))
            message(paste0("Minutes left: ", okara::roundN(minutes.left, 2)))
            message(paste0("Seconds left: ", okara::roundN(seconds.left, 0)))
            message(paste0("Target time: ", format(target.time, "%Y-%m-%d %H:%M:%S")))
        }
    }

    # Creating Hours.Minutes variable for Total.Days and Total.Weeks.
    Total.Days$Hour.Minute <- paste0(okara::roundN(Total.Days$Minute %/% 60, 0), " ", "Hours", " and ", okara::roundN(Total.Days$Minute %% 60, 0), " ", "Minutes")
    Total.Days$Hour.Minute <- gsub("^NA.*", NA, Total.Days$Hour.Minute)
    Total.Weeks$Hour.Minute <- paste0(okara::roundN(Total.Weeks$Minute %/% 60, 0), " ", "Hours", " and ", okara::roundN(Total.Weeks$Minute %% 60, 0), " ", "Minutes")
    Total.Weeks$Hour.Minute <- gsub("^NA.*", NA, Total.Weeks$Hour.Minute)

    # Totals for Total.Days and Total.Weeks.
    Total.Days <- rbind(Total.Days, c("Total", "", okara::roundN(sum(Total.Days$Second, na.rm = TRUE), 2), okara::roundN(sum(Total.Days$Minute, na.rm = TRUE), 2), okara::roundN(sum(Total.Days$Hour, na.rm = TRUE), 2), paste0(okara::roundN(sum(Total.Days$Minute, na.rm = TRUE) %/% 60, 0), " ", "Hours", " and ", okara::roundN(sum(Total.Days$Minute, na.rm = TRUE) %% 60, 0), " ", "Minutes")))
    Total.Weeks <- rbind(Total.Weeks, c("Total", "", okara::roundN(sum(Total.Weeks$Second, na.rm = TRUE), 2), okara::roundN(sum(Total.Weeks$Minute, na.rm = TRUE), 2), okara::roundN(sum(Total.Weeks$Hour, na.rm = TRUE), 2), paste0(okara::roundN(sum(Total.Weeks$Minute, na.rm = TRUE) %/% 60, 0), " ", "Hours", " and ", okara::roundN(sum(Total.Weeks$Minute, na.rm = TRUE) %% 60, 0), " ", "Minutes")))

    # Total hours.
    total.hours <- as.numeric(Total.Days$Hour[nrow(Total.Days)])

    # Total hours and minutes.
    total.hour.minutes <- Total.Days$Hour.Minute[nrow(Total.Days)]

    # Revert the working directory to initial path.
    setwd(WD.temp)

    # The message for your total hours.
    message(paste0("Total: ", total.hours, " hours", " | ", total.hour.minutes))

    # Return total.hours, Time.Detail (details of your time keeping), Total.Days (total time by day), and Total.Weeks (total time by week).
    return(list(Time.Detail = data, Total.Days = Total.Days, Total.Weeks = Total.Weeks, Total.Hours = total.hours))
}

#============================= Min.Sell.Buy.Price ==============================
#' @title Minimum Sell or Buy Prices
#'
#' @description This function gives you the minimum sell price after you buy or the minimum buy price after you sell with a selected fee percentage which makes you break even after the fee.
#'
#' @param Price numeric. Buy or sell price.
#' @param Fee.Per numeric. Fee percentage. Default is 0.1.
#' @param Round numeric. Decimal rounding. Default is 2.
#'
#' @details This is a personal function of the package author. So, it won't make sense to others.
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return Two messages: one for BUY Price, one for SELL Price.
#'
#' @examples
#' Min.Sell.Buy.Price(Price = 60)
#' Min.Sell.Buy.Price(Price = 60, Fee.Per = 0.08)
#' Min.Sell.Buy.Price(Price = 60, Fee.Per = 0.1, Round = 3)
#'
#' @export
#'
Min.Sell.Buy.Price <- function(Price, Fee.Per = 0.1, Round = 2) {
    # Saves the current working directory for further use.
    WD.temp <- getwd()

    # Checks Price argument.
    if (length(Price) != 1)
        stop("Invalid Price. Please choose only one Price.")
    if (!is.numeric(Price))
        stop("Invalid Price. Please choose Price as numeric.")
    if (!(Price > 0))
        stop("Invalid Price. Please choose a positive Price.")

    # Checks Fee.Per argument.
    if (length(Fee.Per) != 1)
        stop("Invalid Fee.Per. Please choose only one Fee.Per.")
    if (!is.numeric(Fee.Per))
        stop("Invalid Fee.Per. Please choose Fee.Per as numeric.")
    if (!(Fee.Per >= 0 & Fee.Per <= 100))
        stop("Invalid Fee.Per. Please choose a Fee.Per between 0 and 100 (including boundaries).")

    # Checks Round argument.
    if (length(Round) != 1)
        stop("Invalid Round. Please choose only one Round.")
    if (!is.numeric(Round))
        stop("Invalid Round. Please choose Round as numeric.")
    if (!(Round >= 0))
        stop("Invalid Round. Please choose a Round equal or larger then 0.")

    # Sell or Buy price.
    price <- Price

    # Fee percentage.
    fee.per <- Fee.Per/100

    # Minimum Sell price after you have bought.
    min.sell.price <- (price * (1 + fee.per)) / (1 - fee.per)

    # Minimum buy price after you have sold.
    min.buy.price <- (price * (1 - fee.per)) / (1 + fee.per)

    # Results
    message(paste0("Min. SELL Price: ", round(min.sell.price, Round)))
    message(paste0("Min. BUY Price: ", round(min.buy.price, Round)))

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#========================== Min.Sell.Buy.Price.Gain ============================
#' @title Minimum Sell or Buy Prices
#'
#' @description This function gives you the minimum sell price after you buy or the minimum buy price after you sell with a selected fee percentage and desired gain percentage.
#'
#' @param Price numeric. Buy or sell price.
#' @param Gain.Per numeric. Gain percentage. Default is 1.
#' @param Fee.Per numeric. Fee percentage. Default is 0.1.
#' @param Round numeric. Decimal rounding. Default is 2.
#'
#' @details This is a personal function of the package author. So, it won't make sense to others.
#'
#' @note
#'
#' @author \href{mailto:omer.kara.ylsy@@gmail.com}{Omer Kara}
#'
#' @references
#'
#' @seealso
#'
#' @return Two messages: one for BUY Price, one for SELL Price.
#'
#' @examples
#' Min.Sell.Buy.Price.Gain(Price = 100)
#' Min.Sell.Buy.Price.Gain(Price = 100, Gain.Per = 1.5)
#' Min.Sell.Buy.Price.Gain(Price = 100, Fee.Per = 0.01)
#' Min.Sell.Buy.Price.Gain(Price = 100, Gain.Per = 2.5, Fee.Per = 0.05, Round = 4)
#'
#' @export
#'
Min.Sell.Buy.Price.Gain <- function(Price, Gain.Per = 1, Fee.Per = 0.1, Round = 2) {
    # Saves the current working directory for further use.
    WD.temp <- getwd()

    # Checks Price argument.
    if (length(Price) != 1)
        stop("Invalid Price. Please choose only one Price.")
    if (!is.numeric(Price))
        stop("Invalid Price. Please choose Price as numeric.")
    if (!(Price > 0))
        stop("Invalid Price. Please choose a positive Price.")

    # Checks Gain.Per argument.
    if (length(Gain.Per) != 1)
        stop("Invalid Gain.Per. Please choose only one Gain.Per.")
    if (!is.numeric(Gain.Per))
        stop("Invalid Gain.Per. Please choose Gain.Per as numeric.")
    if (!(Gain.Per >= 0 & Gain.Per <= 100))
        stop("Invalid Gain.Per. Please choose a Gain.Per between 0 and 100 (including boundaries).")

    # Checks Fee.Per argument.
    if (length(Fee.Per) != 1)
        stop("Invalid Fee.Per. Please choose only one Fee.Per.")
    if (!is.numeric(Fee.Per))
        stop("Invalid Fee.Per. Please choose Fee.Per as numeric.")
    if (!(Fee.Per >= 0 & Fee.Per <= 100))
        stop("Invalid Fee.Per. Please choose a Fee.Per between 0 and 100 (including boundaries).")

    # Checks Round argument.
    if (length(Round) != 1)
        stop("Invalid Round. Please choose only one Round.")
    if (!is.numeric(Round))
        stop("Invalid Round. Please choose Round as numeric.")
    if (!(Round >= 0))
        stop("Invalid Round. Please choose a Round equal or larger then 0.")

    # Sell or Buy price.
    price <- Price

    # Gain percentage.
    gain.per <- Gain.Per/100

    # Fee percentage.
    fee.per <- Fee.Per/100

    # Minimum Sell price after you have bought.
    min.sell.price <- (price * (1 + fee.per)) / (1 - fee.per)

    # Minimum Sell price after you have bought.
    min.sell.price.gain <- min.sell.price * (1 + gain.per)

    # Minimum buy price after you have sold.
    min.buy.price <- (price * (1 - fee.per)) / (1 + fee.per)

    # Minimum buy price after you have sold.
    min.buy.price.gain <- min.buy.price * (1 - gain.per)

    # Results
    message(paste0("Min. SELL Price (Gain: %", Gain.Per, "): ", round(min.sell.price.gain, Round)))
    message(paste0("Min. BUY Price (Gain: %", Gain.Per, "): ", round(min.buy.price.gain, Round)))

    # Revert the working directory to initial path.
    setwd(WD.temp)
}

#==================================== END ======================================
