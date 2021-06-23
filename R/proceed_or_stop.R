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

#==================================== END ======================================
