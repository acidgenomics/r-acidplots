#' Pretty breaks that resemble the base plotting engine
#'
#' @export
#'
#' @param n `integer(1)`.
#'   Desired number of breaks.
#'
#' @return `function`.
#'
#' @seealso
#' - [grDevices::axisTicks()].
#' - [scales::log_breaks()].
#' - [scales::trans_breaks()].
#' - Adapted from a Stack Overflow post:
#'   https://stackoverflow.com/questions/14255533
#'
#' @examples
#' acid_pretty_breaks()
acid_pretty_breaks <-  # nolint
    function(n = 5L) {
        function(x) {
            axisTicks(
                usr = log10(range(x, na.rm = TRUE)),
                log = TRUE,
                axp = NULL,
                nint = n
            )
        }
    }
