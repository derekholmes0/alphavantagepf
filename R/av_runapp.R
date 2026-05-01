#' Set the Alpha Vantage API Key
#'
#' @name av_runShiny
#' @description
#' `av_runShiny()` sets Alphavantage API key and entitlement code
#'
#' @returns Nothing
#'
#' @details
#' Starts Shiny APp
#'
#' @examples
#' \dontrun{
#' av_runShiny()
#' }
#'
#' @rdname av_runShiny
#' @export
av_runShiny <- function() {
  #shiny::runApp("./R")
  shinyApp(ui=av_make_ui(), server=av_make_server(), options=list(width=800,height=400,"launch.browser"))
}

# Source - https://stackoverflow.com/a/76327654
# Posted by panman, modified by community. See post 'Timeline' for change history
# Retrieved 2026-04-17, License - CC BY-SA 4.0

#eval(parse(text = 'invisible(rstudioapi::jobRunScript(path = "~/R/",
#           importEnv = TRUE, exportEnv = "R_GlobalEnv"));rstudioapi::executeCommand(commandId = "activateConsole")'))
